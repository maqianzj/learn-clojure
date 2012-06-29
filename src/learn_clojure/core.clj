(ns learn-clojure.core
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [learn-clojure.http :as http]
  )
  (:use [clojure.xml])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream DataInputStream DataOutputStream InputStream]
           [java.util Properties Date]
           [protocol Constant HeadWrapper IHeadAccessor HeadWrapper$Builder]
  )
)

(def param-type
  {
    :productId ::int32
    :appName ::utf
    :productName ::utf
    :accountId ::int32
    :userId ::utf
    :accountName ::utf
    :userToken ::utf
    :amount ::int32
    :ratio ::int32
    :payType ::int32
    :server ::utf
    :checkKey ::utf
    :buyURL ::utf
    :gameid ::utf
    :stbType ::utf
    :enterURL ::utf
    :spid ::utf
    :remark ::utf
    :password ::utf

    :head ::int32
    :result ::int32
    :message ::utf

    :systemTime ::longTime
    :authType ::int32
    :leftTryNum ::int32
    :leftVSecs ::int32
    :leftVCount ::int32
    :authStartTime ::longTime
    :authEndTime ::longTime

    :sptSub ::bool
    :subUnit ::utf
    :subRatio ::int32
    :sptPoints ::bool
    :pointsUnit ::utf
    :availPoints ::int32
    :pointsRatio ::int32
    :sptRecharge ::bool
    :expendUnit ::utf
    :expendRatio ::int32
    :balance ::int32
    :rechargeRatio ::int32

  })

(def login-req
  [:head :buyURL :userId :accountName :userToken :appName :checkKey])

(def recharge-req
  [:head :buyURL :accountId :accountName :userToken :productId :amount
   :ratio :payType :remark :checkKey :spid :password])

(def rechargegd-req
  [:head :buyURL :accountId :accountName :userToken :productId :amount
   :ratio :payType :remark :checkKey :spid :gameid :enterURL :stbType :password])

(def login-rsp
  [:accountId :userId :productId :productName :appName :systemTime
   :authType :leftTryNum :leftVSecs :leftVCount :authStartTime :authEndTime
   :sptSub :subUnit :subRatio :sptPoints :pointsUnit :availPoints :pointsRatio
   :sptRecharge :expendUnit :expendRatio :balance :rechargeRatio])

(def recharge-rsp [:balance])

(defstruct product :productId :productName :appName)
(def productId (accessor product :productId))
(def productName (accessor product :productName))
(def appName (accessor product :appName))

(defmulti read-element #(tag %))
(defmethod read-element :jadParam [e]
  (let [k (keyword (:id (attrs e)))
        v (apply merge (for [p (content e)] {(:from (attrs p)) (:to (attrs p))}))] {k v}))

(defmethod read-element :jadParams [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:jadParams v}))

(defmethod read-element :product [e]
  (let [at (attrs e) k (keyword (:id at)) v (struct product (:pid at) (:name at) (:id at))] {k v}))

(defmethod read-element :products [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:products v}))

(defmethod read-element :serviceProvider [e]
  (let [at (attrs e) k (keyword (:id at)) v {:name (:name at)}] {k v}))

(defmethod read-element :serviceProviders [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:serviceProviders v}))

(defn read-element-sp [e]
  (let [at (attrs e) k (keyword (:refid at)) v (for [p (content e)] (:refid (attrs p)))] {k {:products v}}))

(defn wrapper-proxy [at v]
  (if (:proxyType at)
    (assoc v :proxyInfo
      (struct http/proxyInfo (:proxyType at) (:proxyHost at)
        (Integer/parseInt (:proxyPort at)) (:proxyUser at) (:proxyPasswd at)))
    v))

(defmethod read-element :telcomOperator [e]
  (let [at (attrs e) k (keyword (:id at))
        v {:name (:name at) :sp (apply merge (for [p (content e)] (read-element-sp p)))}]
    {k (wrapper-proxy at v)}))

(defmethod read-element :telcomOperators [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:telcomOperators v}))

(defmethod read-element :configurations [e] (apply merge (for [p (content e)] (read-element p))))

(defn open-is [path] (jio/input-stream (jio/resource path)))

(def conf (read-element (with-open [is (open-is "configurations.xml")] (parse is))))

(defn read-props[path] (with-open [is (open-is path)] (doto (Properties.) (.load is))))

(defn props-to-map [props pm]
  (reduce #(merge %1 {(keyword (or (pm %2) %2)) (.getProperty props %2)})
    {} (enumeration-seq (.propertyNames props))))

(defn recharge-price [jadParams] (Integer/parseInt (first (clojure.string/split (:price jadParams) #"/"))))

(defn login-req-map [params] params)

(defn recharge-req-map [params]
  (merge params {:ratio 10 :remark "充值测试" :payType 0 :amount (recharge-price params) :password ""}))

(defn passwd-recharge-req-map [params] (assoc (recharge-req-map params) :password "111"))

(defmulti write-param #(param-type %3))
(defmethod write-param ::utf [dos v _] (doto dos (.writeUTF v)))
(defmethod write-param ::int32 [dos v _] (doto dos (.writeInt v)))
(defmethod write-param ::int16 [dos v _] (doto dos (.writeShort v)))
(defmethod write-param ::bytes [dos v _] (doto dos (.write v)))

(defn write-params [dos pm km] (doseq [k km] (println "write" k) (write-param dos (pm k) k)) dos)

(defn req-data[pm km]
  (with-open [baos (ByteArrayOutputStream.) dos (DataOutputStream. baos)]
    (write-params dos pm km) (.toByteArray baos)))

(defmulti read-param #(param-type %2))
(defmethod read-param ::utf [dis _] (.readUTF dis))
(defmethod read-param ::int32 [dis _] (.readInt dis))
(defmethod read-param ::int16 [dis _] (.readShort dis))
(defmethod read-param ::bool [dis _] (.readBoolean dis))
(defmethod read-param ::longTime [dis _] (Date. (.readLong dis)))

(defn read-params [dis vprot]
  (reduce #(merge %1 {%2 (read-param dis %2)}) {} vprot))

(defn read-result [dis vprot]
  (let [head (.readInt dis) result (.readInt dis) v {:head head :result result}]
    (if (zero? result) (merge v (read-params dis vprot)) (assoc v :message (.readUTF dis)))))

(defn rsp-map [rsp vprot]
  (with-open [bais (ByteArrayInputStream. rsp) dis (DataInputStream. bais)]
    (read-result dis vprot)))

(defn read-jad-map [tel sp]
  (let [props (read-props (str (name tel) "-" (name sp) ".jad"))]
    (props-to-map props (sp (:jadParams conf)))))

(defn head [tag cmd] (-> (HeadWrapper$Builder.) (.version Constant/PROTOCOL_VERSION)
                       (.tag tag) (.command cmd) (.build) (.getHead)))
;(def head (HeadWrapper.))
(defn login-head [] (head Constant/PROTOCOL_TAG_ACCOUNT Constant/ACCOUNT_CMD_USER_LOGIN))
(defn recharge-head[] (head Constant/PROTOCOL_TAG_SUBSCRIBE Constant/SUBSCRIBE_CMD_RECHARGE))
(defn rechargegd-head[] (head Constant/PROTOCOL_TAG_SUBSCRIBE Constant/SUBSCRIBE_CMD_RECHARGE_WINSIDEGD))

(defn wrapper-http-opts [httpopts opts]
  (assoc httpopts :content-type "application/octet-stream" :proxyInfo (:proxyInfo opts)))

(defn login-req-body [jadmap opts] (req-data (login-req-map (assoc jadmap :head (login-head))) login-req))

(defn login-rsp-body [jadmap opts] (rsp-map (http/http-post (:server jadmap) opts) login-rsp))

(defn do-test-login [jadmap opts]
  (println "测试登录")
  (let [body (login-req-body jadmap opts) httpopts (wrapper-http-opts {:body body} opts)]
    (try (let [result (login-rsp-body jadmap httpopts)]
           (if (zero? (:result result)) (println "登录成功") (println "登录失败，" (:message result)))
           result)
      (catch Exception e (println "登录失败，" (.getMessage e)) {:result -1 :message (.getMessage e)}))))

(defn recharge-req-body [jadmap opts]
  (if (= :telcomgd (:telcomOperator opts))
    (req-data (recharge-req-map (assoc jadmap :head (rechargegd-head))) rechargegd-req)
    (req-data (recharge-req-map (assoc jadmap :head (recharge-head))) recharge-req)))

(defn passwd-recharge-req-body [jadmap opts]
  (if (= :telcomgd (:telcomOperator opts))
    (req-data (passwd-recharge-req-map (assoc jadmap :head (rechargegd-head))) rechargegd-req)
    (req-data (passwd-recharge-req-map (assoc jadmap :head (recharge-head))) recharge-req)))

(defn recharge-rsq-body [jadmap opts] (rsp-map (http/http-post (:server jadmap) opts) recharge-rsp))

(defn passwd-error [msg] (and (.contains msg "密码") (or (.contains msg "错误") (.contains msg "校验失败"))))

(defn do-test-passwd-recharge [jadmap opts]
  (println "测试密码充值")
  (let [body (recharge-req-body jadmap opts) httpopts (wrapper-http-opts {:body body} opts)]
    (try (let [result (recharge-rsq-body jadmap httpopts)]
           (if (zero? (:result result)) (println "充值成功") (println "充值失败，" (:message result)))
           result)
      (catch Exception e (println "充值失败，" (.getMessage e)) {:result -1 :message (.getMessage e)}))))

(defn do-test-recharge[jadmap opts]
  (println "测试充值")
  (let [body (recharge-req-body jadmap opts) httpopts (wrapper-http-opts {:body body} opts)]
    (try (let [result (recharge-rsq-body jadmap httpopts)]
           (if (zero? (:result result))
             (do (println "充值成功") result)
             (do (println "充值失败，" (:message result))
               (if (passwd-error (:message result))
                 (do-test-passwd-recharge jadmap opts)
                 result))))
      (catch Exception e (println "充值失败，" (.getMessage e)) {:result -1 :message (.getMessage e)}))))

(defn do-test-product [appName jadmap opts]
  (println "测试" appName)
  (let [jadmap (assoc jadmap :appName appName)]
    (let [result (do-test-login jadmap opts)]
      (when (zero? (:result result))
        (do-test-recharge (merge jadmap result) opts)))))

(defn do-test-sp [sp opts]
  (let [jadmap (read-jad-map (:telcomOperator opts) (:serviceProvider opts))]
    (println "测试" (:serviceProvider opts))
    (doseq [appName (:products sp)] (do-test-product appName jadmap opts))))

(defn do-test-telop [telop opts]
  (doseq [[ksp sp] (:sp telop)]
    (println "测试" (:telcomOperator opts))
    (do-test-sp sp (merge opts {:proxyInfo (:proxyInfo telop) :serviceProvider ksp}))))

(defn do-test-telops[telops opts]
  (doseq [[ktelop telop] telops]
    (do-test-telop telop (assoc opts :telcomOperator ktelop))
    (when (:wait opts) (println "等候" (quot (:wait opts) 1000) "秒") (Thread/sleep (:wait opts)))))

;(println (get-in conf [:telcomOperators :telcomfj :sp]))
;(do-test-telops (:telcomOperators conf) {:wait 5000})
(do-test-telop (:telcomgd (:telcomOperators conf)) {:telcomOperator :telcomgd})
;(do-test-telop (:telcomjs (:telcomOperators conf)) {:telcomOperator :telcomjs})
;(http/http-post "")

