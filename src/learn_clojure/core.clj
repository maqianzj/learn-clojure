(ns learn-clojure.core
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clj-http.client :as hc]
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
    :productId :int32
    :appName :utf
    :productName :utf
    :accountId :int32
    :userId :utf
    :accountName :utf
    :userToken :utf
    :amount :int32
    :ratio :int32
    :payType :int32
    :server :utf
    :checkKey :utf
    :buyURL :utf
    :gameid :utf
    :spid :utf

    :head :int32
    :result :int32
    :message :utf

    :systemTime :longTime
    :authType :int32
    :leftTryNum :int32
    :leftVSecs :int32
    :leftVCount :int32
    :authStartTime :longTime
    :authEndTime :longTime

    :sptSub :bool
    :subUnit :utf
    :subRatio :int32
    :sptPoints :bool
    :pointsUnit :utf
    :availPoints :int32
    :pointsRatio :int32
    :sptRecharge :bool
    :expendUnit :utf
    :expendRatio :int32
    :balance :int32
    :rechargeRatio :int32

  })

(def login-req
  [:buyURL :userId :accountName :userToken :appName :checkKey])

(def recharge-req
  [:buyURL :accountId :accountName :userToken :productId :amount
   :ratio :payType :remark :checkKey :spid :password])

(def login-rsp
  [:accountId :userId :productId :productName :appName :systemTime
   :authType :leftTryNum :leftVSecs :leftVCount :authStartTime :authEndTime
   :sptSub :subUnit :subRatio :sptPoints :pointsUnit :availPoints :pointsRatio
   :sptRecharge :expendUnit :expendRatio :balance :rechargeRatio])

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
  (let [at (attrs e) k (keyword (:refid at)) v (for [p (content e)] (:refid (attrs p)))] {k v}))


(defn wrapper-proxy [at v]
  (if (:proxyType at)
    (conj v :proxyInfo
      (struct proxyInfo (:proxyType at) (:proxyHost at) (:proxyPort at) (:proxyUser at) (:proxyPasswd)))
    v))

(defmethod read-element :telcomOperator [e]
  (let [at (attrs e) k (keyword (:id at))
        v {:name (:name at) :sp (apply merge (for [p (content e)] (read-element-sp p)))}]
    {k (wrapper-proxy at v)}))

(defmethod read-element :telcomOperators [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:telcomOperators v}))

(defmethod read-element :configurations [e] (apply merge (for [p (content e)] (read-element p))))

(defn open-is [path] (jio/input-stream (jio/resource path)))

(defn read-props[path] (with-open [is (open-is path)] (doto (Properties.) (.load is))))

(defn props-to-map [props pm]
  (reduce #(merge %1 {(keyword (or (pm %2) %2)) (.getProperty props %2)})
    {} (enumeration-seq (.propertyNames props))))


(defn recharge-price [jadParams] (first (clojure.string/split (:price jadParams) #"/")))

(defn login-req-map [params] params)

(defn recharge-req-map [params]
  (merge params {:ratio 10 :remark "充值测试" :payType 0 :amount (recharge-price params)}))

(defn recharge-with-passwd-req-map [params] (merge params {:password "111"}))

(defmulti write-param #(param-type %3))
(defmethod write-param :utf [dos v _] (doto dos (.writeUTF v)))
(defmethod write-param :int32 [dos v _] (doto dos (.writeInt v)))
(defmethod write-param :int16 [dos v _] (doto dos (.writeShort v)))
(defmethod write-param :bytes [dos v _] (doto dos (.write v)))

(defn write-params [dos pm km] (doseq [k km] (write-param dos (pm k) k)) dos)

(defn req-data[pm km]
  "pm is a hash-map of request data; km is a vector of request param"
  (with-open [baos (ByteArrayOutputStream.) dos (DataOutputStream. baos)]
    (write-params dos pm km) (.toByteArray baos)))

(defmulti read-param #(param-type %2))
(defmethod read-param :utf [dis _] (.readUTF dis))
(defmethod read-param :int32 [dis _] (.readInt dis))
(defmethod read-param :int16 [dis _] (.readShort dis))
(defmethod read-param :longTime [dis _] (Date. (.readLong dis)))

(defn read-params [dis vprot]
  (reduce #(merge %1 {%2 (read-param dis %2)}) {} vprot))

(defn read-result [dis vprot]
  (let [head (.readInt dis) result (.readInt dis) v {:head head :result result}]
    (if (zere? result) (merge v (read-params dis vprot)) (assoc v :message (.readUTF dis)))))

(defn rsp-map [rsp vprot]
  (with-open [bais (ByteArrayInputStream. rsp) dis (DataInputStream. bais)]
    (read-result dis vprot)))

(def conf (read-element (with-open [is (open-is "configurations.xml")] (parse is))))

(defn read-jad-map [tel sp]
  (let [props (read-props (str (name tel) "-" (name sp) ".jad"))]
    (props-to-map props (sp (:jadParams conf)))))

(def telcomjs-winside-jad (read-jad-map :telcomjs :winside))

(defn head [tag cmd] (doto (HeadWrapper$Builder.) (.version Constant/PROTOCOL_VERSION) (.tag tag) (.cmd cmd) (.build)))
;(def head (HeadWrapper.))
(defn login-prot-head [] (head Constant/PROTOCOL_TAG_ACCOUNT Constant/ACCOUNT_CMD_USER_LOGIN))
(defn recharge-prot-head[] (head Constant/PROTOCOL_TAG_SUBSCRIBE Constant/SUBSCRIBE_CMD_RECHARGE))
(defn rechargegd-prop-head[] (head Constant/PROTOCOL_TAG_SUBSCRIBE Constant/SUBSCRIBE_CMD_RECHARGE_WINSIDEGD))

(defn do-test-login [jadmap opts]
  (let [body (req-data (login-req-map jadmap) login-req)]
    (try (rsp-map (http/http-post (:server jadmap)
               {:body body :content-type "application/octet-stream" :proxyInfo (:proxyInfo opts)}) login-rsp)
      (catch Exception e {:result -1 :message (.getMessage e)}))))

(defn do-test-recharge[jadmap opts]
  (let [body (req-data (recharge-req-map jadmap) recharge-req)]
    (try (rsp-map (http/http-post (:server jadmap)
               {:body body :content-type "application/octet-stream" :proxyInfo (:proxyInfo opts)}) recharge-rsp)
      (catch Exception e {:result -1 :message (.getMessage e)}))))

(defn do-test-product [product jadmap opts]
  (let [jadmap (assoc jadmap :productId (productId product) :appName (appName product))]
    (let [result (do-test-login jadmap opts)]
      (if (zero? (:result result))
        (do (println "登录成功")
          (let [result (do-test-recharge (merge jadmap result) opts)]
            (if (zero? (:result result))
              (println "充值成功")
              (println "充值失败，" (:message result))))
        (println "登录失败，" (:message result)))))))

(defn do-test-sp [sp opts]
  (let [jadmap (read-jad-map (:telcomOperator opts) (:serviceProvider opts))]
    (doseq [product (:products opts)] (do-test-product product jadmap opts))))

(defn do-test-telop [telop opts]
  (doseq [[ksp sp] (:sp telop)]
    (do-test-sp sp (merge opts {:proxyInfo (:proxyInfo telop) :serviceProvider ksp}))))

(defn do-test-telops[telops opts]
  (doseq [[ktelop telop] telops]
    (do-test-telop telop {:telcomOperator ktelop})))

