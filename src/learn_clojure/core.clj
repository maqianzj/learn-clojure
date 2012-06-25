(ns learn-clojure.core
  (:require [clojure.java.io :as jio])
  (:use [clojure.xml])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream DataInputStream DataOutputStream]
           [java.util Properties]
  )
)

(def req-param-type
  {
    :price :int32
    :productId :int32
    :appName :string
    :accountId :int32
    :userId :string
    :accountName :string
    :userToken :string
    :amount :int32
    :ratio :int32
    :payType :int32
    :server :string
    :checkKey :string
    :buyURL :string
    :gameid :string
    :spid :stringh
  })

(def user-login-req
  [:buyURL :userId :accountName :userToken :appName :checkKey])

(def recharge-req
  [:buyURL :accountId :accountName :userToken :productId :amount
   :ratio :payType :remark :checkKey :spid :password])

(defstruct product :productId :productName :appName)
(def productId (accessor product :productId))
(def productName (accessor product :productName))
(def appName (accessor product :appName))

(defmulti read-element #(tag %))

(defmethod read-element :jadParam [e]
  (let [k (keyword (:id (attrs e))) v (apply merge (for [p (content e)] {(:from (attrs p)) (:to (attrs p))}))] {k v}))

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

(defmethod read-element :telcomOperator [e]
  (let [at (attrs e) k (keyword (:id at))]
    {k  {:name (:name at) :sp (apply merge (for [p (content e)] (read-element-sp p)))}}))

(defmethod read-element :telcomOperators [e]
  (let [v (apply merge (for [p (content e)] (read-element p)))] {:telcomOperators v}))

(defmulti write-param #(req-param-type %3))
(defmethod write-param :string [dos v _] (doto dos (.writeUTF v)))
(defmethod write-param :int32 [dos v _] (doto dos (.writeInt v)))
(defmethod write-param :int16 [dos v _] (doto dos (.writeShort v)))
(defmethod write-param :bytes [dos v _] (doto dos (.write v)))

(defn write-params [dos pm km] ((doseq [k km] (write-param dos (pm k) k)) dos))

(defn req-data[pm km]
  (with-open [baos (ByteArrayOutputStream.) dos (DataOutputStream. baos)]
    (write-params dos pm km) (.toByteArray baos)))

(defn read-bytes[bytes]
  (with-open [bais (ByteArrayInputStream. bytes) dis (DataInputStream. bais)]
    [(.readInt dis) (.readShort dis) (.readUTF dis)]))

(defn read-props[path]
  (with-open [is (jio/input-stream (jio/resource path))]
    (doto (Properties.) (.load is))))

(defn open-is [path] (jio/input-stream (jio/resource path)))

(def conf (with-open [is (open-is "configurations.xml")] (parse is)))
(doseq [c (content conf)] (println (read-element c)))