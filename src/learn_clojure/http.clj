(ns learn-clojure.http
  (:use [learn-clojure.io])
  (:import [java.io InputStream OutputStream]
           [java.net URL Proxy Proxy$Type InetSocketAddress Authenticator PasswordAuthentication HttpURLConnection]
  )
)

(defstruct proxyInfo :type :host :port :user :passwd)

(defn wrapper-proxy [opts]
  (let [proxyInfo (:proxyInfo opts)]
    (if (nil? proxyInfo) (assoc opts :proxy Proxy/NO_PROXY)
      (let [host (:host proxyInfo) port (:port proxyInfo) prox (condp = (:type proxyInfo)
                    "http" (->> (InetSocketAddress. host port) (Proxy. java.net.Proxy$Type/HTTP))
                    "socks" (->> (InetSocketAddress. host port) (Proxy. java.net.Proxy$Type/SOCKS))
                    Proxy/NO_PROXY)]
        (when (:user proxyInfo)
          (Authenticator/setDefault
            (proxy [Authenticator][]
              (getPasswordAuthentication []
                (PasswordAuthentication. (:user proxyInfo) (.toCharArray (:passwd proxyInfo)))))))
        (assoc opts :proxy prox)))))

(defn wrapper-request [opts]
  (let [url (URL. (:ref opts)) ^HttpURLConnection conn (.openConnection url (:proxy opts))]
    (when (:method opts) (.setRequestMethod conn (:method opts)))
    (when (:content-type opts) (.setRequestProperty conn "Content-Type" (:content-type opts)))
    (.setRequestProperty conn "Connection" "close")
    (when (:body opts) (.setDoOutput conn true)
      (with-open [os (.getOutputStream conn)]
        (.write os (:body opts))))
    (with-open [is (.getInputStream conn)]
      (read-bytes is))))

(defn http-post
  ([ref] (http-post ref {}))
  ([ref opts] (wrapper-request (wrapper-proxy (merge opts {:method "POST" :ref ref})))))
