(ns learn-clojure.io
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream InputStream]
           [java.net HttpURLConnection InetSocketAddress PasswordAuthentication Proxy URL])
)

(defn read-bytes [^InputStream is]
  (let [bytes (make-array Byte/TYPE 512)]
    (with-open [baos (ByteArrayOutputStream.)]
      (loop [ret (.read is bytes)]
        (if (neg? ret) (.toByteArray baos)
          (do (.write baos bytes 0 ret) (recur (.read is bytes))))))))

