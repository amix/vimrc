(ns swank.core.connection
  (:use (swank util)
        (swank.util sys)
        (swank.core protocol))
  (:import (java.net ServerSocket Socket InetAddress)
           (java.io InputStreamReader OutputStreamWriter)))

(def #^{:dynamic true} *current-connection*)
(def default-encoding "iso-8859-1")

(defmacro with-connection [conn & body]
  `(binding [*current-connection* ~conn] ~@body))

(def encoding-map
     {"latin-1" "iso-8859-1"
      "latin-1-unix" "iso-8859-1"
      "iso-latin-1-unix" "iso-8859-1"
      "iso-8859-1" "iso-8859-1"
      "iso-8859-1-unix" "iso-8859-1"

      "utf-8" "utf-8"
      "utf-8-unix" "utf-8"

      "euc-jp" "euc-jp"
      "euc-jp-unix" "euc-jp"

      "us-ascii" "us-ascii" 
      "us-ascii-unix" "us-ascii"})

(defn make-connection ;; rename to make-swank-connection
  "Given a `socket', creates a swank connection. Accepts an optional
   argument `encoding' to define the encoding of the connection. If
   encoding is nil, then the default encoding will be used.

   See also: `default-encoding', `start-server-socket!'"
  ([#^Socket socket] (make-connection socket default-encoding))
  ([#^Socket socket encoding]
     (let [#^String
           encoding (or (encoding-map encoding encoding) default-encoding)]
       {:socket socket
        :reader (InputStreamReader. (.getInputStream socket) encoding)
        :writer (OutputStreamWriter. (.getOutputStream socket) encoding)
        :writer-redir (ref nil)
        
        :indent-cache (ref {})
        :indent-cache-pkg (ref nil)
        
        :control-thread (ref nil)
        :read-thread (ref nil)
        :repl-thread (ref nil)})))

(defn read-from-connection
  "Reads a single message from a swank-connection.

   See also: `write-to-connection', `read-swank-message',
     `make-swank-connection'"
  ([] (read-from-connection *current-connection*))
  ([conn]
     (read-swank-message (conn :reader))))

(defn write-to-connection
  "Writes a single message to a swank-connection.

  See also: `read-from-connection', `write-swank-message',
    `make-swank-connection'"
  ([msg] (write-to-connection *current-connection* msg))
  ([conn msg]
     (write-swank-message (conn :writer) msg)))
