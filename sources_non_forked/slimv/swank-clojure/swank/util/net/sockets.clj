(ns swank.util.net.sockets
  (:use (swank util)
        (swank.util.concurrent thread))
  (:import (java.net ServerSocket Socket SocketException InetAddress)))

(defn make-server-socket
  "Create a java.net.ServerSocket. A map of `options':

    :port - The port which this ServerSocket will listen on. It must
       be a number between 0-65535. If 0 or not provided, the server
       will be created on any free port.

    :host - The address the server will bind to, can be used on multi
      homed hosts. This can be an InetAddress or a hostname string. If
      not provided or nil, it will listen on all addresses.

    :backlog - The maximum queue length of incoming connection
      indications (ie. connection requests). If the queue is full, new
      indications will be refused. If set to less than or equal to 0,
      the default value will be used."
  ([] (ServerSocket.))
  ([options] (ServerSocket. (options :port 0)
                            (options :backlog 0)
                            (when-let [host (options :host)]
                              (if (instance? InetAddress host)
                                host
                                (InetAddress/getByName host))))))

(defn start-server-socket!
  "Given a `server-socket' (java.net.ServerSocket), call
   `handle-socket' for each new connection and provide current
   socket.

   This will return immediately with the Thread that is blocking for
   new connections. Use Thread.join() if you need to wait for the
   server to close."
  ([server-socket handle-socket]
     (dothread-keeping-clj nil
       (thread-set-name (str "Socket Server [" (thread-id) "]"))
       (with-open [#^ServerSocket server server-socket]
         (while (not (.isClosed server))
           (handle-socket (.accept server)))))))

(defn close-socket!
  "Cleanly shutdown and close a java.net.Socket. This will not affect
   an already running instance of SocketServer."
  ([#^Socket socket]
     (doto socket
       (.shutdownInput)
       (.shutdownOutput)
       (.close))))

(defn close-server-socket!
  "Shutdown a java.net.SocketServer. Existing connections will
  persist."
  ([#^ServerSocket server]
     (.close server)))
