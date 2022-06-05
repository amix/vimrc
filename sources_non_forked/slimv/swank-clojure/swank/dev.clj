(ns swank.dev
  (:use (swank util)))

(defmacro with-swank-io [& body]
  `(binding [*out* @(:writer-redir (first @swank.core.server/connections))]
     ~@body))
