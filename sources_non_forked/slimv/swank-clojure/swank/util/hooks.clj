(ns swank.util.hooks)

(defmacro defhook [name & hooks]
  `(defonce ~name (ref (list ~@hooks))))

;;;; Hooks
(defn add-hook [place function]
  (dosync (alter place conj function)))

(defn run-hook [functions & arguments]
  (doseq [f @functions]
    (apply f arguments)))
