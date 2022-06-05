(ns swank.commands)

(defonce slime-fn-map {})

(defmacro defslimefn
  ([fname & body]
     `(alter-var-root #'slime-fn-map
                      assoc
                      (symbol "swank" ~(name fname))
		      (defn ~fname ~@body)))
  {:indent 'defun})

(defn slime-fn [sym]
  (slime-fn-map (symbol "swank" (name sym))))