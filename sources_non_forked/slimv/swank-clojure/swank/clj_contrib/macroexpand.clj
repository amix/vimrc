(ns swank.clj-contrib.macroexpand)

(def
 #^{:private true}
 walk-enabled?
 (.getResource (clojure.lang.RT/baseLoader) "clojure/contrib/macro_utils.clj"))

(when walk-enabled?
  (require 'clojure.contrib.macro-utils))

(defmacro macroexpand-all* [form]
  (if walk-enabled?
    `(clojure.contrib.macro-utils/mexpand-all ~form)
    `(macroexpand ~form)))

(defn macroexpand-all [form]
  (macroexpand-all* form))