(ns swank.clj-contrib.pprint)

(def #^{:private true} pprint-enabled?
  (try ;; 1.2+
    (.getResource (clojure.lang.RT/baseLoader) "clojure/pprint")
    (require '[clojure.pprint :as pp])
    (defmacro #^{:private true} pretty-pr-code*
      ([code]
         (if pprint-enabled?
           `(binding [pp/*print-suppress-namespaces* true]
              (pp/with-pprint-dispatch pp/code-dispatch
                (pp/write ~code :pretty true :stream nil)))
           `(pr-str ~code))))
    true
    (catch Exception e
      (try ;; 1.0, 1.1
        (.loadClass (clojure.lang.RT/baseLoader)
                    "clojure.contrib.pprint.PrettyWriter")
        (require '[clojure.contrib.pprint :as pp])
        (defmacro #^{:private true} pretty-pr-code*
          ([code]
             (if pprint-enabled?
               `(binding [pp/*print-suppress-namespaces* true]
                  (pp/with-pprint-dispatch pp/*code-dispatch*
                    (pp/write ~code :pretty true :stream nil)))
               `(pr-str ~code))))
        true
        ;; if you just don't have contrib, be silent.
        (catch ClassNotFoundException _)
        (catch Exception e
          (println e))))))

(defn pretty-pr-code [code]
  (pretty-pr-code* code))
