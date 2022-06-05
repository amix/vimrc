(ns swank.util
  (:import (java.io StringReader)
           (clojure.lang LineNumberingPushbackReader)))

(defmacro one-of?
  "Short circuiting value comparison."
  ([val & possible]
     (let [v (gensym)]
       `(let [~v ~val]
          (or ~@(map (fn [p] `(= ~v ~p)) possible))))))

(defn find-first
  "Returns the first entry in a coll matches a given predicate."
  ([coll] (find-first identity coll))
  ([pred coll]
     (first (filter pred coll))))

(defn position
  "Finds the first position of an item that matches a given predicate
   within col. Returns nil if not found. Optionally provide a start
   offset to search from."
  ([pred coll] (position pred coll 0))
  ([pred coll start]
     (loop [coll (drop start coll), i start]
       (when (seq coll)
         (if (pred (first coll))
           i
           (recur (rest coll) (inc i))))))
  {:tag Integer})

(when-not (ns-resolve 'clojure.core 'group-by)
  ;; TODO: not sure why eval is necessary here; breaks without it.
  (eval '(defn group-by
           "Categorizes elements within a coll into a map based on a function."
           ([f coll]
              (reduce
               (fn [ret x]
                 (let [k (f x)]
                   (assoc ret k (conj (get ret k []) x))))
               {})))))

(when-not (ns-resolve 'clojure.core 'flatten)
  (eval '(defn flatten [x]
           (filter (complement sequential?)
                   (rest (tree-seq sequential? seq x))))))

(defmacro returning [[var ret] & body]
  `(let [~var ~ret]
     ~@body
     ~var))


(defn deep-replace [smap coll]
  (map #(if (or (seq? %) (vector? %))
          (deep-replace smap %)
          %)
       (replace smap coll)))

(defmacro keep-bindings [bindings f]
  (let [bind-vars (take (count bindings) (repeatedly gensym))]
    `(let [~@(interleave bind-vars bindings)]
       (fn [& args#]
         (binding [~@(interleave bindings bind-vars)]
           (apply ~f args#))))))

(defmacro continuously [& body]
  `(loop [] ~@body (recur)))

(defmacro failing-gracefully [& body]
  `(try
    ~@body
    (catch Throwable _# nil)))
