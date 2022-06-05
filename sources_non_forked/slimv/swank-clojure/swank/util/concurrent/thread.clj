(ns swank.util.concurrent.thread
  (:use (swank util)))

(defn- gen-name []
  (name (gensym "Thread-")))

(defn start-thread
  "Starts a thread that run the given function f"
  ([#^Runnable f]
     (doto (Thread. f)
       (.start))))

(defmacro dothread [& body]
  `(start-thread (fn [] ~@body)))

(defmacro dothread-keeping [bindings & body]
  `(start-thread (keep-bindings ~bindings (fn [] ~@body))))

(defmacro dothread-keeping-clj [more-bindings & body]
  (let [clj-star-syms (filter #(or (= (name %) "*e")
                                   (= (name %) "*1")
                                   (= (name %) "*2")
                                   (= (name %) "*3")
                                   (and (.startsWith #^String (name %) "*")
                                        (.endsWith #^String (name %) "*")
                                        (> (count (name %)) 1)))
                              (keys (ns-publics (find-ns 'clojure.core))))]
    `(dothread-keeping [~@clj-star-syms ~@more-bindings]
       ~@body)))

(defn current-thread []
  (Thread/currentThread))

(defn thread-set-name
  ([name] (thread-set-name (current-thread) name))
  ([#^Thread thread name]
     (.setName thread name)))

(defn thread-name
  ([] (thread-name (current-thread)))
  ([#^Thread thread]
     (.getName thread)))

(defn thread-id
  ([] (thread-id (current-thread)))
  ([#^Thread thread]
     (.getId thread)))

(defn thread-alive? [#^Thread t]
  (.isAlive t))
