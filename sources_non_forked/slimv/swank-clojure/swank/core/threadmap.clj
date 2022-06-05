(ns swank.core.threadmap
  (:use (swank util)
        (swank.util.concurrent thread)))

(defonce thread-map-next-id (ref 1))
(defonce thread-map (ref {}))

(defn- thread-map-clean []
  (doseq [[id t] @thread-map]
    (when (or (nil? t)
              (not (thread-alive? t)))
      (dosync
       (alter thread-map dissoc id)))))

(defn- get-thread-id [thread]
  (if-let [entry (find-first #(= (val %) thread) @thread-map)]
    (key entry)
    (let [next-id @thread-map-next-id]
      (alter thread-map assoc next-id thread)
      (alter thread-map-next-id inc)
      next-id)))

(defn thread-map-id [thread]
  (returning [id (dosync (get-thread-id thread))]
    (thread-map-clean)))

(defn find-thread [id]
  (@thread-map id))

