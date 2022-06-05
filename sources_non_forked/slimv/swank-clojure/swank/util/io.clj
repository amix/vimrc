(ns swank.util.io
  (:use [swank util]
        [swank.util.concurrent thread])
  (:import [java.io StringWriter Reader PrintWriter]))

(defn read-chars
  ([rdr n] (read-chars rdr n false))
  ([#^Reader rdr n throw-exception]
     (let [cbuf (make-array Character/TYPE n)]
       (loop [i 0]
	 (let [size (.read rdr cbuf i (- n i))]
	   (cond
	    (neg? size) (if throw-exception
			  (throw throw-exception)
			  (String. cbuf 0 i))
	    (= (+ i size) n) (String. cbuf)
	    :else (recur (+ i size))))))))

(defn call-on-flush-stream
  "Creates a stream that will call a given function when flushed."
  ([flushf]
     (let [closed? (atom false)
           #^PrintWriter stream
           (PrintWriter.
            (proxy [StringWriter] []
              (close [] (reset! closed? true))
              (flush []
                     (let [#^StringWriter me this
                           len (.. me getBuffer length)]
                       (when (> len 0)
                         (flushf (.. me getBuffer (substring 0 len)))
                         (.. me getBuffer (delete 0 len)))))))]
       (dothread
        (thread-set-name "Call-on-write Stream")
        (continuously
         (Thread/sleep 200)
         (when-not @closed?
           (.flush stream))))
       stream))
  {:tag PrintWriter})
