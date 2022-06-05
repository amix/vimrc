(ns swank.commands.contrib.swank-arglists
  (:use (swank util core commands)))

((slime-fn 'swank-require) :swank-c-p-c)

;;; pos starts at 1 bc 0 is function name
(defn position-in-arglist? [arglist pos]
  (or (some #(= '& %) arglist)
      (<= pos (count arglist))))

;; (position-in-arglist? '[x y] 2)
;; => true

(defn highlight-position [arglist pos]
  (if (zero? pos)
    arglist
    ;; i.e. not rest args
    (let [num-normal-args (count (take-while #(not= % '&) arglist))]
      (if (<= pos num-normal-args)
        (into [] (concat (take (dec pos) arglist)
                         '(===>)
                         (list (nth arglist (dec pos)))
                         '(<===)
                         (drop pos arglist)))
        (let [rest-arg? (some #(= % '&) arglist)]
          (if rest-arg?
            (into [] (concat (take-while #(not= % '&) arglist)
                             '(===>)
                             '(&)
                             (list (last arglist))
                             '(<===)))))))))

;; (highlight-position '[x y] 0)
;; => [===> x <=== y]

(defn highlight-arglists [arglists pos]
  (let [arglists (read-string arglists)]
    (loop [checked []
           current (first arglists)
           remaining (rest arglists)]
      (if (position-in-arglist? current pos)
        (apply list (concat checked
                            [(highlight-position current pos)]
                            remaining))
        (when (seq remaining)
          (recur (conj checked current)
                 (first remaining)
                 (rest remaining)))))))

;; (highlight-arglists "([x] [x & more])" 1)
;; => ([===> x <===] [x & more])

;;(defmacro dbg[x] `(let [x# ~x] (println '~x "->" x#) x#))

(defn defnk-arglists? [arglists]
  (and (not (nil? arglists ))
       (not (vector? (first (read-string arglists))))))

(defn fix-defnk-arglists [arglists]
  (str (list (into [] (read-string arglists)))))

(defn arglists-for-fname-lookup [fname]
  ((slime-fn 'operator-arglist) fname *current-package*))

(defn arglists-for-fname [fname]
  (let [arglists (arglists-for-fname-lookup fname)]
    ;; defnk's arglists format is (a b) instead of ([a b])
    (if (defnk-arglists? arglists)
      (fix-defnk-arglists arglists)
      arglists)))

(defn message-format [cmd arglists pos]
  (str (when cmd (str cmd ": "))
       (when arglists
         (if pos
           (highlight-arglists arglists pos)
           arglists))))

(defn handle-apply [raw-specs pos]
  (let [fname (second (first raw-specs))]
    (message-format fname (arglists-for-fname fname) (dec pos))))

(defslimefn arglist-for-echo-area [raw-specs & options]
  (let [{:keys [arg-indices
                print-right-margin
                print-lines]} (apply hash-map options)]
    (if-not (and raw-specs
                 (seq? raw-specs)
                 (seq? (first raw-specs)))
      nil ;; problem?
      (let [pos (first (second options))
            top-level? (= 1 (count raw-specs))
            parent-pos (when-not top-level?
                         (second (second options)))
            fname (ffirst raw-specs)
            parent-fname (when-not top-level?
                           (first (second raw-specs)))
            arglists (arglists-for-fname fname)
            inside-binding? (and (not top-level?)
                                 (#{"let" "binding" "doseq" "for" "loop"}
                                  parent-fname)
                                 (= 1 parent-pos))]
;;         (dbg raw-specs)
;;         (dbg options)
        (cond
         ;; display arglists for function being applied unless on top of apply
         (and (= fname "apply") (not= pos 0)) (handle-apply raw-specs pos)              
         ;; highlight binding inside binding forms unless >1 level deep
         inside-binding? (message-format parent-fname
                                         (arglists-for-fname parent-fname)
                                         1)
         :else  (message-format fname arglists pos))))))

(defslimefn variable-desc-for-echo-area [variable-name]
  (with-emacs-package
   (or 
    (try
     (when-let [sym (read-string variable-name)]
       (when-let [var (resolve sym)]
         (when (.isBound #^clojure.lang.Var var)
           (str variable-name " => " (var-get var)))))
     (catch Exception e nil))
    "")))
