;;; swank_fuzzy.clj --- fuzzy symbol completion, Clojure implementation.

;; Original CL implementation authors (from swank-fuzzy.lisp) below,
;; Authors: Brian Downing <bdowning@lavos.net>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others

;; This progam is based on the swank-fuzzy.lisp.
;; Thanks the CL implementation authors for that useful software.

(ns swank.commands.contrib.swank-fuzzy
  (:use (swank util core commands))
  (:use (swank.util clojure)))

(def #^{:dynamic true} *fuzzy-recursion-soft-limit* 30)
(defn- compute-most-completions [short full]
  (let [collect-chunk (fn [[pcur [[pa va] ys]] [pb vb]]
                        (let [xs (if (= (dec pb) pcur)
                                   [[pa (str va vb)]]
                                   [[pb vb] [pa va]])]
                          [pb (if ys (conj xs ys) xs)]))
        step (fn step [short full pos chunk seed limit?]
               (cond
                 (and (empty? full) (not (empty? short)))
                 nil
                 (or (empty? short) limit?)
                 (if chunk
                   (conj seed
                         (second (reduce collect-chunk
                                         [(ffirst chunk) [(first chunk)]]
                                         (rest chunk))))
                   seed)
                 (= (first short) (first full))
                 (let [seed2
                       (step short (rest full) (inc pos) chunk seed
                             (< *fuzzy-recursion-soft-limit* (count seed)))]
                   (recur (rest short) (rest full) (inc pos)
                          (conj chunk [pos (str (first short))])
                          (if (and seed2 (not (empty? seed2)))
                            seed2
                            seed)
                          false))
                 :else
                 (recur short (rest full) (inc pos) chunk seed false)))]
    (map reverse (step short full 0 [] () false))))

(def fuzzy-completion-symbol-prefixes "*+-%&?<")
(def fuzzy-completion-word-separators "-/.")
(def fuzzy-completion-symbol-suffixes "*+->?!")
(defn- score-completion [completion short full]
  (let [find1
        (fn [c s]
          (re-find (re-pattern (java.util.regex.Pattern/quote (str c))) s))
        at-beginning? zero?
        after-prefix?
        (fn [pos]
          (and (= pos 1)
               (find1 (nth full 0) fuzzy-completion-symbol-prefixes)))
        word-separator?
        (fn [pos]
          (find1 (nth full pos) fuzzy-completion-word-separators))
        after-word-separator?
        (fn [pos]
          (find1 (nth full (dec pos)) fuzzy-completion-word-separators))
        at-end?
        (fn [pos]
          (= pos (dec (count full))))
        before-suffix?
        (fn [pos]
          (and (= pos (- (count full) 2))
               (find1 (nth full (dec (count full)))
                      fuzzy-completion-symbol-suffixes)))]
    (letfn [(score-or-percentage-of-previous
             [base-score pos chunk-pos]
             (if (zero? chunk-pos)
               base-score
               (max base-score
                    (+ (* (score-char (dec pos) (dec chunk-pos)) 0.85)
                       (Math/pow 1.2 chunk-pos)))))
            (score-char
             [pos chunk-pos]
             (score-or-percentage-of-previous
              (cond (at-beginning? pos)         10
                    (after-prefix? pos)         10
                    (word-separator? pos)       1
                    (after-word-separator? pos) 8
                    (at-end? pos)               6
                    (before-suffix? pos)        6
                    :else                       1)
              pos chunk-pos))
            (score-chunk
             [chunk]
             (let [chunk-len (count (second chunk))]
               (apply +
                      (map score-char
                           (take chunk-len (iterate inc (first chunk)))
                           (reverse (take chunk-len
                                          (iterate dec (dec chunk-len))))))))]
      (let [chunk-scores (map score-chunk completion)
            length-score (/ 10.0 (inc (- (count full) (count short))))]
        [(+ (apply + chunk-scores) length-score)
         (list (map list chunk-scores completion) length-score)]))))

(defn- compute-highest-scoring-completion [short full]
  (let [scored-results
        (map (fn [result]
               [(first (score-completion result short full))
                result])
             (compute-most-completions short full))
        winner (first (sort (fn [[av _] [bv _]] (> av bv))
                            scored-results))]
    [(second winner) (first winner)]))

(defn- call-with-timeout [time-limit-in-msec proc]
  "Create a thunk that returns true if given time-limit-in-msec has been
  elapsed and calls proc with the thunk as an argument. Returns a 3 elements
  vec: A proc result, given time-limit-in-msec has been elapsed or not,
  elapsed time in millisecond."
  (let [timed-out (atom false)
        start! (fn []
                 (future (do
                           (Thread/sleep time-limit-in-msec)
                           (swap! timed-out (constantly true)))))
        timed-out? (fn [] @timed-out)
        started-at (System/nanoTime)]
    (start!)
    [(proc timed-out?)
     @timed-out
     (/ (double (- (System/nanoTime) started-at)) 1000000.0)]))

(defmacro with-timeout
  "Create a thunk that returns true if given time-limit-in-msec has been
  elapsed and bind it to timed-out?. Then execute body."
  #^{:private true}
  [[timed-out? time-limit-in-msec] & body]
  `(call-with-timeout ~time-limit-in-msec (fn [~timed-out?] ~@body)))

(defstruct fuzzy-matching
  :var :ns :symbol :ns-name :score :ns-chunks :var-chunks)

(defn- fuzzy-extract-matching-info [matching string]
  (let [[user-ns-name _] (symbol-name-parts string)]
    (cond
      (:var matching)
      [(str (:symbol matching))
       (cond (nil? user-ns-name) nil
             :else (:ns-name matching))]
      :else
      [""
       (str (:symbol matching))])))

(defn- fuzzy-find-matching-vars
  [string ns var-filter external-only?]
  (let [compute (partial compute-highest-scoring-completion string)
        ns-maps (cond
                  external-only? ns-publics
                  (= ns *ns*)    ns-map
                  :else          ns-interns)]
    (map (fn [[match-result score var sym]]
           (if (var? var)
             (struct fuzzy-matching
                     var nil (or (:name (meta var))
                                 (symbol (pr-str var)))
                     nil
                     score nil match-result)
             (struct fuzzy-matching
                     nil nil sym
                     nil
                     score nil match-result)))
         (filter (fn [[match-result & _]]
                   (or (= string "")
                       (not-empty match-result)))
                 (map (fn [[k v]]
                        (if (= string "")
                          (conj [nil 0.0] v k)
                          (conj (compute (.toLowerCase (str k))) v k)))
                      (filter var-filter (seq (ns-maps ns))))))))
(defn- fuzzy-find-matching-nss
  [string]
  (let [compute (partial compute-highest-scoring-completion string)]
    (map (fn [[match-result score ns ns-sym]]
           (struct fuzzy-matching nil ns ns-sym (str ns-sym)
                   score match-result nil))
         (filter (fn [[match-result & _]] (not-empty match-result))
                 (map (fn [[ns-sym ns]]
                        (conj (compute (str ns-sym)) ns ns-sym))
                      (concat
                       (map (fn [ns] [(symbol (str ns)) ns]) (all-ns))
                       (ns-aliases *ns*)))))))

(defn- fuzzy-generate-matchings
  [string default-ns timed-out?]
  (let [take* (partial take-while (fn [_] (not (timed-out?))))
        [parsed-ns-name parsed-symbol-name] (symbol-name-parts string)
        find-vars
        (fn find-vars
          ([designator ns]
             (find-vars designator ns identity))
          ([designator ns var-filter]
             (find-vars designator ns var-filter nil))
          ([designator ns var-filter external-only?]
             (take* (fuzzy-find-matching-vars designator
                                              ns
                                              var-filter
                                              external-only?))))
        find-nss (comp take* fuzzy-find-matching-nss)
        make-duplicate-var-filter
        (fn [fuzzy-ns-matchings]
          (let [nss (set (map :ns-name fuzzy-ns-matchings))]
            (comp not nss str :ns meta second)))
        matching-greater
        (fn [a b]
          (cond
            (> (:score a) (:score b)) -1
            (< (:score a) (:score b)) 1
            :else (compare (:symbol a) (:symbol b))))
        fix-up
        (fn [matchings parent-package-matching]
          (map (fn [m]
                 (assoc m
                   :ns-name (:ns-name parent-package-matching)
                   :ns-chunks (:ns-chunks parent-package-matching)
                   :score (if (= parsed-ns-name "")
                            (/ (:score parent-package-matching) 100)
                            (+ (:score parent-package-matching)
                               (:score m)))))
               matchings))]
    (sort matching-greater
          (cond
            (nil? parsed-ns-name)
            (concat
             (find-vars parsed-symbol-name (maybe-ns default-ns))
             (find-nss parsed-symbol-name))
            ;; (apply concat
            ;;        (let [ns *ns*]
            ;;          (pcalls #(binding [*ns* ns]
            ;;                     (find-vars parsed-symbol-name
            ;;                                (maybe-ns default-ns)))
            ;;                  #(binding [*ns* ns]
            ;;                     (find-nss parsed-symbol-name)))))
            (= "" parsed-ns-name)
            (find-vars parsed-symbol-name (maybe-ns default-ns))
            :else
            (let [found-nss (find-nss parsed-ns-name)
                  find-vars1 (fn [ns-matching]
                               (fix-up
                                (find-vars parsed-symbol-name
                                           (:ns ns-matching)
                                           (make-duplicate-var-filter
                                            (filter (partial = ns-matching)
                                                    found-nss))
                                           true)
                                ns-matching))]
              (concat
               (apply concat
                      (map find-vars1 (sort matching-greater found-nss)))
               found-nss))))))

(defn- fuzzy-format-matching [string matching]
  (let [[symbol package] (fuzzy-extract-matching-info matching string)
        result (str package (when package "/") symbol)]
    [result (.indexOf #^String result #^String symbol)]))

(defn- classify-matching [m]
  (let [make-var-meta (fn [m]
                        (fn [key]
                          (when-let [var (:var m)]
                            (when-let [var-meta (meta var)]
                              (get var-meta key)))))
        vm (make-var-meta m)]
    (set
     (filter
      identity
      [(when-not (or (vm :macro) (vm :arglists))
         :boundp)
       (when (vm :arglists) :fboundp)
       ;; (:typespec)
       ;; (:class)
       (when (vm :macro)    :macro)
       (when (special-symbol? (:symbol m)) :special-operator)
       (when (:ns-name m)   :package)
       (when (= clojure.lang.MultiFn (vm :tag))
         :generic-function)]))))
(defn- classification->string [flags]
  (format (apply str (replicate 8 "%s"))
          (if (or (:boundp flags)
                  (:constant flags)) "b" "-")
          (if (:fboundp flags) "f" "-")
          (if (:generic-function flags) "g" "-")
          (if (:class flags) "c" "-")
          (if (:typespec flags) "t" "-")
          (if (:macro flags) "m" "-")
          (if (:special-operator flags) "s" "-")
          (if (:package flags) "p" "-")))

(defn- fuzzy-convert-matching-for-emacs [string matching]
  (let [[name added-length] (fuzzy-format-matching string matching)]
    [name
     (format "%.2f" (:score matching))
     (concat (:ns-chunks matching)
             (map (fn [[offset string]] [(+ added-length offset) string])
                  (:var-chunks matching)))
     (classification->string (classify-matching matching))
     ]))

(defn- fuzzy-completion-set
  [string default-ns limit time-limit-in-msec]
  (let [[matchings interrupted? _]
        (with-timeout [timed-out? time-limit-in-msec]
          (vec (fuzzy-generate-matchings string default-ns timed-out?)))
        subvec1 (if (and limit
                         (> limit 0)
                         (< limit (count matchings)))
                  (fn [v] (subvec v 0 limit))
                  identity)]
    [(subvec1 (vec (map (partial fuzzy-convert-matching-for-emacs string)
                        matchings)))
     interrupted?]))

(defslimefn fuzzy-completions
  [string default-package-name
   _limit limit _time-limit-in-msec time-limit-in-msec]
  (let [[xs x] (fuzzy-completion-set string default-package-name
                                     limit time-limit-in-msec)]
    (list
     (map (fn [[symbol score chunks class]]
            (list symbol score (map (partial apply list) chunks) class))
          xs)
     (when x 't))))

(defslimefn fuzzy-completion-selected [_ _] nil)

(comment
  (do
    (use '[clojure.test])

    (is (= '(([0 "m"] [9 "v"] [15 "b"]))
           (compute-most-completions "mvb" "multiple-value-bind")))
    (is (= '(([0 "zz"]) ([0 "z"] [2 "z"]) ([1 "zz"]))
           (compute-most-completions "zz" "zzz")))
    (is (= 103
           (binding [*fuzzy-recursion-soft-limit* 2]
             (count
              (compute-most-completions "ZZZZZZ" "ZZZZZZZZZZZZZZZZZZZZZZZ")))))

    (are [x p s] (= x (score-completion [[p s]] s "*multiple-value+"))
         '[10.625 (((10 [0 "*"])) 0.625)] 0  "*"  ;; at-beginning
         '[10.625 (((10 [1 "m"])) 0.625)] 1  "m"  ;; after-prefix
         '[1.625 (((1 [9 "-"])) 0.625)]   9  "-"  ;; word-sep
         '[8.625 (((8 [10 "v"])) 0.625)]  10 "v"  ;; after-word-sep
         '[6.625 (((6 [15 "+"])) 0.625)]  15 "+"  ;; at-end
         '[6.625 (((6 [14 "e"])) 0.625)]  14 "e"  ;; before-suffix
         '[1.625 (((1 [2 "u"])) 0.625)]   2  "u"  ;; other
         )
    (is (= (+ 10 ;; m's score
              (+ (* 10 0.85) (Math/pow 1.2 1))) ;; u's score
           (let [[_ x]
                 (score-completion [[1 "mu"]] "mu" "*multiple-value+")]
             ((comp first ffirst) x)))
        "`m''s score + `u''s score (percentage of previous which is 'm''s)")

    (is (= '[([0 "zz"]) 24.7]
           (compute-highest-scoring-completion "zz" "zzz")))

    (are [to? ret to proc] (= [ret to?]
                              (let [[x y _] (call-with-timeout to proc)]
                                [x y]))
         false "r" 10 (fn [_] "r")
         true  nil 1 (fn [_] (Thread/sleep 10) nil))

    (are [symbol package input] (= [symbol package]
                                   (fuzzy-extract-matching-info
                                    (struct fuzzy-matching
                                            true nil
                                            "symbol" "ns-name"
                                            nil nil nil)
                                    input))
         "symbol" "ns-name" "p/*"
         "symbol" nil "*")
    (is (= ["" "ns-name"]
           (fuzzy-extract-matching-info
            (struct fuzzy-matching
                    nil nil
                    "ns-name" ""
                    nil nil nil)
            "")))

    (defmacro try! #^{:private true}
      [& body]
      `(do
         ~@(map (fn [x] `(try ~x (catch Throwable ~'_ nil)))
                body)))

    (try
     (def testing-testing0 't)
     (def #^{:private true} testing-testing1 't)
     (are [x external-only?] (= x
                                (vec
                                 (sort
                                  (map (comp str :symbol)
                                       (fuzzy-find-matching-vars
                                        "testing" *ns*
                                        (fn [[k v]]
                                          (and (= ((comp :ns meta) v) *ns*)
                                               (re-find #"^testing-"
                                                        (str k))))
                                        external-only?)))))
          ["testing-testing0" "testing-testing1"] nil
          ["testing-testing0"] true)
     (finally
      (try!
        (ns-unmap *ns* 'testing-testing0)
        (ns-unmap *ns* 'testing-testing1))))

    (try
     (create-ns 'testing-testing0)
     (create-ns 'testing-testing1)
     (is (= '["testing-testing0" "testing-testing1"]
            (vec
             (sort
              (map (comp str :symbol)
                   (fuzzy-find-matching-nss "testing-"))))))
     (finally
      (try!
        (remove-ns 'testing-testing0)
        (remove-ns 'testing-testing1))))
    )
  )
