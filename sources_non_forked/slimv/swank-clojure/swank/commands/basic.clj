(ns swank.commands.basic
  (:refer-clojure :exclude [load-file print-doc])
  (:use (swank util commands core)
        (swank.util.concurrent thread)
        (swank.util string clojure)
        (swank.clj-contrib pprint macroexpand))
  (:require (swank.util [sys :as sys])
            (swank.commands [xref :as xref]))
  (:import (java.io StringReader File)
           (java.util.zip ZipFile)
           (clojure.lang LineNumberingPushbackReader)))

;;;; Connection

(defslimefn connection-info []
  `(:pid ~(sys/get-pid)
         :style :spawn
         :lisp-implementation (:type "Clojure"
                                     :name "clojure"
                                     :version ~(clojure-version))
         :package (:name ~(name (ns-name *ns*))
                         :prompt ~(name (ns-name *ns*)))
         :version ~(deref protocol-version)))

(defslimefn quit-lisp []
  (System/exit 0))

(defslimefn toggle-debug-on-swank-error []
  (alter-var-root #'swank.core/debug-swank-clojure not))

;;;; Evaluation

(defn- eval-region
  "Evaluate string, return the results of the last form as a list and
   a secondary value the last form."
  ([string]
     (eval-region string "NO_SOURCE_FILE" 1))
  ([string file line]
     (with-open [rdr (proxy [LineNumberingPushbackReader]
                         ((StringReader. string))
                       (getLineNumber [] line))]
       (binding [*file* file]
         (loop [form (read rdr false rdr), value nil, last-form nil]
           (if (= form rdr)
             [value last-form]
             (recur (read rdr false rdr)
                    (eval (with-env-locals form))
                    form)))))))

(defn- compile-region
  "Compile region."
  ([string file line]
     (with-open [rdr1 (proxy [LineNumberingPushbackReader]
                          ((StringReader. string)))
                 rdr (proxy [LineNumberingPushbackReader] (rdr1)
                       (getLineNumber [] (+ line (.getLineNumber rdr1) -1)))]
       (clojure.lang.Compiler/load rdr file (.getName (File. file))))))


(defslimefn interactive-eval-region [string]
  (with-emacs-package
    (pr-str (first (eval-region string)))))

(defslimefn interactive-eval [string]
  (with-emacs-package
    (pr-str (first (eval-region string)))))

(defslimefn listener-eval [form]
  (with-emacs-package
    (with-package-tracking
      (let [[value last-form] (eval-region form)]
        (when (and last-form (not (one-of? last-form '*1 '*2 '*3 '*e)))
          (set! *3 *2)
          (set! *2 *1)
          (set! *1 value))
        (send-repl-results-to-emacs value)))))

(defslimefn eval-and-grab-output [string]
  (with-emacs-package
    (let [retval (promise)]
      (list (with-out-str
              (deliver retval (pr-str (first (eval-region string)))))
            @retval))))

(defslimefn pprint-eval [string]
  (with-emacs-package
    (pretty-pr-code (first (eval-region string)))))

;;;; Macro expansion

(defn- apply-macro-expander [expander string]
  (pretty-pr-code (expander (read-string string))))

(defslimefn swank-macroexpand-1 [string]
  (apply-macro-expander macroexpand-1 string))

(defslimefn swank-macroexpand [string]
  (apply-macro-expander macroexpand string))

;; not implemented yet, needs walker
(defslimefn swank-macroexpand-all [string]
  (apply-macro-expander macroexpand-all string))

;;;; Compiler / Execution

(def compiler-exception-location-re #"Exception:.*\(([^:]+):([0-9]+)(:[0-9]+)?\)")
(defn- guess-compiler-exception-location [#^Throwable t]
  (when (instance? clojure.lang.Compiler$CompilerException t)
    (let [[match file line] (re-find compiler-exception-location-re (str t))]
      (when (and file line)
        `(:location (:file ~file) (:line ~(Integer/parseInt line)) nil)))))

;; TODO: Make more and better guesses
(defn- exception-location [#^Throwable t]
  (or (guess-compiler-exception-location t)
      '(:error "No error location available")))

;; plist of message, severity, location, references, short-message
(defn- exception-to-message [#^Throwable t]
  `(:message ~(.toString t)
             :severity :error
             :location ~(exception-location t)
             :references nil
             :short-message ~(.toString t)))

(defn- compile-file-for-emacs*
  "Compiles a file for emacs. Because clojure doesn't compile, this is
   simple an alias for load file w/ timing and messages. This function
   is to reply with the following:
     (:swank-compilation-unit notes results durations)"
  ([file-name]
     (let [start (System/nanoTime)]
       (try
         (let [ret (clojure.core/load-file file-name)
               delta (- (System/nanoTime) start)]
           `(:compilation-result nil ~(pr-str ret) ~(/ delta 1000000000.0)))
         (catch Throwable t
           (let [delta (- (System/nanoTime) start)
                 causes (exception-causes t)
                 num (count causes)]
             (.printStackTrace t) ;; prints to *inferior-lisp*
             `(:compilation-result
               ~(map exception-to-message causes) ;; notes
               nil ;; results
               ~(/ delta 1000000000.0) ;; durations
               )))))))

(defslimefn compile-file-for-emacs
  ([file-name load? & compile-options]
     (when load?
       (compile-file-for-emacs* file-name))))

(defslimefn load-file [file-name]
  (let [libs-ref @(resolve 'clojure.core/*loaded-libs*)
        libs @libs-ref]
    (try
      (dosync (ref-set libs-ref #{}))
      (pr-str (clojure.core/load-file file-name))
         (finally
          (dosync (alter libs-ref into libs))))))

(defn- line-at-position [file position]
  (try
    (with-open [f (java.io.LineNumberReader. (java.io.FileReader. file))]
      (.skip f position)
      (.getLineNumber f))
    (catch Exception e 1)))

(defmacro compiler-exception [directory line ex]
  `(eval (if (>= (:minor *clojure-version*) 5)
           '(clojure.lang.Compiler$CompilerException.
             ~directory ~line 0 ~ex)
           '(clojure.lang.Compiler$CompilerException.
             ~directory ~line ~ex))))

(defslimefn compile-string-for-emacs [string buffer position directory debug]
  (let [start (System/nanoTime)
        line (line-at-position directory position)
        ret (with-emacs-package
              (when-not (= (name (ns-name *ns*)) *current-package*)
                (throw (compiler-exception
                        directory line
                        (Exception. (str "No such namespace: "
                                         *current-package*)))))
              (compile-region string directory line))
        delta (- (System/nanoTime) start)]
    `(:compilation-result nil ~(pr-str ret) ~(/ delta 1000000000.0))))

;;;; Describe

(defn- maybe-resolve-sym [symbol-name]
  (try
    (ns-resolve (maybe-ns *current-package*) (symbol symbol-name))
    (catch ClassNotFoundException e nil)))

(defn- maybe-resolve-ns [sym-name]
  (let [sym (symbol sym-name)]
    (or ((ns-aliases (maybe-ns *current-package*)) sym)
        (find-ns sym))))

(defn- print-doc* [m]
  (println "-------------------------")
  (println (str (when-let [ns (:ns m)] (str (ns-name ns) "/")) (:name m)))
  (cond
   (:forms m) (doseq [f (:forms m)]
                (print "  ")
                (prn f))
   (:arglists m) (prn (:arglists m)))
  (if (:special-form m)
    (do
      (println "Special Form")
      (println " " (:doc m))
      (if (contains? m :url)
        (when (:url m)
          (println (str "\n  Please see http://clojure.org/" (:url m))))
        (println (str "\n  Please see http://clojure.org/special_forms#"
                      (:name m)))))
    (do
      (when (:macro m)
        (println "Macro"))
      (println " " (:doc m)))))

(def print-doc (let [print-doc (resolve 'clojure.core/print-doc)]
                 (if (or (nil? print-doc) (-> print-doc meta :private))
                   (comp print-doc* meta)
                   print-doc)))

(defn- describe-to-string [var]
  (with-out-str
    (print-doc var)))

(defn- describe-symbol* [symbol-name]
  (with-emacs-package
    (if-let [v (maybe-resolve-sym symbol-name)]
      (if-not (class? v)
        (describe-to-string v)))))

(defslimefn describe-symbol [symbol-name]
  (describe-symbol* symbol-name))

(defslimefn describe-function [symbol-name]
  (describe-symbol* symbol-name))

;; Only one namespace... so no kinds
(defslimefn describe-definition-for-emacs [name kind]
  (describe-symbol* name))

;; Only one namespace... so only describe symbol
(defslimefn documentation-symbol
  ([symbol-name default] (documentation-symbol symbol-name))
  ([symbol-name] (describe-symbol* symbol-name)))

;;;; Documentation

(defn- briefly-describe-symbol-for-emacs [var]
  (let [lines (fn [s] (.split #^String s (System/getProperty "line.separator")))
        [_ symbol-name arglists d1 d2 & __] (lines (describe-to-string var))
        macro? (= d1 "Macro")]
    (list :designator symbol-name
          (cond
           macro? :macro
           (:arglists (meta var)) :function
           :else :variable)
          (apply str (concat arglists (if macro? d2 d1))))))

(defn- make-apropos-matcher [pattern case-sensitive?]
  (let [pattern (java.util.regex.Pattern/quote pattern)
        pat (re-pattern (if case-sensitive?
                          pattern
                          (format "(?i:%s)" pattern)))]
    (fn [var] (re-find pat (pr-str var)))))

(defn- apropos-symbols [string external-only? case-sensitive? package]
  (let [packages (or (when package [package]) (all-ns))
        matcher (make-apropos-matcher string case-sensitive?)
        lister (if external-only? ns-publics ns-interns)]
    (filter matcher
            (apply concat (map (comp (partial map second) lister)
                               packages)))))

(defn- present-symbol-before
  "Comparator such that x belongs before y in a printed summary of symbols.
Sorted alphabetically by namespace name and then symbol name, except
that symbols accessible in the current namespace go first."
  [x y]
  (let [accessible?
        (fn [var] (= (maybe-resolve-sym (:name (meta var)))
                     var))
        ax (accessible? x) ay (accessible? y)]
    (cond
     (and ax ay) (compare (:name (meta x)) (:name (meta y)))
     ax -1
     ay 1
     :else (let [nx (str (:ns (meta x))) ny (str (:ns (meta y)))]
             (if (= nx ny)
               (compare (:name (meta x)) (:name (meta y)))
               (compare nx ny))))))

(defslimefn apropos-list-for-emacs
  ([name]
     (apropos-list-for-emacs name nil))
  ([name external-only?]
     (apropos-list-for-emacs name external-only? nil))
  ([name external-only? case-sensitive?]
     (apropos-list-for-emacs name external-only? case-sensitive? nil))
  ([name external-only? case-sensitive? package]
     (let [package (when package
                     (maybe-ns package))]
       (map briefly-describe-symbol-for-emacs
            (sort present-symbol-before
                  (apropos-symbols name external-only? case-sensitive?
                                   package))))))

;;;; Operator messages
(defslimefn operator-arglist [name package]
  (try
    (let [f (read-string name)]
      (cond
       (keyword? f) "([map])"
       (symbol? f) (let [var (ns-resolve (maybe-ns package) f)]
                     (if-let [args (and var (:arglists (meta var)))]
                       (pr-str args)
                       nil))
       :else nil))
    (catch Throwable t nil)))

;;;; Package Commands

(defslimefn list-all-package-names
  ([] (map (comp str ns-name) (all-ns)))
  ([nicknames?] (list-all-package-names)))

(defslimefn set-package [name]
  (let [ns (maybe-ns name)]
    (in-ns (ns-name ns))
    (list (str (ns-name ns))
          (str (ns-name ns)))))

;;;; Tracing

(defonce traced-fn-map {})

(def #^{:dynamic true} *trace-level* 0)

(defn- indent [num]
  (dotimes [x (+ 1 num)]
    (print "  ")))

(defn- trace-fn-call [sym f args]
  (let [fname (symbol (str (.name (.ns sym)) "/" (.sym sym)))]
    (indent *trace-level*)
    (println (str *trace-level* ":")
             (apply str (take 240 (pr-str (when fname (cons fname args)) ))))
    (let [result (binding [*trace-level* (+ *trace-level* 1)] (apply f args))]
      (indent *trace-level*)
      (println (str *trace-level* ": " fname " returned " (apply str (take 240 (pr-str result)))))
      result)))

(defslimefn swank-toggle-trace [fname]
  (when-let [sym (maybe-resolve-sym fname)]
    (if-let [f# (get traced-fn-map sym)]
      (do
        (alter-var-root #'traced-fn-map dissoc sym)
        (alter-var-root sym (constantly f#))
        (str " untraced."))
      (let [f# @sym]
        (alter-var-root #'traced-fn-map assoc sym f#)
        (alter-var-root sym
                        (constantly
                         (fn [& args]
                           (trace-fn-call sym f# args))))
        (str " traced.")))))

(defslimefn untrace-all []
  (doseq [sym (keys traced-fn-map)]
    (swank-toggle-trace (.sym sym))))

;;;; Source Locations
(comment
  "Sets the default directory (java's user.dir). Note, however, that
   this will not change the search path of load-file. ")
(defslimefn set-default-directory
  ([directory & ignore]
     (System/setProperty "user.dir" directory)
     directory))


;;;; meta dot find

(defn- clean-windows-path [#^String path]
  ;; Decode file URI encoding and remove an opening slash from
  ;; /c:/program%20files/... in jar file URLs and file resources.
  (or (and (.startsWith (System/getProperty "os.name") "Windows")
           (second (re-matches #"^/([a-zA-Z]:/.*)$" path)))
      path))

(defn- slime-zip-resource [#^java.net.URL resource]
  (let [jar-connection #^java.net.JarURLConnection (.openConnection resource)
        jar-file (.getPath (.toURI (.getJarFileURL jar-connection)))]
    (list :zip (clean-windows-path jar-file) (.getEntryName jar-connection))))

(defn- slime-file-resource [#^java.net.URL resource]
  (list :file (clean-windows-path (.getFile resource))))

(defn- slime-find-resource [#^String file]
  (if-let [resource (.getResource (clojure.lang.RT/baseLoader) file)]
    (if (= (.getProtocol resource) "jar")
      (slime-zip-resource resource)
      (slime-file-resource resource))))

(defn- slime-find-file [#^String file]
  (if (.isAbsolute (File. file))
    (list :file file)
    (slime-find-resource file)))

(defn- namespace-to-path [ns]
  (let [#^String ns-str (name (ns-name ns))
        last-dot-index (.lastIndexOf ns-str ".")]
    (if (pos? last-dot-index)
      (-> (.substring ns-str 0 last-dot-index)
          (.replace \- \_)
          (.replace \. \/)))))

(defn- classname-to-path [class-name]
  (namespace-to-path
   (symbol (.replace class-name \_ \-))))


(defn- location-in-file [path line]
  `(:location ~path (:line ~line) nil))

(defn- location-label [name type]
  (if type
    (str "(" type " " name ")")
    (str name)))

(defn- location [name type path line]
  `((~(location-label name type)
     ~(if path
        (location-in-file path line)
        (list :error (format "%s - definition not found." name))))))

(defn- location-not-found [name type]
  (location name type nil nil))

(defn source-location-for-frame [#^StackTraceElement frame]
  (let [line     (.getLineNumber frame)
        filename (if (.. frame getFileName (endsWith ".java"))
                   (.. frame getClassName (replace \. \/)
                       (substring 0 (.lastIndexOf (.getClassName frame) "."))
                       (concat (str File/separator (.getFileName frame))))
                   (let [ns-path (classname-to-path
                                  ((re-find #"(.*?)\$"
                                            (.getClassName frame)) 1))]
                     (if ns-path
                       (str ns-path File/separator (.getFileName frame))
                       (.getFileName frame))))
        path     (slime-find-file filename)]
    (location-in-file path line)))

(defn- namespace-to-filename [ns]
  (str (-> (str ns)
           (.replaceAll "\\." File/separator)
           (.replace \- \_ ))
       ".clj"))

(defn- source-location-for-meta [meta xref-type-name]
  (location (:name meta)
            xref-type-name
            (slime-find-file (:file meta))
            (:line meta)))

(defn- find-ns-definition [sym-name]
  (if-let [ns (maybe-resolve-ns sym-name)]
    (when-let [path (slime-find-file (namespace-to-filename ns))]
      (location ns nil path 1))))

(defn- find-var-definition [sym-name]
  (if-let [meta (meta (maybe-resolve-sym sym-name))]
    (source-location-for-meta meta "defn")))

(defslimefn find-definitions-for-emacs [name]
  (let [sym-name (read-string name)]
    (or (find-var-definition sym-name)
        (find-ns-definition sym-name)
        (location name nil nil nil))))

(defn who-specializes [class]
  (letfn [(xref-lisp [sym] ; see find-definitions-for-emacs
                     (if-let [meta (meta sym)]
                       (source-location-for-meta meta "method")
                       (location-not-found (.getName sym) "method")))]
    (let [methods (try (. class getMethods)
                       (catch java.lang.IllegalArgumentException e nil)
                       (catch java.lang.NullPointerException e nil))]
      (map xref-lisp methods))))

(defn who-calls [name]
  (letfn [(xref-lisp [sym-var]        ; see find-definitions-for-emacs
                     (when-let [meta (meta sym-var)]
                       (source-location-for-meta meta nil)))]
    (let [callers (xref/all-vars-who-call name) ]
      (map first (map xref-lisp callers)))))

(defslimefn xref [type name]
  (let [sexp (maybe-resolve-sym name)]
       (condp = type
              :specializes (who-specializes sexp)
              :calls   (who-calls (symbol name))
              :callers nil
              :not-implemented)))

(defslimefn throw-to-toplevel []
  (throw debug-quit-exception))

(defn invoke-restart [restart]
  ((nth restart 2)))

(defslimefn invoke-nth-restart-for-emacs [level n]
  ((invoke-restart (*sldb-restarts* (nth (keys *sldb-restarts*) n)))))

(defslimefn throw-to-toplevel []
  (if-let [restart (*sldb-restarts* :quit)]
    (invoke-restart restart)))

(defslimefn sldb-continue []
  (if-let [restart (*sldb-restarts* :continue)]
    (invoke-restart restart)))

(defslimefn sldb-abort []
  (if-let [restart (*sldb-restarts* :abort)]
    (invoke-restart restart)))


(defslimefn backtrace [start end]
  (build-backtrace start end))

(defslimefn buffer-first-change [file-name] nil)

(defn locals-for-emacs [m]
  (sort-by second
           (map #(list :name (name (first %)) :id 0
                       :value (pr-str (second %))) m)))

(defslimefn frame-catch-tags-for-emacs [n] nil)
(defslimefn frame-locals-for-emacs [n]
  (if (and (zero? n) (seq *current-env*))
    (locals-for-emacs *current-env*)))

(defslimefn frame-locals-and-catch-tags [n]
  (list (frame-locals-for-emacs n)
        (frame-catch-tags-for-emacs n)))

(defslimefn debugger-info-for-emacs [start end]
  (build-debugger-info-for-emacs start end))

(defslimefn eval-string-in-frame [expr n]
  (if (and (zero? n) *current-env*)
    (with-bindings *current-env*
      (eval expr))))

(defslimefn frame-source-location [n]
  (source-location-for-frame
   (nth (.getStackTrace *current-exception*) n)))

;; Older versions of slime use this instead of the above.
(defslimefn frame-source-location-for-emacs [n]
  (source-location-for-frame
   (nth (.getStackTrace *current-exception*) n)))

(defslimefn create-repl [target] '("user" "user"))

;;; Threads

(def #^{:private true} thread-list (atom []))

(defn- get-root-group [#^java.lang.ThreadGroup tg]
  (if-let [parent (.getParent tg)]
    (recur parent)
    tg))

(defn- get-thread-list []
  (let [rg (get-root-group (.getThreadGroup (Thread/currentThread)))
        arr (make-array Thread (.activeCount rg))]
    (.enumerate rg arr true)
    (seq arr)))

(defn- extract-info [#^Thread t]
  (map str [(.getId t) (.getName t) (.getPriority t) (.getState t)]))

(defslimefn list-threads
  "Return a list (LABELS (ID NAME STATUS ATTRS ...) ...).
LABELS is a list of attribute names and the remaining lists are the
corresponding attribute values per thread."
  []
  (reset! thread-list (get-thread-list))
  (let [labels '(id name priority state)]
    (cons labels (map extract-info @thread-list))))

;;; TODO: Find a better way, as Thread.stop is deprecated
(defslimefn kill-nth-thread [index]
  (when index
    (when-let [thread (nth @thread-list index nil)]
      (println "Thread: " thread)
      (.stop thread))))

(defslimefn quit-thread-browser []
  (reset! thread-list []))
