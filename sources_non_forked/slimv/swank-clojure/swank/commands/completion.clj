(ns swank.commands.completion
  (:use (swank util core commands)
        (swank.util string clojure java class-browse)))

(defn potential-ns
  "Returns a list of potential namespace completions for a given
   namespace"
  ([] (potential-ns *ns*))
  ([ns]
     (for [ns-sym (concat (keys (ns-aliases (ns-name ns)))
                          (map ns-name (all-ns)))]
       (name ns-sym))))

(defn potential-var-public
  "Returns a list of potential public var name completions for a
   given namespace"
  ([] (potential-var-public *ns*))
  ([ns]
     (for [var-sym (keys (ns-publics ns))]
       (name var-sym))))

(defn potential-var
  "Returns a list of all potential var name completions for a given
   namespace"
  ([] (potential-var *ns*))
  ([ns]
     (for [[key v] (ns-map ns)
           :when (var? v)]
       (name key))))

(defn potential-classes
  "Returns a list of potential class name completions for a given
   namespace"
  ([] (potential-classes *ns*))
  ([ns]
     (for [class-sym (keys (ns-imports ns))]
       (name class-sym))))

(defn potential-dot
  "Returns a list of potential dot method name completions for a given
   namespace"
  ([] (potential-dot *ns*))
  ([ns]
     (map #(str "." %) (set (map member-name (mapcat instance-methods (vals (ns-imports ns))))))))

(defn potential-static
  "Returns a list of potential static members for a given namespace"
  ([#^Class class]
     (concat (map member-name (static-methods class))
	     (map member-name (static-fields class)))))


(defn potential-classes-on-path
  "Returns a list of Java class and Clojure package names found on the current
  classpath. To minimize noise, list is nil unless a '.' is present in the search
  string, and nested classes are only shown if a '$' is present."
  ([symbol-string]
	 (when (.contains symbol-string ".")
	   (if (.contains symbol-string "$")
		 @nested-classes
		 @top-level-classes))))

(defn resolve-class
  "Attempts to resolve a symbol into a java Class. Returns nil on
   failure."
  ([sym]
     (try
      (let [res (resolve sym)]
        (when (class? res)
          res))
      (catch Throwable t
        nil))))


(defn- maybe-alias [sym ns]
  (or (resolve-ns sym (maybe-ns ns))
      (maybe-ns ns)))

(defn potential-completions [symbol-ns ns]
  (if symbol-ns
    (map #(str symbol-ns "/" %)
         (if-let [class (resolve-class symbol-ns)]
           (potential-static class)
           (potential-var-public (maybe-alias symbol-ns ns))))
    (concat (potential-var ns)
            (when-not symbol-ns
              (potential-ns))
            (potential-classes ns)
            (potential-dot ns))))


(defslimefn simple-completions [symbol-string package]
  (try
   (let [[sym-ns sym-name] (symbol-name-parts symbol-string)
		 potential         (concat (potential-completions (when sym-ns (symbol sym-ns)) (ns-name (maybe-ns package)))
								   (potential-classes-on-path symbol-string))
         matches           (seq (sort (filter #(.startsWith #^String % symbol-string) potential)))]
     (list matches
           (if matches
             (reduce largest-common-prefix matches)
             symbol-string)))
   (catch java.lang.Throwable t
     (list nil symbol-string))))
