(ns swank.util.clojure)

(defn unmunge
  "Converts a javafied name to a clojure symbol name"
  ([#^String name]
     (reduce (fn [#^String s [to from]]
               (.replaceAll s from (str to)))
             name
             clojure.lang.Compiler/CHAR_MAP)))

(defn ns-path
  "Returns the path form of a given namespace"
  ([#^clojure.lang.Namespace ns]
     (let [#^String ns-str (name (ns-name ns))]
       (-> ns-str
           (.substring 0 (.lastIndexOf ns-str "."))
           (.replace \- \_)
           (.replace \. \/)))))

(defn symbol-name-parts
  "Parses a symbol name into a namespace and a name. If name doesn't
   contain a namespace, the default-ns is used (nil if none provided)."
  ([symbol]
     (symbol-name-parts symbol nil))
  ([#^String symbol default-ns]
     (let [ns-pos (.indexOf symbol (int \/))]
       (if (= ns-pos -1) ;; namespace found? 
         [default-ns symbol] 
         [(.substring symbol 0 ns-pos) (.substring symbol (inc ns-pos))]))))

(defn resolve-ns [sym ns]
  (or (find-ns sym)
      (get (ns-aliases ns) sym))) 