(ns swank.commands.contrib.swank-c-p-c
  (:use (swank util core commands)
        (swank.commands completion)
        (swank.util string clojure)
        (swank.commands.contrib.swank-c-p-c internal)))

(defslimefn completions [symbol-string package]
  (try
   (let [[sym-ns sym-name] (symbol-name-parts symbol-string)
         potential         (concat
			    (potential-completions
			     (when sym-ns (symbol sym-ns))
			     (ns-name (maybe-ns package)))
			    (potential-classes-on-path symbol-string))
         matches           (seq (sort (filter #(split-compound-prefix-match? symbol-string %) potential)))]
     (list matches
           (if matches
             (reduce largest-common-prefix matches)
             symbol-string)))
   (catch java.lang.Throwable t
     (list nil symbol-string))))
