(ns swank.commands.contrib.swank-c-p-c.internal
  (:use (swank util core commands)
        (swank.commands completion)
        (swank.util string clojure)))

(defn compound-prefix-match?
  "Takes a `prefix' and a `target' string and returns whether `prefix'
  is a compound-prefix of `target'.

  Viewing each of `prefix' and `target' as a series of substrings
  split by `split', if each substring of `prefix' is a prefix of the
  corresponding substring in `target' then we call `prefix' a
  compound-prefix of `target'."
  ([split #^String prefix #^String target]
     (let [prefixes (split prefix)
           targets  (split target)]
       (when (<= (count prefixes) (count targets))
         (every? true? (map #(.startsWith #^String %1 %2) targets prefixes))))))

(defn unacronym
  "Interposes delimiter between each character of string."
  ([delimiter #^String string]
     (apply str (interpose delimiter string)))
  {:tag String})

(defn delimited-compound-prefix-match?
  "Uses a delimiter as the `split' for a compound prefix match check.
  See also: `compound-prefix-match?'"
  ([delimiter prefix target]
     (compound-prefix-match? #(.split #^String % (str "[" (java.util.regex.Pattern/quote delimiter) "]") -1)
                             prefix
                             target)))


(defn delimited-compound-prefix-match-acronym?
  ([delimiter prefix target]
     (or (delimited-compound-prefix-match? delimiter prefix target)
         (delimited-compound-prefix-match? delimiter (unacronym (first delimiter) prefix) target))))

(defn camel-compound-prefix-match?
  "Uses camel case as a delimiter for a compound prefix match check.

   See also: `compound-prefix-match?'"
  ([#^String prefix #^String target]
     (compound-prefix-match? #(re-seq #"(?:^.|[A-Z])[^A-Z]*" %)
                             prefix
                             target)))

(defn split-compound-prefix-match? [#^String symbol-string #^String potential]
  (if (.startsWith symbol-string ".")
    (and (.startsWith potential ".")
         (camel-compound-prefix-match? symbol-string potential))
    (let [[sym-ns sym-name] (symbol-name-parts symbol-string)
          [pot-ns pot-name] (symbol-name-parts potential)]
      (and (or (= sym-ns pot-ns)
               (and sym-ns pot-ns
                    (delimited-compound-prefix-match-acronym? "." sym-ns pot-ns)))
           (or (delimited-compound-prefix-match-acronym? "-." sym-name pot-name)
               (camel-compound-prefix-match? sym-name pot-name))))))
