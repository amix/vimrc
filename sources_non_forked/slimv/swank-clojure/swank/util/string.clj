(ns swank.util.string)

(defn largest-common-prefix
  "Returns the largest common prefix of two strings."
  ([#^String a, #^String b]
     (apply str (take-while (comp not nil?) (map #(when (= %1 %2) %1) a b))))
  {:tag String})

(defn char-position
  "Finds the position of a character within a string, optionally
   provide a starting index. Returns nil if none is found."
  ([c str] (char-position c str 0))
  ([#^Character c #^String str #^Integer start]
     (let [idx (.indexOf str (int c) start)]
       (when (not= -1 idx)
         idx))))