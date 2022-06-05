;;; class-browse.clj -- Java classpath and Clojure namespace browsing

;; by Jeff Valk
;; created 2009-10-14

;; Scans the classpath for all class files, and provides functions for
;; categorizing them.

;; See the following for JVM classpath and wildcard expansion rules:
;;   http://java.sun.com/javase/6/docs/technotes/tools/findingclasses.html
;;   http://java.sun.com/javase/6/docs/technotes/tools/solaris/classpath.html

(ns swank.util.class-browse
  "Provides Java classpath and (compiled) Clojure namespace browsing.
  Scans the classpath for all class files, and provides functions for
  categorizing them. Classes are resolved on the start-up classpath only.
  Calls to 'add-classpath', etc are not considered.

  Class information is built as a list of maps of the following keys:
    :name  Java class or Clojure namespace name
    :loc   Classpath entry (directory or jar) on which the class is located
    :file  Path of the class file, relative to :loc"
  (:import [java.io File FilenameFilter]
           [java.util StringTokenizer]
           [java.util.jar JarFile JarEntry]
           [java.util.regex Pattern]))

;;; Class file naming, categorization

(defn jar-file? [#^String n] (.endsWith n ".jar"))
(defn class-file? [#^String n] (.endsWith n ".class"))
(defn clojure-ns-file? [#^String n] (.endsWith n "__init.class"))
(defn clojure-fn-file? [#^String n] (re-find #"\$.*__\d+\.class" n))
(defn top-level-class-file? [#^String n] (re-find #"^[^\$]+\.class" n))
(defn nested-class-file? [#^String n]
  ;; ^ excludes anonymous classes
  (re-find #"^[^\$]+(\$[^\d]\w*)+\.class" n))

(def clojure-ns? (comp clojure-ns-file? :file))
(def clojure-fn? (comp clojure-fn-file? :file))
(def top-level-class? (comp top-level-class-file? :file))
(def nested-class? (comp nested-class-file? :file))

(defn class-or-ns-name
  "Returns the Java class or Clojure namespace name for a class relative path."
  [#^String n]
  (.replace
   (if (clojure-ns-file? n)
     (-> n (.replace "__init.class" "") (.replace "_" "-"))
     (.replace n ".class" ""))
   File/separator "."))

;;; Path scanning

(defmulti path-class-files
  "Returns a list of classes found on the specified path location
  (jar or directory), each comprised of a map with the following keys:
    :name  Java class or Clojure namespace name
    :loc   Classpath entry (directory or jar) on which the class is located
    :file  Path of the class file, relative to :loc"
  (fn [#^ File f _]
    (cond (.isDirectory f)           :dir
          (jar-file? (.getName f))   :jar
          (class-file? (.getName f)) :class)))

(defmethod path-class-files :default
  [& _] [])

(defmethod path-class-files :jar
  ;; Build class info for all jar entry class files.
  [#^File f #^File loc]
  (let [lp (.getPath loc)]
    (try
     (map (fn [fp] {:loc lp :file fp :name (class-or-ns-name fp)})
          (filter class-file?
                  (map #(.getName #^JarEntry %)
                       (enumeration-seq (.entries (JarFile. f))))))
     (catch Exception e []))))          ; fail gracefully if jar is unreadable

(defmethod path-class-files :dir
  ;; Dispatch directories and files (excluding jars) recursively.
  [#^File d #^File loc]
  (let [fs (.listFiles d (proxy [FilenameFilter] []
                           (accept [d n] (not (jar-file? n)))))]
    (reduce concat (for [f fs] (path-class-files f loc)))))

(defmethod path-class-files :class
  ;; Build class info using file path relative to parent classpath entry
  ;; location. Make sure it decends; a class can't be on classpath directly.
  [#^File f #^File loc]
  (let [fp (.getPath f), lp (.getPath loc)
        m (re-matcher (re-pattern (Pattern/quote
                                   (str "^" lp File/separator))) fp)]
    (if (not (.find m))                 ; must be descendent of loc
      []
      (let [fpr (.substring fp (.end m))]
        [{:loc lp :file fpr :name (class-or-ns-name fpr)}]))))

;;; Classpath expansion

(def java-version
     (Float/parseFloat (.substring (System/getProperty "java.version") 0 3)))

(defn expand-wildcard
  "Expands a wildcard path entry to its matching .jar files (JDK 1.6+).
  If not expanding, returns the path entry as a single-element vector."
  [#^String path]
  (let [f (File. path)]
    (if (and (= (.getName f) "*") (>= java-version 1.6))
      (-> f .getParentFile
          (.list (proxy [FilenameFilter] []
                   (accept [d n] (jar-file? n)))))
      [f])))

(defn scan-paths
  "Takes one or more classpath strings, scans each classpath entry location, and
  returns a list of all class file paths found, each relative to its parent
  directory or jar on the classpath."
  ([cp]
     (if cp
       (let [entries (enumeration-seq
                      (StringTokenizer. cp File/pathSeparator))
             locs (mapcat expand-wildcard entries)]
         (reduce concat (for [loc locs] (path-class-files loc loc))))
       ()))
  ([cp & more]
     (reduce #(concat %1 (scan-paths %2)) (scan-paths cp) more)))

;;; Class browsing

(def available-classes
     (filter (complement clojure-fn?)  ; omit compiled clojure fns
             (scan-paths (System/getProperty "sun.boot.class.path")
                         (System/getProperty "java.ext.dirs")
                         (System/getProperty "java.class.path"))))

;; Force lazy seqs before any user calls, and in background threads; there's
;; no sense holding up SLIME init. (It's usually quick, but a monstrous
;; classpath could concievably take a while.)

(def top-level-classes
     (future (doall (map (comp class-or-ns-name :name)
                         (filter top-level-class?
                                 available-classes)))))

(def nested-classes
     (future (doall (map (comp class-or-ns-name :name)
                         (filter nested-class?
                                 available-classes)))))
