(ns swank.loader
  (:require [swank.util.sys :as sys]
            [swank.util.clojure :as clj])
  (:import [java.io File]))

(defonce #^File *swank-source-path*
  (if-let [resource (.getResource (clojure.lang.RT/baseLoader)
                                  #^String *file*)]
    (.getParentFile (File. (.getFile resource)))))

(defonce #^File *swank-compile-path*
  (File. (str (sys/user-home-path)
              File/separator
              ".slime"
              File/separator
              "cljclass")))

(defn file-directory? [#^File f]
  (.isDirectory f))

(defn file-last-modified [#^File f]
  (.lastModified f))

(defn all-files-in-directory [#^File f]
  (let [list-files  (.listFiles f)
        files       (remove file-directory? list-files)
        directories (filter file-directory? list-files)]
    (concat files (mapcat all-files-in-directory directories))))

(defn clj-file? [#^File f]
  (.endsWith (str f) ".clj"))

(defn swank-source-files [#^File path]
  (filter clj-file? (all-files-in-directory path)))

(defn relative-path-name [#^File parent #^File file]
  (let [file-name (str file)
        parent-name (str parent)]
    (when (.startsWith file-name parent-name)
      (.substring file-name (inc (.length parent-name))))))

(defn file-name-to-swank-package-sym [#^String file-name]
  (assert (clj-file? file-name))
  (symbol
   (str "swank."
        (clj/unmunge
         (.replaceAll (.substring file-name 0 (- (.length file-name) 4))
                      File/separator
                      ".")))))

(defn swank-packages []
  (map #(file-name-to-swank-package-sym (relative-path-name *swank-source-path* %))
       (swank-source-files *swank-source-path*)))

(defn swank-version
  "A likely bad way of calculating a version number for swank clojure"
  ([]
     (str (reduce + (map file-last-modified (swank-source-files *swank-source-path*)))
	  "+" (clojure-version))))

(defn delete-file-recursive [& paths]
  (when-not (empty? paths)
    (let [f #^File (first paths)]
      (if (and f (.exists f))
        (if (.isDirectory f)
          (if-let [files (seq (.listFiles f))]
            (recur (concat files paths))
            (do
              (.delete f)
              (recur (rest paths))))
          (do
            (.delete f)
            (recur (rest paths))))
        (recur (rest paths))))))

(defn clean-up []
  (let [current-path (File. *swank-compile-path* (str (swank-version)))]
    (doseq [compiled-path (.listFiles *swank-compile-path*)
            :when (not= current-path compiled-path)]
      (delete-file-recursive compiled-path))))

(defn swank-ns? [ns]
  (.startsWith (name (ns-name ns)) "swank."))

(defn all-swank-ns []
  (filter swank-ns? (all-ns)))

(defn compile-swank [#^String path]
  (binding [*compile-path* path]
    (doseq [sym (swank-packages)]
      (println "Compiling" (name sym))
      (compile sym))))

(defn init []
  (let [path (File. *swank-compile-path* (str (swank-version)))
        path-already-exists? (.exists path)]
    (when-not path-already-exists?
      (.mkdirs path))
    (add-classpath (-> path .toURI .toURL))
    (when-not path-already-exists?
      (compile-swank (str path)))))
