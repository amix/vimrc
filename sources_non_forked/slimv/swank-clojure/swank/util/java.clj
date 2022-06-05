(ns swank.util.java)

(defn member-name [#^java.lang.reflect.Member member]
  (.getName member))

(defn member-static? [#^java.lang.reflect.Member member]
  (java.lang.reflect.Modifier/isStatic (.getModifiers member)))

(defn static-methods [#^Class class]
  (filter member-static? (.getMethods class)))

(defn static-fields [#^Class class]
  (filter member-static? (.getDeclaredFields class)))

(defn instance-methods [#^Class class]
  (remove member-static? (.getMethods class)))
