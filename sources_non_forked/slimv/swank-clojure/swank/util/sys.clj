(ns swank.util.sys)

(defn get-pid
  "Returns the PID of the JVM. This is largely a hack and may or may
   not be accurate depending on the JVM in which clojure is running
   off of."
  ([]
     (or (first (.. java.lang.management.ManagementFactory (getRuntimeMXBean) (getName) (split "@")))
         (System/getProperty "pid")))
  {:tag String})

(defn user-home-path []
  (System/getProperty "user.home"))
