(ns swank.commands.indent
  (:use (swank util core)
        (swank.core hooks connection)
        (swank.util hooks)))

(defn- need-full-indentation-update?
  "Return true if the indentation cache should be updated for all
   namespaces. 

   This is a heuristic so as to avoid scanning all symbols from all
   namespaces. Instead, we only check whether the set of namespaces in
   the cache match the set of currently defined namespaces."
  ([connection]
     (not= (hash (all-ns))
           (hash @(connection :indent-cache-pkg)))))

(defn- find-args-body-position
  "Given an arglist, return the number of arguments before 
     [... & body]
   If no & body is found, nil will be returned"
  ([args]
     (when (coll? args)
       (when-let [amp-position (position '#{&} args)]
         (when-let [body-position (position '#{body clauses} args)]
           (when (= (inc amp-position) body-position)
             amp-position))))))

(defn- find-arglists-body-position
  "Find the smallest body position from an arglist"
  ([arglists]
     (let [positions (remove nil? (map find-args-body-position arglists))]
       (when-not (empty? positions)
         (apply min positions)))))

(defn- find-var-body-position
  "Returns a var's :indent override or the smallest body position of a
   var's arglists"
  ([var]
     (let [var-meta (meta var)]
       (or (:indent var-meta)
           (find-arglists-body-position (:arglists var-meta))))))

(defn- var-indent-representation
  "Returns the slime indentation representation (name . position) for
   a given var. If there is no indentation representation, nil is
   returned."
  ([var]
     (when-let [body-position (find-var-body-position var)]
       (when (or (= body-position 'defun)
                 (not (neg? body-position)))
         (list (name (:name (meta var)))
               '.
               body-position)))))

(defn- get-cache-update-for-var
  "Checks whether a given var needs to be updated in a cache. If it
   needs updating, return [var-name var-indentation-representation].
   Otherwise return nil"
  ([find-in-cache var]
     (when-let [indent (var-indent-representation var)]
       (let [name (:name (meta var))]
         (when-not (= (find-in-cache name) indent)
           [name indent])))))

(defn- get-cache-updates-in-namespace
  "Finds all cache updates needed within a namespace"
  ([find-in-cache ns]
     (remove nil? (map (partial get-cache-update-for-var find-in-cache) (vals (ns-interns ns))))))

(defn- update-indentation-delta
  "Update the cache and return the changes in a (symbol '. indent) list.
   If FORCE is true then check all symbols, otherwise only check
   symbols belonging to the buffer package"
  ([cache-ref load-all-ns?]
     (let [find-in-cache @cache-ref]
       (let [namespaces (if load-all-ns? (all-ns) [(maybe-ns *current-package*)])
             updates (mapcat (partial get-cache-updates-in-namespace find-in-cache) namespaces)]
         (when (seq updates)
           (dosync (alter cache-ref into updates))
           (map second updates))))))

(defn- perform-indentation-update
  "Update the indentation cache in connection and update emacs.
   If force is true, then start again without considering the old cache."
  ([conn force]
     (let [cache (conn :indent-cache)]
       (let [delta (update-indentation-delta cache force)]
         (dosync
          (ref-set (conn :indent-cache-pkg) (hash (all-ns)))
          (when (seq delta)
            (send-to-emacs `(:indentation-update ~delta))))))))

(defn- sync-indentation-to-emacs
  "Send any indentation updates to Emacs via emacs-connection"
  ([]
     (perform-indentation-update
      *current-connection*
      (need-full-indentation-update? *current-connection*))))

(add-hook pre-reply-hook #'sync-indentation-to-emacs)
