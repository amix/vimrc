(ns swank.util.concurrent.mbox
  (:refer-clojure :exclude [send get]))

;; Holds references to the mailboxes (message queues)
(defonce mailboxes (ref {}))

(defn get
  "Returns the mailbox for a given id. Creates one if one does not
   already exist."
  ([id]
     (dosync
      (when-not (@mailboxes id)
        (alter mailboxes assoc
               id (java.util.concurrent.LinkedBlockingQueue.))))
     (@mailboxes id))
  {:tag java.util.concurrent.LinkedBlockingQueue})

(defn send
  "Sends a message to a given id."
  ([id message]
     (let [mbox (get id)]
       (.put mbox message))))

(defn receive
  "Blocking recieve for messages for the given id."
  ([id]
     (let [mb (get id)]
       (.take mb))))

(defn clean []
  )
