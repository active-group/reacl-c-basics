(ns ^:no-doc reacl-c-basics.mvvm.async
  "Internal async command abstraction.")

;; actions

(defrecord ^:Private AsyncCommand [f])

(defn async-command [f]
  (AsyncCommand. f))

(defn async-command? [v]
  (instance? AsyncCommand v))

(defn run [command update!]
  (assert (instance? AsyncCommand command))
  ((:f command) update!))
