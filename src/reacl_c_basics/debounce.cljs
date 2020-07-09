(ns reacl-c-basics.debounce
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c-basics.core :as core :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]))

(defrecord ^:private Publish [previous state])

(let [publish (fn [{dirty :dirty previous :previous outgoing :outgoing} _]
                (c/return :action (Publish. previous outgoing)
                          :state {:dirty nil
                                  :previous nil
                                  :outgoing nil}))]
  (c/defn ^:private publisher [f]
    (c/with-state-as {dirty :dirty}
      (if dirty
        ;; we want to enforce a restart of the publisher everytime something changes; so add a (random) key:
        (-> f
            (c/keyed (str dirty))
            (c/handle-action publish))
        c/empty))))

(defn- current-state
  ([[outer {dirty :dirty outgoing :outgoing}]]
   (if dirty outgoing outer))
  ([[outer {dirty :dirty}] new-inner]
   [outer {:dirty (if dirty (inc dirty) 0)
           :previous outer
           :outgoing new-inner}]))

(let [set-state (fn [opt-resolve current a]
                  (if (instance? Publish a)
                    (c/return :state
                              (if (and opt-resolve
                                       (not= current (:previous a)))
                                (opt-resolve current (:state a))
                                (:state a)))
                    (c/return :action a)))]
  (c/defn ^:private debouncer [item f opt-resolve]
    (c/with-state-as outer-1
      ;; respect to the beginning of the delay, and add an optional fn to
      ;; resolve such a conflict.
      (-> (c/local-state {:dirty nil
                          :previous nil
                          :outgoing nil}
                         (c/fragment (c/focus current-state item)
                                     (c/focus lens/second (publisher f))))
          (c/handle-action (f/partial set-state opt-resolve))))))

(defn debounce
  "Delays the upward propagation of state updates of the given item,
  until the next animation frame."
  [item & [resolve-conflict]]
  (debouncer item (core/animation-frame) resolve-conflict))

(defn debounce-delay
  "Delays the upward propagation of state updates of the given item,
  for the given number of milliseconds."
  [ms item & [resolve-conflict]]
  (debouncer item (core/timeout ms) resolve-conflict))
