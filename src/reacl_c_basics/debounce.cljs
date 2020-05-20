(ns reacl-c-basics.debounce
  (:require [reacl-c.core :as c]
            [active.clojure.functions :as f]))

(c/defn-subscription animation-frame deliver! [_]
  (let [id (js/window.requestAnimationFrame (fn [timestamp]
                                              (deliver! timestamp)))]
    (fn []
      (js/window.cancelAnimationFrame id))))

(c/defn-subscription timer deliver! [ms _]
  (let [id (js/window.setTimeout (fn []
                                   (deliver! true))
                                 ms)]
    (fn []
      (js/window.clearTimeout id))))

(let [publish (fn [[outer {dirty? :dirty? outgoing :outgoing}] _]
                (c/return :state [outgoing {:dirty? false :outgoing nil}]))]
  (c/defn-dynamic ^:private publisher [outer {dirty? :dirty? outgoing :outgoing}] [f]
    (if dirty?
      ;; we want to enforce a restart of the publisher overytime something changes; so add a random key:
      (-> (f (str (rand)))
          (c/handle-action publish))
      c/empty)))

(defn- current-state
  ([[outer {dirty? :dirty? outgoing :outgoing}]]
   (if dirty? outgoing outer))
  ([[outer {dirty? :dirty? outgoing :outgoing}] new-inner]
   [outer {:dirty? true :outgoing new-inner}]))

(defn- debouncer [item f]
  ;; TODO: we should check if the outer state has changed, with
  ;; respect to the beginning of the delay, and add an optional fn to
  ;; resolve such a conflict.
  (c/local-state {:dirty? false
                  :outgoing nil}
                 (c/fragment (c/focus current-state item)
                             (publisher f))))

(defn debounce
  "Delays the upward propagation of state updates of the given item,
  until the next animation frame."
  [item]
  (debouncer item animation-frame))

(defn debounce-delay
  "Delays the upward propagation of state updates of the given item,
  for the given number of milliseconds."
  [ms item]
  (debouncer item (f/partial timer ms)))
