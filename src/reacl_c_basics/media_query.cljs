(ns reacl-c-basics.media-query
  "Utilities to do CSS media queries and get notified when their results
  change, e.g. the user resized the window."
  (:require [reacl-c.core :as c :include-macros true]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

(c/defn-subscription changes
  "Returns a subscription item, emitting changes of the result of the
  given media query over time."
  deliver! [query]
  (let [mq (js/window.matchMedia query)
        f (fn [mq]
            (deliver! (.-matches mq)))]
    (.addListener mq f)
    (fn []
      (.removeListener mq f))))

(c/defn-effect query!
  "Returns an effect, that performs the given media query and returns
  the result - a boolean value."
  [query]
  (.-matches (js/window.matchMedia query)))

(let [as-state (fn [_ v]
                 (c/return :state v))]
  (defn result
    "Returns an item the keeps state its state up to date with the
  result of the given media query."
    [query]
    (c/fragment
     ;; once to initialize
     (c/handle-effect-result as-state (query! query))
     ;; and listen to changes
     (c/handle-action (changes query)
                      as-state))))

(let [call-f (fn [f args [_ res]]
               (c/focus lens/first (apply f res args)))]
  (defn with-result
    "Returns an item the calls `f` with the result of the given media query, and calls it again when it changes over time."
    [query f & args]
    (c/local-state false
                   (c/fragment
                    (c/focus lens/second (result query))
                    (c/dynamic (f/partial call-f f args))))))
