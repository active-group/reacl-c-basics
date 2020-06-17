(ns reacl-c-basics.core
  (:require [reacl-c.dom :as dom]
            [reacl-c.core :as c :include-macros true]))

(defn ^:no-doc split-dom-attrs [args]
  (if (and (not-empty args)
           (dom/dom-attributes? (first args)))
    [(first args) (rest args)]
    [{} args]))

(defn ^:no-doc dom-like [f]
  (fn [& args]
    (let [[attrs children] (split-dom-attrs args)]
      (apply f attrs children))))

(c/defn-subscription animation-frame
  "Subscription to the browser's animation frames, emitting the timestamps as actions."
  deliver! []
  (let [id (js/window.requestAnimationFrame (fn [timestamp]
                                              (deliver! timestamp)))]
    (fn []
      (js/window.cancelAnimationFrame id))))

(c/defn-subscription timeout
  "Subscription to a timer, emitting `true` as an action once, after they given number of milliseconds."
  deliver! [ms]
  (let [id (js/window.setTimeout (fn []
                                   (deliver! true))
                                 ms)]
    (fn []
      (js/window.clearTimeout id))))

(c/defn-subscription interval
  "Subscription to a timer, emitting `true` as an action every given milliseconds."
  deliver! [ms]
  (let [id (js/window.setInterval (fn []
                                    (deliver! true))
                                  ms)]
    (fn []
      (js/window.clearInterval id))))
