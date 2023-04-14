(ns reacl-c-basics.core
  "A collection of various small utilities."
  (:require [reacl-c.dom :as dom]
            [reacl-c.core :as c :include-macros true]))

(c/defn-subscription animation-frame
  "Subscription to a single animation frame, emitting the timestamp as an action."
  deliver! []
  (let [id (js/window.requestAnimationFrame deliver!)]
    (fn []
      (js/window.cancelAnimationFrame id))))

(c/defn-subscription animation-frames
  "Subscription to the browser's animation frames, continuously emitting the timestamp as actions."
  deliver! []
  (let [id (atom nil)
        start (fn next []
                (js/window.requestAnimationFrame (fn [timestamp]
                                                   (deliver! timestamp)
                                                   (next))))]
    (start)
    (fn []
      (when-let [v @id]
        (js/window.cancelAnimationFrame v)))))

(c/defn-subscription timeout
  "Subscription to a timer, emitting `true` or `action` as an action once, after the given number of milliseconds."
  deliver! [ms & [action]]
  (let [id (js/window.setTimeout (fn []
                                   (deliver! (if (some? action) action true)))
                                 ms)]
    (fn []
      (js/window.clearTimeout id))))

(c/defn-subscription interval
  "Subscription to a timer, emitting `true` or `action` as an action every given milliseconds."
  deliver! [ms & [action]]
  (let [id (js/window.setInterval (fn []
                                    (deliver! (if (some? action) action true)))
                                  ms)]
    (fn []
      (js/window.clearInterval id))))

(c/defn-subscription intersection-change
  "Returns a subscription to intersection information for the given
  dom element as of the IntersectionObserver API. The options map may
  contain the `:root`, `:root-margin` and `:threshold` keys."
  deliver! [elem options]
  ;; TODO: on a native dom elem?
  (let [obs (new js/IntersectionObserver
                 (fn [changes]
                   (doseq [change (array-seq changes)]
                     (deliver! (js->clj change))))
                 #js {:root (:root options)
                      :rootMargin (:root-margin options)
                      :treshold (to-array (:threshold options))})]
    (.observe obs elem)
    (fn []
      (.unobserve obs elem)
      (.disconnect obs))))

(defn visibility-change
  "Returns a subscription that emits true or false, when the given dom
  element becomes visible or invisible to the user in the browser
  window."
  [elem & [options]]
  (-> (intersection-change elem
                           options)
      (c/map-actions :isIntersecting)))
