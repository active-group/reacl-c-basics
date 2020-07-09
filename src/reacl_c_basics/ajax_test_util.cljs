(ns reacl-c-basics.ajax-test-util
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c-basics.ajax :as ajax]
            [active.clojure.functions :as f]
            [reacl-c.test-util.core :as tu]))

(defn request-subscribe-effect?
  "Returns if the given effect action performs the subscription to the result of the given request."
  [eff & [request]]
  ;; Note: unsure why this does not work: (tu/subscribe-effect? eff (ajax/execute request))
  (and (tu/subscribe-effect? eff)
       (or (nil? request)
           (tu/subscribe-effect? eff (ajax/execute request)))))

(defn request-subscribe-effect-request [eff]
  {:pre [(request-subscribe-effect? eff)]}
  (first (tu/subscribe-effect-args eff)))

(let [h (fn [f sub-effect]
          (if-let [resp (and (request-subscribe-effect? sub-effect)
                             (f (request-subscribe-effect-request sub-effect)))]
            resp ;; the response is the action
            nil  ;; keep effect as is
            ))]
  (defn emulate-requests [item f]
    ;; f may return c/no-effect to turn request 'off'.
    (tu/emulate-subscriptions item
                              (f/partial h f))))
