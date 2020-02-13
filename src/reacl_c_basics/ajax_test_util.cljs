(ns reacl-c-basics.ajax-test-util
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.test-util.core :as tu]))

(defn request-subscribe-effect?
  "Returns if the given effect action performs the subscription to the result of the given request."
  [eff & [request]]
  (if request
    ;; with a concrete requests
    (tu/subscribe-effect? eff (ajax/execute request))
    ;; or generally, a request should be at a certain position of the subscription-effect args
    (and (tu/subscribe-effect? eff)
         (let [[req] (tu/subscribe-effect-args eff)]
           (ajax/request? req)))))

(defn request-unsubscribe-effect?
  "Returns if the given effect action performs the unsubscription from the result of the given request."
  [eff]
  (and (tu/unsubscribe-effect? eff)
       (let [[req] (tu/unsubscribe-effect-args eff)]
         (ajax/request? req))))

(defn request-subscribe-effect-request
  [eff]
  ;; Note: only for the subscription effect (not the unsubscription)
  (assert (request-subscribe-effect? eff))
  (first (tu/subscribe-effect-args eff)))

(defn request-unsubscribe-effect-request
  [eff]
  ;; Note: only for the subscription effect (not the unsubscription)
  (assert (request-unsubscribe-effect? eff))
  (first (tu/unsubscribe-effect-args eff)))

(defn requests-emulator
  "Returns an effect reducer, that calls f for all ajax requests that
  are subscribed to in a test environment, and if f returns a
  response, then makes it an action emitted from the corresponding
  subscription item. Otherwise it is passed on."
  [f]
  (fn [env eff]
    (cond
      (request-subscribe-effect? eff)
      (let [req (request-subscribe-effect-request eff)]
        (if-let [resp (f req)]
          ;; TODO: merge-returned should be in core or tu
          (reacl-c.base/merge-returned (tu/subscription-start-return env eff)
                                       (tu/subscription-result-return env eff resp))
          (c/return :action eff)))
      
      (request-unsubscribe-effect? eff)
      (let [req (request-unsubscribe-effect-request eff)]
        ;; is it one of the requests we emulate here? Then drop
        ;; it (it's a noop anyway - no stop! fn passed to
        ;; subscription-start-return)
        (if (some? (f req))
          (c/return)
          (c/return :action eff)))
      
      :else
      (c/return :action eff))))

