(ns reacl-c-basics.ajax-test-util
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c-basics.ajax :as ajax]
            [active.clojure.functions :as f]
            [reacl-c.test-util.core :as tu]))

(defn- request [sub]
  (and (tu/subscription? sub ajax/execute)
       (first (tu/subscription-args sub))))

(let [f (fn [deliver! resp] (deliver! resp) (constantly nil))]
  (defn- const-sub [resp]
    (c/subscription f resp)))

(let [h (fn [f sub]
          (if-let [resp (when-let [req (request sub)]
                          (f req))]
            (do (assert (ajax/response? resp))
                (const-sub resp))
            nil))]
  (defn emulate-requests
    "Returns an item that calls `f` on every ajax request executed in
  `item`. The function may return an ajax response, in which case the
  request isn't actually executed, but results in the returned response
  instead.

  If the function returns `nil`, the request is executed as
  normal. Note that this means you can pass in a map as `f`, mapping
  requests to responsed."
    [item f]
    ;; It used to be possible that f returns c/no-effect to turn request 'off'. But who want's that?
    (tu/map-subscriptions item (f/partial h f))))
