(ns reacl-c-basics.pages.ring
  (:require [reacl-c-basics.pages.routes :as routes]))

(defn wrap-client-fn-routes
  [handler routes client-fn]
  (fn [request]
    (if-let [route (first (filter #(routes/route-matches % request) routes))]
      ;; TODO: really call client-fn with route?
      (apply client-fn route (routes/route-matches route request))
      (handler request))))

(defn wrap-client-routes
  [handler routes client]
  (wrap-client-fn-routes handler routes (constantly client)))
