(ns reacl-c-basics.pages.ring
  "Functions to help serving the client code in a ring server of a so
  called single page application for all routes handled on the client
  side.

  After defining the routes (in a 'cljc' file):

  ```
  (routes/defroutes my-app
    (routes/defroute home \"/\"))
  ```

  you can create a ring handler that serves the client html with a
  suitable ring response:

  ```
  (-> handler
      (wrap-client-routes my-app response))
  ```
  
  "
  (:require [reacl-c-basics.pages.routes :as routes]))

(defn wrap-client-fn-routes
  "A ring middleware that returns `(client-fn route & route-args)` for
  the given `routes`."
  [handler routes client-fn]
  (fn [request]
    (if-let [route (first (filter #(routes/route-matches % request) routes))]
      ;; TODO: really call client-fn with route?
      (apply client-fn route (routes/route-matches route request))
      (handler request))))

(defn wrap-client-routes
  "A ring middleware that returns the same response (the `client`) for
  the given `routes`."
  [handler routes client]
  (wrap-client-fn-routes handler routes (constantly client)))
