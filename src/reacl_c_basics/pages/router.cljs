(ns reacl-c-basics.pages.router
  (:require [reacl-c.core :as c :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [reacl-c-basics.pages.history :as history]
            [reacl-c-basics.pages.routes :as routes]))

(defn- get-parsed [pages uri]
  (some (fn [[route page]]
          (when-let [args (routes/parse route uri)]
            [page args]))
        pages))

(defn- page-exists-fn? [pages uri]
  (some? (get-parsed pages uri)))

(defrecord ^:private Goto [uri])

(defn goto
  "Returns an action to be handled by a [[history-router]] up in the
  hierarchy, instructing it to show the page registered for the given
  `path` (and query string)."
  [path]
  (Goto. path))

(defn- show-page [pages uri]
  (when uri
    (if-let [[page args] (get-parsed pages uri)]
      (apply page args)
      (do (js/console.warn "Route not found:" uri) ;; TODO: side effect!?
          nil))))

(let [set-uri (fn [state uri] uri)
      handle-goto (fn [history state a]
                    (condp instance? a
                      Goto (c/return :action (history/push! history (:uri a))
                                     :state (assoc state 1 (:uri a)))
                      (c/return :action a)))]
  (c/defn history-router
    "Returns an item class that dispatches rendering based on the
  given map of routes to pages, where the current route and route
  changes are managed by the given implementation of
  the [[reacl-c-basics.pages.history/History]] protocol."
    [history pages]
    (c/with-state-as [_ uri :local nil]
      (c/fragment
       (c/focus lens/second
                (-> (history/listen history (f/partial page-exists-fn? pages))
                    (c/handle-action set-uri)))
       (-> (c/focus lens/first (show-page pages uri))
           (c/handle-action (f/partial handle-goto history)))))))

