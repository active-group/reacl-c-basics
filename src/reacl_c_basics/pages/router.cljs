(ns ^:no-doc reacl-c-basics.pages.router
  ;; Note: use corresponding functions in 'pages.core'.
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

(let [g (fn [f state a]
          (if (instance? Goto a)
            (f state (:uri a))
            (c/return :action a)))]
  (defn handle-goto [item f]
    (-> item
        (c/handle-action (f/partial g f)))))

(defn- dispatch [pages uri]
  (when uri
    (if-let [[page args] (get-parsed pages uri)]
      (apply page args)
      (do (js/console.warn "Route not found:" uri) ;; TODO: side effect!?
          nil))))

(let [set-uri (fn [state uri] uri)
      do-goto (fn [history state uri]
                (c/return :action (history/push! history uri)
                          :state (assoc state 1 uri)))]
  (c/defn-item history-router
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
       (-> (c/focus lens/first (dispatch pages uri))
           (handle-goto (f/partial do-goto history)))))))

