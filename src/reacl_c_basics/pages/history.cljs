(ns ^:no-doc reacl-c-basics.pages.history
  (:require [accountant.core :as accountant]
            [reacl-c-basics.pages.r-history :as r-history]
            [reacl-c.core :as c :include-macros true]))

(c/defn-subscription listen dispatch! [history page-exists?]
  (dispatch! (r-history/get-current history))
  (r-history/start! history dispatch! page-exists?)
  (fn []
    (r-history/stop! history)))

(c/defn-effect push! [history uri]
  (r-history/push! history uri))

(defrecord ^:private Html5History [auto-nav?]
  r-history/History
  (start! [_ nav-path! path-exists?]
    (accountant/configure-navigation!
     {:nav-handler (fn [path] (when-not @auto-nav? (nav-path! path)))
      :path-exists? path-exists?}))
  (get-current [_]
    ;; akin to accountang/dispatch-current!
    (let [path (-> js/window .-location .-pathname)
          query (-> js/window .-location .-search)
          hash (-> js/window .-location .-hash)]
      (str path query hash)))
  (push! [_ path]
    ;; navigate! triggers the nav-handler (even synchronous), to prevent that, we could unlisten, call it, then listen again.
    ;; but it should also ok to use a global atom for now; there can't be multiple history listers anyway.
    (try (reset! auto-nav? true)
         (accountant/navigate! path)
         (finally (reset! auto-nav? false))))
  (stop! [_]
    (accountant/unconfigure-navigation!)))

(let [glob-h (Html5History. (atom false))]
  (defn html5-history
    "Returns an implementation of the [[History]] protocol using the Html5 History API, resp. `venantius/accountant`."
    [] ;; TODO :reload-same-path? option?
    ;; Note: put a data-trigger attribute on <a> that are not client-links
    ;; Note: return value should compare = for equal arguments.
    glob-h))

(defrecord ^:private ControlledHistory [base inactive?]
  r-history/History
  (start! [this nav-path! path-exists?]
    (when-not inactive?
      (r-history/start! base nav-path!
                        path-exists?)))
  (get-current [this]
    (r-history/get-current base))
  (push! [this path]
    (when-not inactive?
      (r-history/push! base path)))
  (stop! [this]
    (when-not inactive?
      (r-history/stop! base))))

(defn controlled-history
  "A modified history, that can be turned off with a boolean flag."
  [base inactive?]
  (assert (satisfies? r-history/History base))
  (ControlledHistory. base inactive?))

(defn controlled-html5-history
  "Like [[controlled-history]] with a HTML5 historory is the base."
  [inactive?]
  (controlled-history (html5-history) inactive?))
