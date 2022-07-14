(ns ^:no-doc reacl-c-basics.pages.history
  (:require [accountant.core :as accountant]
            [cemerick.url :as url]
            [reacl-basics.pages.history :as r-history]
            [reacl-c.core :as c :include-macros true]))

(c/defn-subscription listen dispatch! [history page-exists?]
  (dispatch! (r-history/get-current history))
  (r-history/start! history dispatch! page-exists?)
  (fn []
    (r-history/stop! history)))

(c/defn-effect push! [history uri]
  (r-history/push! history uri))

(def html5-history r-history/html5-history)

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
  (controlled-history (r-history/html5-history) inactive?))
