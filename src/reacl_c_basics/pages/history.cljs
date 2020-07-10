(ns reacl-c-basics.pages.history
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

