(ns reacl-c-basics.ajax-test-util-test
  (:require [reacl-c-basics.ajax-test-util :as ajax-tu]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest emulate-requests-test
  (let [req1 (ajax/GET "http://invalid.invalid/url")
        ok-res (ajax/ok-response :result)

        it (-> (c/fragment (ajax/fetch req1)
                           (c/with-state-as st
                             (cond (nil? st) "Pending"
                                   (ajax/response-ok? st) (str "Ok " (ajax/response-value st))
                                   :else "Error")))
               (ajax-tu/emulate-requests {req1 ok-res}))]

    (dt/rendering it
                  (fn [env]
                    (is (dt/get env (dt/by-text "Ok :result")))))))
