(ns reacl-c-basics.forms-test
  (:require [reacl-c-basics.forms :as forms]
            [reacl-c.core :as c]
            [reacl-c.test-util.core :as tu]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest form-test
  (let [e (tu/env (forms/form {:default {:text ""}
                               :onsubmit (fn [value] (c/return :action [:submit! value]))}
                              (-> (forms/input-string)
                                  (c/focus :text))))]
    (tu/mount! e {:text "abc"})

    ;; reset to default works
    (is (= (c/return :state {:text ""})
           (tu/invoke-callback! (xpath/select (tu/get-component e) (xpath/>> ** "form"))
                                :onreset #js {:type "reset"})))

    ;; setting value
    (is (= (c/return :state {:text "foobar"})
           (tu/invoke-callback! (xpath/select (tu/get-component e) (xpath/>> ** "input"))
                                :onchange #js {:type "change"
                                               :target #js {:value "foobar"}})))

    ;; submitting
    (tu/update! e {:text "foobar"})
    (is (= (c/return :action [:submit! {:text "foobar"}])
           (tu/invoke-callback! (xpath/select (tu/get-component e) (xpath/>> ** "form"))
                                :onsubmit #js {:type "submit"
                                               :preventDefault (fn [] nil)}))))

  )
