(ns reacl-c-basics.forms-test
  (:require [reacl-c-basics.forms :as forms]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu]
            [cljs.test :refer (is deftest testing) :include-macros true]))

(deftest form-test
  (let [e (tu/env (forms/form {:default {:text ""}
                               :onsubmit (fn [value] (c/return :action [:submit! value]))}
                              (c/focus :text (forms/input-string))))]
    (tu/mount! e {:text "abc"})

    ;; reset to default works
    (is (= (c/return :state {:text ""})
           (tu/invoke-callback! (tu/find e (dom/form))
                                :onreset #js {:type "reset"})))

    ;; setting value
    (is (= (c/return :state {:text "foobar"})
           (tu/invoke-callback! (tu/find e (dom/input))
                                :onchange #js {:type "change"
                                               :target #js {:value "foobar"}})))

    ;; submitting
    (tu/update! e {:text "foobar"})
    (is (= (c/return :action [:submit! {:text "foobar"}])
           (tu/invoke-callback! (tu/find e (dom/form))
                                :onsubmit #js {:type "submit"
                                               :preventDefault (fn [] nil)}))))

  )

(deftest input-number-test
  (let [e (tu/env (forms/input-number))

        current-input (fn []
                        (tu/find e (dom/input)))
        current-text (fn []
                       (-> (current-input)
                           (.-props)
                           (.-value)))

        enter-text (fn [txt]
                     (let [comp (current-input)]
                       (tu/invoke-callback! comp :onchange
                                            #js {:type "change" :target #js {:value txt}})))]

    (testing "shows initial value"
      (is (= (c/return)
             (tu/mount! e 42)))
      (is (= "42" (current-text))))

    (testing "show updated value"
      (is (= (c/return)
             (tu/update! e 21)))
      (is (= "21" (current-text))))

    (testing "returns value when new valid input"
      (is (= (c/return :state 10)
             (enter-text "10"))))

    (testing "returns nil but keeps text on invalid input"
      (is (= (c/return :state nil)
             (enter-text "foobar")))
      (tu/update! e nil)
      (is (= "foobar" (current-text))))))
