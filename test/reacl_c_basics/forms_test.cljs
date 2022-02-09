(ns reacl-c-basics.forms-test
  (:require [reacl-c-basics.forms :as forms]
            [reacl-c-basics.forms.core :as fcore]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c.main :as main]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest form-test
  (let [submitted (atom nil)]
    (dt/rendering
     (forms/form {:default {:text "def"}
                  :data-testid "myform"
                  :onsubmit (fn [value] (reset! submitted value) (c/return))}
                 (c/focus :text (forms/input-string)))
     :state {:text "abc"}
     (fn [env]
       (is (some? (dt/query env (dt/by-display-value "abc"))))

       ;; reset to default works
       (dt/fire-event (dt/get env (dt/by-test-id "myform")) :reset)
       (is (some? (dt/query env (dt/by-display-value "def"))))
     
       ;; setting value
       (dt/fire-event (dt/query env (dt/by-display-value "def"))
                      :change {:target {:value "foobar"}})
       (is (some? (dt/query env (dt/by-display-value "foobar"))))

       ;; submitting
       (dt/fire-event (dt/get env (dt/by-test-id "myform")) :submit)
       (is (= @submitted {:text "foobar"}))
       ))))

(deftest input-number-test
  (let [last-state (atom nil)
        item (dom/div (forms/input-number {:type "text" ;; Note: type number makes it harder to simulate invalid input.
                                           :data-testid "inp"})
                      (dom/button {:onclick (constantly 21)} "update")
                      (c/dynamic (fn [v]
                                   (reset! last-state v)
                                   (str v))))

        current-input (fn [e]
                        (dt/query e (dt/by-test-id "inp")))
        current-text (fn [e]
                       (-> (current-input e)
                           (.-value)))

        enter-text (fn [e txt]
                     (let [comp (current-input e)]
                       (dt/fire-event comp :change {:target {:value txt}})))]

    (dt/rendering
     item
     :state 42
     ;; :visible? true
     (fn [e]
       (testing "shows initial value"
         (is (= "42" (current-text e))))

       (testing "show updated value"
         (dt/fire-event (dt/get e (dt/by-text "update")) :click)
         (is (= "21" (current-text e))))

       (testing "changes state when new valid input"
         (enter-text e "10")
         (is (= 10 @last-state)))

       (testing "state nil but keeps text on invalid input if focused"
         (dt/fire-event (current-input e) :focus)
         (enter-text e "foobar")

         (is (= "foobar" (current-text e)))
         (is (= nil @last-state)))

       (testing "then, on blur, sets the text to the unparsed text."
         (dt/fire-event (current-input e) :blur)
         (is (= "" (current-text e))))   
       ))))

(deftest select-string-test
  (testing "can take many args"
    (dt/rendering (apply forms/select-string {} (repeat 50 (forms/option {:value 42} "42")))
                  (fn [env]
                    (is true) ;; ok if it does not fail
                    ))))
