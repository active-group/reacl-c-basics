(ns reacl-c-basics.forms.validation-test
  (:require [reacl-c-basics.forms.core :as core]
            [reacl-c-basics.forms.validation :as fval]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(dom/def-dom non-empty-input core/input
  {:validate (fn [s] (if (empty? s) "Must not be empty" ""))})

(deftest with-validity-test-1
  (dt/rendering
   (fval/with-validity
     (fn [msg oninvalid]
       ;; Note: can put attrs in a div.
       (dom/div {:onInvalid oninvalid}
                (non-empty-input)
                (when msg (dom/div (str "Msg: " msg))))))
   :state "test"
   (fn [env]
     (let [input (dt/get env (dt/by-display-value "test"))]
       (dt/set-state! env "")
       (.reportValidity input) ;; can be an input
       (is (some? (dt/find env (dt/by-text "Msg: Must not be empty"))))))))

(deftest with-validity-test-2
  (dt/rendering
   (fval/with-validity
     (fn [msg oninvalid]
       ;; Note: can put attrs in an input directly.
       (c/with-state-as state
         (dom/form {:data-testid "foo"}
                   (non-empty-input {:onInvalid oninvalid})
                   (when msg (dom/div (str "Msg: " msg)))))))
   :state ""
   (fn [env]
     (let [form (dt/get env (dt/by-test-id "foo"))]
       (.reportValidity form) ;; can be a form
       (is (some? (dt/find env (dt/by-text "Msg: Must not be empty"))))))))

(deftest append-validity-test
  (dt/rendering
   (fval/append-validity (non-empty-input {:data-testid "foo"})
                         (fn [msg]
                           (when msg (dom/div (str "Msg: " msg)))))
   :state ""
   (fn [env]
     (let [input (dt/get env (dt/by-test-id "foo"))]
       (.reportValidity input)
       (is (some? (dt/find env (dt/by-text "Msg: Must not be empty"))))))))
