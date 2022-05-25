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
     (fn [attrs msg]
       ;; Note: attrs can put attrs in a div.
       (dom/div attrs
                (non-empty-input)
                (when msg (dom/div (str "Msg: " msg))))))
   :state "test"
   (fn [env]
     (let [input (dt/get env (dt/by-display-value "test"))]
       (dt/set-state! env "")
       (.reportValidity input) ;; can be an input
       (is (some? (dt/query env (dt/by-text "Msg: Must not be empty"))))))))

(deftest with-validity-test-2
  (dt/rendering
   (fval/with-validity
     (fn [attrs msg]
       ;; Note: attrs can put attrs in an input directly.
       (c/with-state-as state
         (dom/form {:data-testid "foo"}
                   (non-empty-input attrs)
                   (when msg (dom/div (str "Msg: " msg)))))))
   :state ""
   (fn [env]
     (let [form (dt/get env (dt/by-test-id "foo"))]
       (.reportValidity form) ;; can be a form
       (is (some? (dt/query env (dt/by-text "Msg: Must not be empty"))))))))

(deftest report-validity-test
  (dt/rendering
   (fval/form-with-validity {:report-validity true}
                            (fn [msg reset-action]
                              (c/fragment
                               (non-empty-input {})
                               (c/with-state-as txt
                                 (when-not (empty? txt)
                                   (c/init (c/return :action reset-action))))
                               (when msg (dom/div (str "Msg: " msg))))))
   :state ""
   (fn [env]
     (is (some? (dt/query env (dt/by-text "Msg: Must not be empty"))))

     (dt/set-state! env "foo")
     (is (nil? (dt/query env (dt/by-text "Msg: Must not be empty"))))
     )))

(deftest append-validity-test
  (dt/rendering
   (fval/append-validity (non-empty-input {:data-testid "foo"})
                         (fn [msg]
                           (when msg (dom/div (str "Msg: " msg)))))
   :state ""
   (fn [env]
     (let [input (dt/get env (dt/by-test-id "foo"))]
       (.reportValidity input)
       (is (some? (dt/query env (dt/by-text "Msg: Must not be empty"))))))))
