(ns reacl-c-basics.forms.core-test
  (:require [reacl-c-basics.forms.core :as core]
            [reacl-c.test-util.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]
            ["@testing-library/user-event$default" :as userEvent]))

(defn state-changes-text [env state & [text]]
  (dt/set-state! env state)
  (some? (dt/query env (dt/by-display-value (or text state)))))

(defn state-changes-text! [env state & [text]]
  (dt/set-state! env state)
  ;; throws if not found.
  (dt/get env (dt/by-display-value (or text state))))

(defn text-changes-state
  ([env node text state]
   (dt/fire-event node
                  :change {:target {:value text}})
   (= state (dt/current-state env)))
  
  ([env node text]
   (text-changes-state env node text text)))

(defn selected-options-changes-state [env node texts state]
  ;; Note: no public api to create a HTMLOptionsCollection; need to mutate in place.
  ;; Also note: https://github.com/testing-library/react-testing-library/issues/375
  (assert (.-multiple node))

  ;; Seems this requires the 'userEvent' lib
  ;; https://github.com/testing-library/user-event/pull/130
  (if (empty? texts)
    (do
      ;; seems 'selectOptions []' just does not change the selections *aahhhgl*
      ;; deselectOptions not yet available in our version
      #_(.deselectOptions userEvent node (-> (js/Array.from (.-options node))
                                             (.map (fn [o] (.-value o)))))
      (dt/fire-event node :change {:target {:value ""}}))
      
    (.selectOptions userEvent node (to-array texts)))
  
  (= state (dt/current-state env)))

(deftest input-test
  (dt/rendering
   (core/input {:type "text"})
   :state "foo"
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "foo"))]
       (is (state-changes-text env "bar"))
       (is (text-changes-state env node "baz"))))))

(deftest select-test
  (let [foo [:foo]
        bar {:bar true}
        foo-bar-select (fn [attrs & more]
                         (apply core/select
                                attrs
                                (core/option {:value foo :placeholder "xxx"} "foo")
                                (core/option {:value bar} "bar")
                                more))
        ]
    (testing "single"
      (dt/rendering
       (foo-bar-select {} (core/option {:value nil} "none"))
       :state foo
       (fn [env]
         (let [node (dt/get env (dt/by-display-value "foo"))]
           (is (state-changes-text env bar "bar"))
           (is (state-changes-text env nil "none"))
         
           ;; Note: onChange still 'needs' the placeholder value, not the actual value.
           (is (text-changes-state env node "xxx" foo))
           (is (text-changes-state env node "" nil))
           ))))
    (testing "multiple"
      (dt/rendering
       (foo-bar-select {:multiple true})
       :state (list foo)
       (fn [env]
         (let [node (dt/get env (dt/by-display-value "foo"))]
           (is (state-changes-text env (list bar) "bar"))
           ;; selecting 'nothing' has a different semantics now:
           (is (not (state-changes-text env (list) "bar")))
           (is (not (state-changes-text env nil "bar")))

           ;; Note: onChange still 'needs' the placeholder list, not the actual value.
           (is (selected-options-changes-state env node (list "xxx") (list foo)))
           
           (is (selected-options-changes-state env node (list "xxx" "{:bar true}") (list foo bar)))

           (is (selected-options-changes-state env node (list) nil)))))
      )
    ))

(deftest select-multiple-test
  (let [foo [:foo]
        bar {:bar true}]
    (dt/rendering
     (core/select
      (core/option {:value nil} "none")
      (core/option {:value foo :placeholder "xxx"} "foo")
      (core/option {:value bar} "bar"))
     :state foo
     (fn [env]
       (let [node (dt/get env (dt/by-display-value "foo"))]
         (is (state-changes-text env bar "bar"))
         (is (state-changes-text env nil "none"))
         
         ;; Note: onChange still 'needs' the placeholder value, not the actual value.
         (is (text-changes-state env node "xxx" foo))
         (is (text-changes-state env node "" nil))
         )))))

(deftest validate-test
  (dt/rendering
   (core/input {:type "text"
                :validate (fn [v] (when (not= v "foo")
                                    "Not foo"))})
   :state "bar"
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "bar"))]
       (.reportValidity node)
       (is (= "Not foo" (.-validationMessage node)))
       
       (dt/set-state! env "foo")
       (.reportValidity node)
       (is (= "" (.-validationMessage node)))))))
