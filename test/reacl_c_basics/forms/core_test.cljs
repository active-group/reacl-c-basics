(ns reacl-c-basics.forms.core-test
  (:require [reacl-c-basics.forms.core :as core]
            [reacl-c.test-util.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

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
