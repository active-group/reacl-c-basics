(ns reacl-c-basics.forms.types-test
  (:require [reacl-c-basics.forms.core-test :as ct :refer [text-changes-state state-changes-text]]
            [reacl-c-basics.forms.core :as core]
            [reacl-c-basics.forms.types :as types]
            [reacl-c.test-util.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest string-test
  (dt/rendering
   (core/input {:type types/string})
   :state "foo"
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "foo"))]
       (is (state-changes-text env "bar"))
       (is (text-changes-state env node "baz"))))))

(deftest opt-string-test
  (dt/rendering
   (core/input {:type types/opt-string})
   :state "foo"
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "foo"))]
       (is (state-changes-text env "bar"))
       (is (state-changes-text env nil ""))
       
       (is (text-changes-state env node "baz"))

       (is (text-changes-state env node "" nil))))))

(deftest number-test
  (dt/rendering
   (core/input {:type types/number})
   :state 12.3
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "12.3"))]
       (is (state-changes-text env 2 "2"))
       (is (text-changes-state env node "1.2" 1.2))))))

(deftest integer-test
  (dt/rendering
   (core/input {:type types/integer})
   :state 12
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "12"))]
       (is (state-changes-text env 2 "2"))
       ;; non-int input truncated
       (is (text-changes-state env node "1.2" 1))))))

(deftest strict-integer-test
  (dt/rendering
   (core/input {:type types/strict-integer})
   :state 12
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "12"))]
       ;; non-int input dropped
       (is (text-changes-state env node "1.2" 12))))))

(deftest fixnum-test
  (dt/rendering
   (core/input {:type (types/fixnum 2)})
   :state 12
   (fn [env]
     (let [node (dt/get env (dt/by-display-value "12.00"))]
       (is (state-changes-text env 2.1 "2.10"))
       (is (text-changes-state env node "1.201" 1.2))))))

(deftest enum-test
  (let [foo :foo
        bar {:bar true}]
    (dt/rendering
     (core/input {:type (types/enum [[foo "foo"] [bar "bar"]])})
     :state foo
     (fn [env]
       (let [node (dt/get env (dt/by-display-value "foo"))]
         (is (state-changes-text env bar "bar"))
         ;; Note: 'onChange' to internal text value placeholder
         (is (text-changes-state env node (pr-str foo) foo)))))))

(deftest optional-enum-test
  (let [foo :foo
        bar {:bar true}]
    (dt/rendering
     (core/input {:type (types/optional (types/enum [[foo "foo"] [bar "bar"]]))})
     :state foo
     (fn [env]
       (let [node (dt/get env (dt/by-display-value "foo"))]
         (is (state-changes-text env nil ""))
         (is (state-changes-text env bar "bar"))
         ;; Note: 'onChange' to internal text value placeholder
         (is (text-changes-state env node "" nil)))))))
