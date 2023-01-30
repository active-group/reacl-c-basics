(ns reacl-c-basics.forms.bootstrap-test
  (:require [reacl-c-basics.forms.bootstrap :as forms-bs]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.lens :as lens]
            [reacl-c.test-util.dom-testing :as dt]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

;; Note: we cannot really test the styling that bootstrap adds.

(deftest validated-form-test
  (dt/rendering
   (forms-bs/validated-form* {:data-testid "foo"
                              :onSubmit (fn [state ev]
                                          (.preventDefault ev)
                                          (c/return :state [[true (second (first state))] (second state)]))}
                             (dom/input {:type "submit" :data-testid "submit"})
                             (c/focus (lens/>> lens/first lens/second)
                                      (forms-bs/input {:type "text" :required true})))
   :state [[false ""] false]
   (fn [env]
     
     (let [form (dt/get env (dt/by-testid "foo"))
           btn (dt/get env (dt/by-testid "submit"))]
       (assert (some? form))
       ;; Note: (.submit form) seems to ignore :onSubmit, or preventDefault. Need to click the button.

       (do
         (dt/fire-event btn :click)
         (is (= [[false ""] true] (dt/current-state env)) "did not submit"))
       
       (do
         (dt/set-state! env [[false "foo"] false])
         (dt/fire-event btn :click)
         (is (= [[true "foo"] false] (dt/current-state env)) "did submit"))))))
