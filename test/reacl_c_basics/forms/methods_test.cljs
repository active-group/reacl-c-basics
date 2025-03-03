(ns reacl-c-basics.forms.methods-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c-basics.ajax-test-util :as ajax-test-util]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c.test-util.core :as tuc :include-macros true]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c-basics.forms.core :as forms]
            [reacl-c-basics.forms.methods :as forms-methods]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest ajax-method-test
  ;; this very much also tests the subscription method.
  
  (let [post (fn [v] (ajax/POST "http://invalid.invalid/" {:params v}))]
    (dt/rendering
     (-> (forms/form {:data-testid "form-test-form"
                      :method (forms-methods/ajax post)}
                     (c/focus :foo (forms/input {:data-testid "input" :type "text"}))
                     (dom/button {:data-testid "submit" :type "submit"}))

         (ajax-test-util/emulate-requests
          {(post {:foo "baz"}) (ajax/make-response true :ok)}))
     :state {:foo "bar"}
     (fn [env]
       (let [input (dt/get env (dt/by-testid "input"))
             submit-btn (dt/get env (dt/by-testid "submit"))]

         ;; change value to baz
         (dt/fire-event input
                        :change {:target {:value "baz"}})

         ;; submit (with ok response)
         (dt/fire-event submit-btn :click)

         ;; state changed
         (is (= {:foo "baz"} (dt/current-state env)))))))

  (testing "onSubmit can change the value to be submitted"
    (let [post (fn [v] (ajax/POST "http://invalid.invalid/" {:params v}))
          requests (atom [])
          completed (atom nil)]
      (dt/rendering
       (-> (forms/form {:data-testid "form"
                        :method (forms-methods/ajax post)
                        :onComplete (fn [state result]
                                      (reset! completed result)
                                      (c/return))
                        :onSubmit (fn [state ev]
                                    (c/return :state (update state :foo str "zinga")))}
                       (c/focus :foo (forms/input {:data-testid "input" :type "text"}))
                       (dom/button {:data-testid "submit" :type "submit"}))
           (ajax-test-util/emulate-requests
            (fn [req]
              (swap! requests conj req)
              ({(post {:foo "bazzinga"}) (ajax/make-response true :ok)}
               req))))
       :state {:foo "bar"}
       (fn [env]
         (let [input (dt/get env (dt/by-testid "input"))
               submit-btn (dt/get env (dt/by-testid "submit"))]

           ;; change value to baz and submit
           (dt/fire-event input
                          :change {:target {:value "baz"}})
           (dt/fire-event submit-btn :click)

           ;; state changed, and request was made
           ;; ideally the request before the state change in :onSubmit isn't even tried;
           (is (= [(post {:foo "bazzinga"})] @requests))

           (is (= (ajax/make-response true :ok) @completed))

           (is (= {:foo "bazzinga"} (dt/current-state env)))))))))
