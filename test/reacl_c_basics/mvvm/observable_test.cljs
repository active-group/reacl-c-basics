(ns reacl-c-basics.mvvm.observable-test
  (:require [reacl-c-basics.mvvm.observable :as sut]
            [cljs.test :as t :include-macros true]))

(t/deftest observable-sync-test
  (let [o (sut/observable (fn [subscriber]
                            (.next subscriber 42)
                            (.next subscriber 12)
                            (.complete subscriber)
                            nil))
        values (atom [])
        done (atom false)]
    (let [sub (sut/subscribe! o
                              (sut/observer (fn [v]
                                              (swap! values conj v))
                                            (fn [err]
                                              (assert false err))
                                            (fn []
                                              (reset! done true))))]
      (t/is (sut/subscription-closed? sub))
      (t/is (= [42 12] @values))
      (t/is @done))))

(t/deftest observable-async-test
  (t/async done
           (let [o (sut/observable (fn [subscriber]
                                     (let [id (js/window.setTimeout (fn []
                                                                      (.next subscriber 42)
                                                                      (.next subscriber 12)
                                                                      (.complete subscriber))
                                                                    10)]
                                       (fn []
                                         (js/window.clearTimeout id)))))
                 values (atom [])
                 done? (atom false)]
             (let [sub (sut/subscribe! o
                                       (sut/observer (fn [v]
                                                       (swap! values conj v))
                                                     (fn [err]
                                                       (assert false err))
                                                     (fn []
                                                       (reset! done? true))))]
               (js/window.setTimeout (fn []
                                       (t/is (= [42 12] @values))
                                       (t/is (sut/subscription-closed? sub))
                                       (t/is @done?)
                                       (done))
                                     20)))))

(t/deftest unsubscribe-test
  (t/async done
           (let [cleared? (atom false)
                 o (sut/observable (fn [subscriber]
                                     (let [id (js/window.setTimeout (fn []
                                                                      (.next subscriber 42))
                                                                    10)]
                                       (fn []
                                         (reset! cleared? true)
                                         (js/window.clearTimeout id)))))
                 values (atom [])]
             (let [sub (sut/subscribe! o
                                       (sut/observer (fn [v]
                                                       (swap! values conj v))
                                                     (fn [err]
                                                       (assert false err))
                                                     (fn []
                                                       (assert false))))]
               (t/is (not (sut/subscription-closed? sub)))
               (sut/unsubscribe! sub)
               (t/is (sut/subscription-closed? sub))
               (t/is @cleared?)
               ;; won't get any values no more.
               (js/window.setTimeout (fn []
                                       (t/is (= [] @values))
                                       (done))
                                     20)))))
