(ns reacl-c-basics.ajax-test-util-test
  (:require [reacl-c-basics.ajax-test-util :as ajax-tu]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.core :as tu :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest request-subscribe-effect-test
  (let [req (ajax/GET "http://invalid.invalid/url")
        sub-eff (let [r (atom nil)]
                  (tu/mount! (tu/env (-> (ajax/execute req)
                                         (tu/emulate-subscriptions (fn [sub-eff]
                                                                     (reset! r sub-eff)
                                                                     c/no-effect))))
                             nil)
                  @r)]
    (is (is (tu/subscribe-effect? sub-eff (ajax/execute req))))
    (is (ajax-tu/request-subscribe-effect? sub-eff))
    (is (ajax-tu/request-subscribe-effect? sub-eff req))
    (is (= req (ajax-tu/request-subscribe-effect-request sub-eff)))))

(deftest emulator-test
  (let [req1 (ajax/GET "http://invalid.invalid/url")
        ok-res (ajax/ok-response :result)
        env (tu/env (-> (c/dynamic #(if % (ajax/execute req1) c/empty))
                        (ajax-tu/emulate-requests {req1 ok-res})))]

    (is (= (c/return :action ok-res)
           (tu/mount! env true)))

    (is (= (c/return)
           (tu/update!! env false)))

    (tu/mount! env true)
    (is (= (c/return)
           (tu/unmount! env)))))
