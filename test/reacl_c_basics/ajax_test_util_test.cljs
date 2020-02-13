(ns reacl-c-basics.ajax-test-util-test
  (:require [reacl-c-basics.ajax-test-util :as ajax-tu]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.test-util.core :as tu :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest emulator-test
  (let [req1 (ajax/GET "http://invalid.invalid/url")
        ok-res (ajax/ok-response :result)
        env (tu/env (c/dynamic #(if % (ajax/execute req1) c/empty))
                    {:emulator
                     (ajax-tu/requests-emulator {req1 ok-res})})]

    (is (= (c/return :action ok-res)
           (tu/mount! env true)))

    (is (= (c/return)
           (tu/update!! env false)))

    (tu/mount! env true)
    (is (= (c/return)
           (tu/unmount! env)))))
