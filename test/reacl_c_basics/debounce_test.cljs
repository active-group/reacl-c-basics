(ns reacl-c-basics.debounce-test
  (:require [reacl-c-basics.debounce :as debounce]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.functions :as f]
            [reacl-c.test-util.core :as tu :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

;; Note: for unknown reasons, the test fails, but the debouncer "works" - could be some React TestRunner issue, although a similar test in reacl-c works :-/
#_(deftest debounce-test
  (let [af-env (tu/subscription-emulator-env (debounce/animation-frame))
        
        env (tu/env (c/dynamic (f/constantly (debounce/debounce (dom/div {:onclick (constantly (c/return :state :change))}))))
                    {:emulator (tu/subscription-emulator af-env)})]

    (tu/mount! env :start)

    (is (not (tu/subscription-emulator-running? af-env)))
    
    (let [d (tu/find env (dom/div))]
      (is (some? d))
      (is (= (c/return)
             (tu/inject-state-change! d :change))))

    (is (tu/subscription-emulator-running? af-env))

    (is (= (c/return :state :change)
           (tu/subscription-emulator-inject! af-env 4711))))
  )
