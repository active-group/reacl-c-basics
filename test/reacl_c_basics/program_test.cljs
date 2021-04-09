(ns reacl-c-basics.program-test
  (:require [reacl-c-basics.program :as p :include-macros true]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dt]
            [active.clojure.lens :as lens]
            [cljs-async.core :as a :include-macros true]
            [cljs-async.test :as at :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(defn- run-sync [program & [dflt]]
  (let [result (atom dflt)]
    (dt/rendering
     (p/run
       program
       (fn [st res]
         (reset! result res)
         res))
     (fn [env]
       @result))))

(defn run-async [program & [interact dflt]]
  (let [result (atom dflt)]
    (dt/rendering
     (c/local-state false
                    (c/fragment (p/run
                                  (p/wrap (partial c/focus lens/first) program)
                                  (fn [st res]
                                    (reset! result res)
                                    [(first st) ::done]))
                                (c/dynamic (comp str second))))
     (fn [env]
       (a/async
        (when interact (a/await (interact env)))
        (a/await (dt/find env (dt/by-text (str ::done))))
        @result)))))

(deftest return-test
  (is (= 42 (run-sync (p/return 42)))))

(deftest bind-test-1
  (is (= "42"
         (run-sync
          (p/bind (p/return "4")
                  (fn [s]
                    (p/return (str s "2"))))))))

(deftest bind-test-2
  (is (= "42xy"
         (run-sync
          (p/bind (p/bind (p/return "4")
                          (fn [s]
                            (p/return (str s "2"))))
                  (fn [s]
                    (p/bind (p/return "x")
                            (fn [s2]
                              (p/return (str s s2 "y"))))))))))

(deftest do-program-test
  (is (= "42" (run-sync
               (p/do-program
                [s1 (p/return "4")]
                [s2 (p/return "2")]
                (p/return (str s1 s2)))))))

(deftest await-state-test-1
  (is (= "4" (run-sync
              (p/await-state (c/init "4")
                             #(= % "4"))))))

(at/deftest await-state-test-2
  (a/async
   (is (= "4"
          (a/await
           (run-async
            (p/await-state (dom/button {:onclick (constantly "4")} "click")
                           #(= % "4"))
            (fn [env]
              (a/async
               (dt/fire-event (dt/get env (dt/by-text "click")) :click)))))))))

(deftest await-action-test-1
  (is (= "4" (run-sync
              (p/await-action (c/init (c/return :action "4"))
                              string?)))))

(at/deftest await-action-test-2
  (a/async
   (is (= "4"
          (a/await
           (run-async
            (p/await-action (dom/button {:onclick (constantly (c/return :action "4"))} "click")
                            string?)
            (fn [env]
              (a/async
               (dt/fire-event (dt/get env (dt/by-text "click")) :click)))))))))

(deftest sequ-test-1
  (let [r1 (atom nil)
        ord (atom nil)]
    (is (= 21
           (run-sync
            (p/sequ (p/bind (p/return 42) (fn [r]
                                            (reset! ord :p1)
                                            (reset! r1 r)
                                            (p/return nil)))
                    (p/bind (p/return nil) (fn [r]
                                             (reset! ord :p2)
                                             (p/return 21)))))))
    ;; p1 did run, but p2 came after it
    (is (= 42 @r1))
    (is (= :p2 @ord))))

(deftest sequ-test-2
  ;; can run the same program twice
  (let [cnt (atom 0)
        foo (c/effect (fn [] (swap! cnt inc)))
        p (p/bind (p/return nil)
                  (fn [r]
                    (p/wrap (fn [item]
                              (c/fragment item
                                          (c/init (c/return :action foo))))
                            (p/return 42))))]
    (is (= 42
           (run-sync (p/sequ p p))))
    (is (= 2 @cnt))))

(deftest par-test-1
  (is (= [:a :b]
         (run-sync (p/par (p/return :a)
                          (p/return :b))))))

(deftest race-test-1
  ;; TODO: more elaborated tests
  (is (#{:a :b}
       (run-sync (p/race (p/return :a)
                         (p/return :b)))))
  (is (= :b
         (run-sync (p/race (p/block "Foo")
                           (p/return :b))))))


(deftest run-button-test
  (let [run? (atom false)]
    (dt/rendering
     (p/run-button
      {:program (p/bind (p/return "42")
                        (fn [v]
                          (reset! run? true)
                          (p/return nil)))}
      "click me")
     (fn [env]
       (let [btn (dt/get env (dt/by-text "click me"))]
         (is (some? btn))
         (dt/fire-event btn :click))

       (is @run?)))))

(deftest referntial-equality-test
  (is (= (p/return 42) (p/return 42)))
  
  (let [p (p/return 42)
        k (constantly (p/return 0))]
    (is (= (p/bind p k) (p/bind p k))))

  (let [p1 (p/return 42)
        p2 (p/return 21)]
    (is (= (p/sequ p1 p2) (p/sequ p1 p2))))

  (let [p1 (p/return 42)
        p2 (p/return 21)]
    (is (= (p/par p1 p2) (p/par p1 p2))))

  (let [p1 (p/return 42)
        p2 (p/return 21)]
    (is (= (p/race p1 p2) (p/race p1 p2))))

  (let [p (p/return 42)]
    (is (= (p/fmap inc p) (p/fmap inc p))))

  (let [p (p/return 42)
        f (partial dom/div)]
    (is (= (p/wrap f p) (p/wrap f p))))

  (let [p (p/return 42)]
    (is (= (p/block p) (p/block p))))

  (is (= (p/await-action c/empty empty?) (p/await-action c/empty empty?)))

  (is (= (p/await-state c/empty :foo) (p/await-state c/empty :foo))))
