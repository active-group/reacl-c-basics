(ns reacl-c-basics.programs.core-test
  (:require [reacl-c-basics.programs.core :as p :include-macros true]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dt]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]
            [cljs-async.core :as a :include-macros true]
            [cljs-async.test :as at :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(at/deftest runner-test
  (a/async
   (a/await
    (testing "should run programs only once"
      (let [result (atom [])]
        (dt/rendering
         (c/isolate-state 0
                          (c/fragment
                           (dom/button {:onClick (fn [c]
                                                   (inc c))}
                                       "click")
                           (p/runner
                            ;; TODO: need to test it for all primitives?
                            #_(p/return 42)
                            #_(p/then (p/return 21) (fn [x] (p/return (* 2 x))))
                            (p/race (p/show "xxx") (p/then (p/return 21) (fn [x] (p/return (* 2 x)))))
                            (fn [st res]
                              (swap! result conj res)
                              st))))
       
         (fn [env]
           (a/async
            (is (= [42] @result))
            ;; the click does not run it again
            (dt/fire-event (dt/get env (dt/by-text "click")) :click)
            (is (= [42] @result))))))))

   (a/await
    (testing "does not change state when unmounted"
      (let [changes (atom [])]
        (dt/rendering
         (c/isolate-state 0
                          (c/dynamic
                           (fn [res]
                             (-> (if (zero? res)
                                   (p/runner
                                    ;; TODO: need to test it for all primitives?
                                    #_(p/return 42)
                                    #_(p/race (p/show "42") (p/then (p/return 21) (fn [x] (p/return (* 2 x)))))
                                    #_(p/await-state #(c/init 42))
                                    (p/race (p/show "42") (p/await-state #(when % (c/init 42))))
                                    #_(p/await-action #(c/init (c/return :action 42))
                                                      int?)
                                    (fn [st res]
                                      (assert (= res 42))
                                      res))
                                   "done")
                                  
                                 (c/monitor-state
                                  (fn [prev next]
                                    (swap! changes conj next)))))))
       
         (fn [env]
           (a/async
            (a/await (dt/find env (dt/by-text "done")))
            (a/await (a/timeout 100))
            (is (= [42] @changes))
            ))))))))

(defn- run-sync [program & [dflt]]
  (let [result (atom dflt)]
    (dt/rendering
     (p/runner
       program
       (fn [st res]
         (reset! result res)
         res))
     (fn [env]
       @result))))

(defn run-async [program & [interact dflt tmo-ms]]
  (let [result (atom dflt)]
    (dt/rendering
     (c/local-state false
                    (c/fragment (p/runner
                                  (p/wrap (partial c/focus lens/first) program)
                                  (fn [st res]
                                    (reset! result res)
                                    [(first st) ::done]))
                                (c/dynamic (comp str second))))
     (fn [env]
       (a/async
        (when interact (a/await (interact env)))
        (a/await (apply dt/find env (dt/by-text (str ::done))
                        (when tmo-ms
                          [:timeout tmo-ms])))
        @result)))))

(deftest return-test
  (is (= 42 (run-sync (p/return 42))))
  (is (= nil (run-sync (p/return nil) :init))))

(deftest then-test-1
  (is (= "42"
         (run-sync
          (p/then (p/return "4")
                  (fn [s]
                    (p/return (str s "2"))))))))

(deftest then-test-2
  (is (= "42xy"
         (run-sync
          (p/then (p/then (p/return "4")
                          (fn [s]
                            (p/return (str s "2"))))
                  (fn [s]
                    (p/then (p/return "x")
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
              (p/await-state #(when % (c/init "4"))
                             #(= % "4"))))))

(at/deftest await-state-test-2
  (a/async
   (is (= "4"
          (a/await
           (run-async
            (p/await-state (fn [running?]
                             (dom/button {:onclick (constantly "4") :disabled (not running?)} "click"))
                           #(= % "4"))
            (fn [env]
              (a/async
               (dt/fire-event (dt/get env (dt/by-text "click")) :click)))))))))

(deftest await-action-test-1
  (is (= "4" (run-sync
              (p/await-action #(when % (c/init (c/return :action "4")))
                              string?)))))

(at/deftest await-action-test-2
  (a/async
   (is (= "4"
          (a/await
           (run-async
            (p/await-action (fn [running?]
                              (dom/button {:onclick (constantly (c/return :action "4")) :disabled (not running?)} "click"))
                            string?)
            (fn [env]
              (a/async
               (dt/fire-event (dt/get env (dt/by-text "click")) :click)))))))))

(deftest sequ-test-1
  (let [r1 (atom nil)
        ord (atom [])]
    (is (= 21
           (run-sync
            (p/sequ (p/then (p/return 42) (fn [r]
                                            (swap! ord conj :p1)
                                            (reset! r1 r)
                                            (p/return nil)))
                    (p/then (p/return nil) (fn [r]
                                             (swap! ord conj :p2)
                                             (p/return 21)))))))
    ;; p1 did run, but p2 came after it
    (is (= 42 @r1))
    ;; Note: continuation are allowed to called more than once, currently.
    (is (= [:p1 :p2] (distinct @ord)))))

(deftest sequ-test-2
  ;; can run the same program twice
  (let [cnt (atom 0)
        foo (c/effect (fn [] (swap! cnt inc)))
        p (p/then (p/return nil)
                  (fn [r]
                    (p/wrap (fn [item]
                              (c/fragment item (c/init (c/return :action foo))))
                            (p/return 42))))]
    (is (= 42
           (run-sync (p/sequ p p p))))
    (is (= 3 @cnt))))

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
         (run-sync (p/race (p/show "Foo")
                           (p/return :b)))))
  (is (= :a
         (run-sync (p/race (p/return :a)
                           (p/show "Foo"))))))


(deftest referntial-equality-test
  (is (= (p/return 42) (p/return 42)))
  
  (let [p (p/return 42)
        k (constantly (p/return 0))]
    (is (= (p/then p k) (p/then p k))))

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

  (is (= (p/show "42") (p/show "42")))

  (is (= (p/await-action (f/constantly c/empty) empty?) (p/await-action (f/constantly c/empty) empty?)))

  (is (= (p/await-state (f/constantly c/empty) :foo) (p/await-state (f/constantly c/empty) :foo))))

(let [eh (fn [ev]
           (.preventDefault ev))]
  (defn preventing-error-log-async [f]
    (js/window.addEventListener "error" eh)
    (a/finally (f)
               (fn []
                 (js/window.removeEventListener "error" eh)))))

(let [eh (fn [ev]
           (.preventDefault ev))
      inst (c/effect (fn []
                       (js/window.addEventListener "error" eh)))
      uninst (c/effect (fn []
                         (js/window.removeEventListener "error" eh)))]
  (defn silenced-unhandled-errors [item]
    item
    ;; unfortunately, this breaks something in the tail-rec test... :-/
    #_(c/local-state false
                   (c/fragment
                    (c/dynamic (fn [[_ t]]
                                 (if t (c/focus lens/first item) c/empty)))
                    (c/focus lens/second
                             (c/once (f/constantly (c/return :action inst
                                                             :state true))
                                     (f/constantly (c/return :action uninst))))))))

(defn throw-once []
  (silenced-unhandled-errors
   (c/local-state true
                  (-> (c/dynamic (fn [[_ do?]]
                                   (if do?
                                     (throw (js/Error. "IGNORE: Intended test error"))
                                     c/empty)))
                      (c/handle-error (fn [[st _]]
                                        [st false]))))))

(c/defn-subscription capturing-console-error deliver! [f]
  (let [orig (.-error js/console)]
    (set! (.-error js/console) (fn [& args]
                                 (when-let [a (f args orig)]
                                   (deliver! a))))
    (deliver! ::installed)
    (fn []
      (set! (.-error js/console) orig))))

(defn with-capturing-console-error [item f]
  (c/local-state false
                 (c/fragment
                  (c/focus lens/second
                           (-> (capturing-console-error f)
                               (c/handle-action (fn [state a]
                                                  (if (= a ::installed)
                                                    (c/return :state true)
                                                    (c/return :action a))))))
                  (c/dynamic (fn [[_ installed?]]
                               (when installed?
                                 item))))))

(defn current-component-stack []
  ;; an item that emits the current component stack as an action.
  
  ;; this is quite hacky, and might well fail in future React
  ;; versions: We try to capture the console error that react prints
  ;; when an exception is thrown, which lists the component hierarchy.
  (with-capturing-console-error
    (throw-once)
    (fn [args original]
      (if (and (string? (first args))
               (.startsWith (first args) "The above error occurred in"))
        (first args)
        (do 
          (apply original args)
          nil)))))

(defn with-size-of-component-structure [f]
  (let [top (atom nil)
        max-size (atom 0)]
    (p/wrap (fn [item]
              (c/with-ref (fn [ref]
                            (reset! top ref)
                            (c/refer item ref))))
            (f (fn [_]
                 (p/fmap count
                         (p/await-action #(current-component-stack)
                                         string?)))))))

(at/deftest test-tail-recursiveness-test
  (let [size-of (fn [N]
                  (run-async (with-size-of-component-structure
                               (fn [k]
                                 (apply p/sequ (concat (repeat N (p/return 0))
                                                       [(p/then (p/return nil) k)]))))
                             
                             nil :init 1000))]
    (a/async
     ;; to check the above technique seems to be working:
     (is (not= 0 (a/await (size-of 1))))
     (is (= (a/await (size-of 2)) (a/await (size-of 2)))))))

(at/deftest sequ-tail-recursiveness
  (let [size-of (fn [N]
                  (run-async (with-size-of-component-structure
                               (fn [k]
                                 (apply p/sequ (concat (repeat N (p/return 0))
                                                       [(p/then (p/return nil) k)]))))
                             
                             nil :init 1000))]
    #_(preventing-error-log-async
     (fn []
       ))
    (a/async
     (is (not (< (a/await (size-of 2))
                 (a/await (size-of 4))
                 (a/await (size-of 8))
                 (a/await (size-of 16))))))
    ))

(at/deftest trampoline-tail-recursiveness
  (let [p (fn p [n]
            (if (> n 0)
              (p/then (p/return nil) (fn [_] (p/return (p (dec n)))))
              (p/return nil)))
        size-of (fn [N]
                  (run-async (with-size-of-component-structure
                               (fn [k]
                                 (p/trampoline
                                  (p/return (p N)))))
                             
                             nil :init 1000))]
    (a/async
     (is (not (< (a/await (size-of 2))
                 (a/await (size-of 4))
                 (a/await (size-of 8))
                 (a/await (size-of 16))))))
    ))
