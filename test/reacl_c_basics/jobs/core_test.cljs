(ns reacl-c-basics.jobs.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.jobs.core :as jobs]
            [reacl-c.test-util.dom-testing :as dt]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(c/defn-subscription async! deliver! [resp req]
  (let [id (js/setTimeout #(deliver! resp) 0)]
    (fn []
      (js/clearTimeout id))))

(c/defn-subscription sync! deliver! [resp req]
  (deliver! resp)
  (fn [] nil))

(defn run-with [options return f]
  (dt/rendering
   (jobs/handle-jobs (dom/button {:data-testid "btn"
                                  :onclick (fn [_ _]
                                             return)})
                     options)
   (fn [env]
     (dt/fire-event (dt/get env (dt/by-testid "btn")) :click)
     (f env))))

(deftest handle-jobs-test
  ;; basic functionality
  (async done
         (-> (let [req  :my-job
                   resp :my-result
                   result (atom nil)
                   states (atom [])]
               (run-with {:execute (partial async! resp)
                          :transition
                          (fn transition [state job]
                            (is (= (jobs/job-description job) :my-job))
                            (is (= (jobs/job-info job) :my-info))
                                       
                            (swap! states conj (jobs/job-status job))
                            (when (= :completed (jobs/job-status job))
                              (reset! result (jobs/job-result job)))
                            (c/return))}
                         
                         (c/return :action (jobs/start-job req :my-info))
                         
                         (fn [env]
                           (is (= [:pending :running] @states))

                           ;; getting completed state/response, asynchronously.
                           (new js/Promise.
                                (fn [resolve reject]
                                  (js/setTimeout (fn []
                                                   (is (= [:pending :running :completed] @states))
                                                   (is (= resp @result))
                                                   (resolve :done))
                                                 1))))))
             (.then (fn [_] (done))))))

(deftest handle-jobs-manage-test
  ;; :manage can remove things from the queue.
  (async done
         (-> (let [req  :my-job
                   resp :my-result
                   completed (atom 0)
                   last-queue (atom nil)]
               (run-with {:execute (partial async! resp)
                          :transition (fn transition [state job]
                                        (when (= :completed (jobs/job-status job))
                                          (swap! completed inc))
                                        (c/return))
                          :manage (fn manage [queue]
                                    (let [q (if (empty? queue)
                                              queue
                                              [(last queue)])]
                                      (reset! last-queue q)
                                      q))}
                         
                         (c/return :action (jobs/start-job req)
                                   :action (jobs/start-job req)
                                   :action (jobs/start-job req))

                         (fn [env]
                           ;; getting completed state/response, asynchronously.
                           (new js/Promise
                                (fn [resolve reject]
                                  (js/setTimeout (fn []
                                                   (is (= 1 @completed))
                                                   (is (empty? @last-queue))
                                                   (resolve))
                                                 1))))))
             (.then (fn [_] (done))))))

(deftest handle-jobs-parallelity-test
  ;; only one (N) jobs are executed at a time
  (async done
         (-> (let [req  :my-job
                   resp :my-result
                   states (atom [])]
               (run-with {:execute (partial sync! resp)
                          :parallelity 1
                          :transition (fn transition [state job]
                                        (swap! states conj [(jobs/job-info job) (jobs/job-status job)])
                                        (c/return))}
                         
                         (c/return :action (jobs/start-job req 1)
                                   :action (jobs/start-job req 2)
                                   :action (jobs/start-job req 3))

                         (fn [env]
                           (new js/Promise
                                (fn [resolve reject]
                                  (js/setTimeout (fn []
                                                   ;; Note: also depends on the order of actions in c/return

                                                   ;; Note: when doing it with 'async!' above, we actually see some kind of non-determinism - :running 3 before the :completed 2
                                                   #_[[1 :pending] [2 :pending] [3 :pending] [1 :running] [1 :completed] [2 :running] [3 :running] [2 :completed] [3 :completed]]
                                            
                                                   (is (= [[1 :pending] [2 :pending] [3 :pending] [1 :running] [1 :completed] [2 :running] [2 :completed] [3 :running] [3 :completed]]
                                                          @states))
                                                   (resolve))
                                                 10))))))
             (.then (fn [_] (done))))))
