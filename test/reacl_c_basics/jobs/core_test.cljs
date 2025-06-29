(ns reacl-c-basics.jobs.core-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.jobs.core :as jobs]
            [reacl-c.test-util.dom-testing :as dt]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(c/defn-subscription async! deliver! [resp req]
  (let [id (js/setTimeout (fn []
                            (deliver! resp))
                          0)]
    (fn []
      (js/clearTimeout id))))

(c/defn-subscription sync! deliver! [resp req]
  (deliver! resp)
  (fn [] nil))

(defn run-with [options return f]
  (dt/rendering
   (jobs/handle-jobs (dom/button {:data-testid "btn"
                                  :onClick (fn [_ _]
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
                           (new js/Promise
                                (fn [resolve reject]
                                  (js/setTimeout (fn []
                                                   (is (= [:pending :running :completed] @states))
                                                   (is (= resp @result))
                                                   (resolve :done))
                                                 10))))))
             (.then done))))

(defn- timeout [ms]
  (js/Promise. (fn [resolve]
                 (js/setTimeout (fn []
                                  (resolve nil))
                                ms))))

(deftest handle-jobs-manage-test
  ;; :manage can remove things from the queue.
  (async done
         (-> (let [req  :my-job
                   resp :my-result
                   job-completions (atom 0)
                   [completed! completed]
                   (let [a (atom nil)]
                     [a
                      (new js/Promise (fn [resolve reject]
                                        (reset! a resolve)))])
                   [manage-completed! manage-completed]
                   (let [a (atom nil)]
                     [a
                      (new js/Promise (fn [resolve reject]
                                        (reset! a resolve)))])
                     
                   last-queue (atom nil)]
               (run-with {:execute (partial async! resp)
                          :transition (fn transition [state job]
                                        (when (= :completed (jobs/job-status job))
                                          (swap! job-completions inc)
                                          (@completed! true))
                                        (c/return))
                          :manage (fn manage [queue]
                                    ;; keep only 1 in queue
                                    (let [q (if (empty? queue)
                                              queue
                                              [(last queue)])]
                                      (reset! last-queue q)
                                      (when (and (empty? q)
                                                 (> @job-completions 0))
                                        (@manage-completed! true))
                                      q))}

                         ;; start 3 jobs
                         (c/return :action (jobs/start-job req)
                                   :action (jobs/start-job req)
                                   :action (jobs/start-job req))

                         (fn [env]
                           ;; Note: completed is after exection of the job
                           ;; We still want to await the last call to manage with the final empty queue.
                           (js/Promise.race
                            [(.then completed
                                    (fn [_]
                                      (.then manage-completed
                                             (fn [_]
                                               (is (= 1 @job-completions))
                                               (is (empty? @last-queue))))))
                             (.then (timeout 5000)
                                    (fn [_]
                                      (is false "timed out")))]))))
             (.then done))))

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
             (.then done))))
