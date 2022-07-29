(ns reacl-c-basics.jobs.core
  "Jobs allow you to define arbitrary and domain specific synchronous or
  asynchronous side effects of your application, separate from their
  implementation, and functions to execute and control them savely in
  your application."
  (:require [reacl-c.core :as c :include-macros true]
            [active.clojure.cljs.record :as r :include-macros true]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

(r/define-record-type ^:private Job
  {:rtd-record? true}
  (make-job id description info status result)
  ^{:private false :arglists '([value]) :doc "Returns whether the given value is a job."} job?
  [id job-id
   description job-description
   info really-job-info
   status really-job-status
   result really-job-result])

(def ^{:private false :arglists '([job]) :doc "Returns the info object passed to [[run]]."}
  ;; Note: remove the two-arity fn from arglists actually makes that uncallable (not only documentation)
  job-info really-job-info)

(def ^{:private false :arglists '([job]) :doc "Returns the status of this job, `:pending`, `:running` or `:completed`."}
  job-status really-job-status)

(def ^{:private false :arglists '([job]) :doc "Returns the result for this job, or nil if the status is not `:completed`."}
  job-result really-job-result)

(defn- completed? [job]
  (= :completed (job-status job)))

(defn ^:no-doc job! ;; probably not useful to anybody.
  "Creates a new job, that you can put into the queue backing
  up a [[handle-josb]] via the `:manage` option."
  [description & [info]]
  (make-job (gensym "job") description info :pending nil))

(defn start-job
  "Returns an action that signals the next [[handle-jobs]] up in the
  item tree to execute a job with the given description. An arbitrary
  `info` value can be attached to distinguish separate jobs with the
  same description (see [[job-info]])."
  [description & [info]]
  ;; the job is the action for simplicity; id will be added in the 'handler'
  (make-job nil description info :pending nil))

(defn- ensure-id!
  "add job id, if not set yet"
  [a]
  ;; TODO: should be an effect.
  (if (nil? (job-id a))
    (lens/shove a job-id (gensym "job"))
    a))

(defn- ret-state [ret state]
  ;; TODO: try this without mangling with returned objects; should be possible without.
  (if (not (c/returned? ret))
    [(c/return) ret]
    (let [st (c/returned-state ret)]
      (if (= st c/keep-state)
        [ret state]
        [(lens/shove ret c/returned-state c/keep-state) st]))))

(let [->running (fn [f [state job]]
                  (if (some? job)
                    (let [job (lens/shove job really-job-status :running)
                          [ret state] (ret-state (f state job) state)]
                      (c/merge-returned (c/return :state [state job])
                                        ret))
                    (c/return)))
      ->completed (fn [f [state job] result]
                    (if (some? job)
                      (let [job (-> job
                                    (lens/shove really-job-status :completed)
                                    (lens/shove really-job-result result))
                            [ret state] (ret-state (f state job) state)]
                        (c/merge-returned (c/return :state [state nil]) ;; nil to remove it from queue
                                          ret))
                      (c/return)))]
  (c/defn-item ^:private job-executor [execute-fn f]
    (c/with-state-as [st job]
      (when (some? job) ;; may happen when 'manage' removed jobs.
        (case (job-status job)
          :pending (c/once (f/partial ->running f))
          :running (-> (execute-fn (job-description job))
                       (c/handle-action (f/partial ->completed f)))
          :completed c/empty)))))

(let [->pending (fn [f [state queue] job]
                  ;; run :pending
                  (let [[ret state] (ret-state (f state job) state)]
                    (c/merge-returned (c/return :state [state (conj queue job)])
                                      ret)))
      add-jobs (fn [f pred [state queue] a]
                 (if (and (job? a) (pred (job-description a)))
                   (->pending f [state queue] (ensure-id! a))
                   (c/return :action a)))
      job-lens (fn ;; the job needs the item state, and the single job.
                 ([id [state queue]]
                  [state (first (filter #(= id (job-id %)) queue))])
                 ([id [st0 queue] [state job]]
                  [state
                   (if (nil? job)
                     ;; remove
                     (remove #(= id (job-id %)) queue)
                     (into (empty queue)
                           (map (fn [j]
                                  (if (= id (job-id j))
                                    job
                                    j))
                                queue)))]))
      run-jobs (fn [execute-fn f parallelity [st0 queue]]
                 (apply c/fragment
                        (map (fn [id]
                               ;; NOTE: when 'manage' is used, jobs may disapper; so job may become nil.
                               (-> (c/focus (f/partial job-lens id)
                                            (job-executor execute-fn f))
                                   (c/keyed (str id))))
                             (map job-id
                                  (cond->> queue
                                    (some? parallelity) (take parallelity))))))
      manage-queue (fn [manage queue]
                     (c/init (manage queue)))]
  (defn ^{:doc "Returns an item that defines how jobs are
  executed. Use [[run]] to get an action that can be emitted by
  `item` that adds a new job to an internal queue.

  `:execute` Required option: The item function to actually execute
  the jobs. It is called with the job description and return an item
  that must eventually emit an action with the result of the job.

  `:predicate` options: A predicate on the job descriptions. If it
  returns false, the job is passed on upwards in the tree. Per default
  all jobs must be handled by `:execute`.

  `:transition` option: Jobs transition from a :pending state,
  over :running into :completed, and for each transition `(transition
  state job)` is evaluated on the returned item's state, which must
  return a [[reacl-c.core/return]] value.

  `:manage` option:
  Some control over the queue can be achieved by specifying `manage`
  which is called on the queue after it changed and must return an
  updated queue. You can remove jobs or change their order for example.
  Newest items are at the end of the queue.

  `:parallelity` option: If `nil`, the default, all jobs in the queue
  are actually started immediately and 'in parallel'. Set to an
  integer larger than 0 to specify that only that number of jobs are
  executed at the same time.
  "
          }
    handle-jobs [item options]
    (assert (some? (:execute options)) "Missing required option :execute.")
    (assert (or (nil? (:parallelity options)) (and (integer? (:parallelity options))
                                                   (> (:parallelity options) 0)))
            "The :parallelity option must be an integer larger than 0.")
    (let [transition (or (:transition options) (f/constantly (c/return)))
          manage (or (:manage options) identity)
          execute (:execute options)
          parallelity (when (some? (:parallelity options))
                        (max 1 (int (:parallelity options))))
          filter-pred (or (:predicate options) (f/constantly true))]
      
      (c/local-state []
                     (c/fragment
                      (c/focus lens/second (c/dynamic (f/partial manage-queue manage)))
                      (c/dynamic (f/partial run-jobs execute transition parallelity))
                      (c/handle-action (c/focus lens/first item)
                                       (f/partial add-jobs transition filter-pred)))))))

