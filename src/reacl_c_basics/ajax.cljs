(ns reacl-c-basics.ajax
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.cljs.record :as r :include-macros true]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [ajax.core :as ajax]))

(defn check-options-invariants! [options]
  (assert (not (:handler options)))
  (assert (not (:error-handler options))))

(r/define-record-type Request (make-request f uri options)
  request?
  [f request-f
   uri request-uri
   options request-options])

(r/define-record-type Response
  (make-response ok? value)
  response?
  [ok? response-ok?
   value response-value])

(defn ok-response [result]
  (make-response true result))

(defn error-response [status & [body]]
  (make-response false {:status status :body body}))

(defn ^:no-doc request [f uri & [options]]
  (check-options-invariants! options)
  (make-request f uri options))

(let [g (fn [f args resp]
          (apply f resp args))]
  (defn map-response
    "Convert the response to the given request through a call to `(f response & args)`."
    [req f & args]
    (assert (instance? Request req))
    (let [f (if args (f/partial g f args) f)
          f (if-let [f2 (:convert-response (:options req))]
              (f/comp f f2)
              f)]
      (assoc-in req [:options :convert-response] f))))

(let [g (fn [resp f & args]
          (if (response-ok? resp)
            (assoc resp :value (apply f (response-value resp) args))
            resp))]
  (defn map-ok-response
    "Convert the [[response-value]] to the given request through a
  call to `(f response & args)` if it's an ok response."
    [req f & args]
    (apply map-response req g f args)))

(defn GET [uri & [options]] (request ajax/GET uri options))
(defn HEAD [uri & [options]] (request ajax/HEAD uri options))
(defn POST [uri & [options]] (request ajax/POST uri options))
(defn PUT [uri & [options]] (request ajax/PUT uri options))
(defn DELETE [uri & [options]] (request ajax/DELETE uri options))
(defn OPTIONS [uri & [options]] (request ajax/OPTIONS uri options))
(defn TRACE [uri & [options]] (request ajax/TRACE uri options))
(defn PATCH [uri & [options]] (request ajax/PATCH uri options))
(defn PURGE [uri & [options]] (request ajax/PURGE uri options))

(defn- execute-request! [request handler]
  (let [{f :f uri :uri options :options} request
        conv (or (:convert-response options) identity)
        nopts (-> options
                  (assoc :handler
                         (fn [response]
                           (handler (conv (make-response true response))))
                         :error-handler
                         (fn [error]
                           (when (not= :aborted (:failure error))
                             (handler (conv (make-response false error))))))
                  (dissoc :convert-response))]
    (f uri nopts)))

(c/defn-subscription execute deliver! [request]
  (let [id (execute-request! request deliver!)]
    (fn []
      (ajax/abort id))))

;; fetching data from server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-once
  "Returns an invisible item, that will execute the given Ajax
  request once, when mounted. When the request completes with an error
  or success, then `(f state response)` is evaluated, which must
  return a [[reacl-c.core/return]] value."
  [req f]
  (assert (request? req) req)
  (-> (execute req)
      (c/handle-action f)))

(let [handler (fn [_ response]
                (assert (response? response))
                (c/return :state response))]
  (c/defn-item fetch
    "Returns an invisible item, that will
  execute the given request whenever its state is or becomes nil, and
  set its state to the success or error response as soon as
  available."
    [req]
    (c/with-state-as response
      (if (some? response)
        (c/fragment)
        (fetch-once req handler)))))

(let [handler (fn [_ response]
                (c/return :state [response false]))]
  (c/defn-item fetch-when+state
    "Returns an invisible item, that will execute the given request once
  if `cond` is true, and also whenever `cond` changes from false to
  true. Updates its state to a tuple of the response and a loading flag:
  - `true` when a request is started, but hasn't completed yet
  - `false` when a request is completed.
  Initialize the state to false or nil."
    [req cond]
    (c/with-state-as [resp cond-c :local false]
      (c/fragment
       (c/focus lens/second (c/init cond))
       (if cond-c
         ;; refetch:
         (c/focus lens/first
                  (c/fragment (c/focus lens/second (c/init true))
                              (fetch-once req handler)))
         c/empty)))))

(defn fetch-when
  "Returns an invisible item, that will execute the given request once
  if `cond` is true, and also whenever `cond` changes from false to
  true. Updates its state to the response value after each request
  completed (successful or not)."
  [req cond]
  (c/local-state nil (fetch-when+state req cond)))

(defn throw-response-error [error]
  ;; error = {:status ...}
  (throw (ex-info (str "Ajax request failed: " (pr-str error)) {:type ::error :error error})))

(defn show-response-value
  [resp f-ok & [f-error]]
  (let [res-value (response-value resp)]
    (if (response-ok? resp)
      (f-ok res-value)
      (if f-error
        (f-error res-value)
        (throw-response-error res-value)))))

(defn maybe-show-response-value
  [resp e-pending f-ok & [f-error]]
  (if (nil? resp)
    e-pending
    (show-response-value resp f-ok f-error)))

;; delivering data to server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(r/define-record-type DeliveryJob
  ^private (make-delivery-job id req info status response)
  delivery-job?
  [id ^private delivery-job-id
   req delivery-job-request
   info delivery-job-info
   status delivery-job-status
   response delivery-job-response])

(defn- completed? [job]
  (= :completed (delivery-job-status job)))

(defn delivery-job!
  "Returns a new delivery job, that you can put into the state backing
  up a [[delivery-queue]]."
  [req & [info]]
  (make-delivery-job (gensym "delivery-job") req info :pending nil))

(defn deliver
  "Returns an action to add the given request to the end of the next
  queue [[delivery-queue]] up in the item tree. An arbitrary `info`
  value can be attached, identifying or describing the request."
  [req & [info]]
  ;; the job is the action for simplicity; id will be added in the 'handler'
  (make-delivery-job nil req info :pending nil))

(defn- ensure-id!
  "add job id, if not set yet"
  [a]
  ;; TODO: should be an effect.
  (if (nil? (delivery-job-id a))
    (lens/shove a delivery-job-id (gensym "delivery-job"))
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
                    (let [job (lens/shove job delivery-job-status :running)
                          [ret state] (ret-state (f state job) state)]
                      (c/merge-returned (c/return :state [state job])
                                        ret))
                    (c/return)))
      ->completed (fn [f [state job] resp]
                    (if (some? job)
                      (let [job (-> job
                                    (lens/shove delivery-job-status :completed)
                                    (lens/shove delivery-job-response resp))
                            [ret state] (ret-state (f state job) state)]
                        (c/merge-returned (c/return :state [state nil]) ;; nil to remove it from queue
                                          ret))
                      (c/return)))]
  (c/defn-item ^:private job-executor [execute-fn f]
    (c/with-state-as [st job]
      (when (some? job) ;; may happen when 'manage' removed jobs.
        (case (delivery-job-status job)
          :pending (c/once (f/partial ->running f))
          :running (-> (execute-fn (delivery-job-request job))
                       (c/handle-action (f/partial ->completed f)))
          :completed c/empty)))))

(let [->pending (fn [f [state queue] job]
                  ;; run :pending
                  (let [[ret state] (ret-state (f state job) state)]
                    (c/merge-returned (c/return :state [state (conj queue job)])
                                      ret)))
      add-jobs (fn [f [state queue] a]
                 (if (delivery-job? a)
                   (->pending f [state queue] (ensure-id! a))
                   (c/return :action a)))
      job-lens (fn ;; the job needs the item state, and the single job.
                 ([id [state queue]]
                  [state (first (filter #(= id (delivery-job-id %)) queue))])
                 ([id [st0 queue] [state job]]
                  [state
                   (if (nil? job)
                     ;; remove
                     (remove #(= id (delivery-job-id %)) queue)
                     (into (empty queue)
                           (map (fn [j]
                                  (if (= id (delivery-job-id j))
                                    job
                                    j))
                                queue)))]))
      run-jobs (fn [execute-fn f [st0 queue]]
                 (apply c/fragment
                        (map (fn [id]
                               ;; NOTE: when 'manage' is used, jobs may disapper; so job may become nil.
                               (-> (c/focus (f/partial job-lens id)
                                            (job-executor execute-fn f))
                                   (c/keyed (str id))))
                             (map delivery-job-id queue))))
      manage-queue (fn [manage queue]
                     (c/init (manage queue)))
      options-map? (fn [options]
                     (or (nil? options)
                         (and (map? options)
                              ;; active.clojure.functions are maps too :-/
                              (or (empty? options)
                                  (contains? options :transition)
                                  (contains? options :manage)
                                  (contains? options :execute)))))]
  (defn delivery
    "Returns an item that manages execution of Ajax requests in its
  local state. Use [[deliver]] to get an action that can be emitted by
  `item` that adds a new delivery job to an internal queue.

  Jobs transition from a :pending state, over :running
  into :completed (successful or not), and for each transition `(transition
  state job)` is evaluated on the returned items state, and which can modify it.

  Some control over the queue can be achieved by specifying `manage`
  which is called on the queue after it changed and must return an
  updated queue. Newest items are at the end of the queue.

  The item function to actually execute the requests can also be
  explicitly set; it defaults to [[execute]].
  "
    ([item]
     (delivery item {}))
    ([item options]
     ;; backwards compat: options actually looks like a transition function:
     (if (not (options-map? options))
       (delivery item options nil)
       (let [transition (or (:transition options) (f/constantly (c/return)))
             manage (or (:manage options) identity)
             execute (or (:execute options) execute)]
         (c/local-state []
                        (c/fragment
                         (c/focus lens/second (c/dynamic (f/partial manage-queue manage)))
                         (c/dynamic (f/partial run-jobs execute transition))
                         (c/handle-action (c/focus lens/first item)
                                          (f/partial add-jobs transition)))))))
    ([item transition manage]
     ;; deperecated; use options map
     (delivery item {:transition transition
                     :manage manage}))))
