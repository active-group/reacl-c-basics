(ns reacl-c-basics.ajax
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu]
            [active.clojure.cljs.record :as r :include-macros true]
            [active.clojure.lens :as lens :include-macros true]
            [ajax.core :as ajax]))

(defn check-options-invariants! [options]
  (assert (not (:handler options)))
  (assert (not (:error-handler options))))

(r/define-record-type ^:private Request (make-request f uri options)
  request?
  [f request-f
   uri request-uri
   options request-options])

(r/define-record-type Response
  (make-response ok? value)
  response?
  [ok? response-ok?
   value response-value])

(def ^:no-doc empty-response (make-response true nil))

(defn ^:no-doc request [f uri & [options]]
  (check-options-invariants! options)
  (make-request f uri options))

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
        nopts (assoc options
                     :handler
                     (fn [response]
                       (handler (make-response true response)))
                     :error-handler
                     (fn [error]
                       (when (not= :aborted (:failure error))
                         (handler (make-response false error)))))]
    (f uri nopts)))

(c/defn-subscription execute deliver! [request]
  (let [id (execute-request! request deliver!)]
    (fn []
      (ajax/abort id))))

;; test utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ok-response [result]
  (make-response true result))

(defn error-response [status & [body]]
  (make-response false {:status status :body body}))

(defn request-subscribe-effect?
  "Returns if the given effect action performs the subscription to the result of the given request."
  [eff & [request]]
  (if request
    ;; with a concrete requests
    (tu/subscribe-effect? eff (execute request))
    ;; or generally, a request should be at a certain position of the subscription-effect args
    (and (tu/subscribe-effect? eff)
         (let [[req] (tu/subscribe-effect-args eff)]
           (request? req)))))

(defn request-unsubscribe-effect?
  "Returns if the given effect action performs the unsubscription from the result of the given request."
  [eff]
  (and (tu/unsubscribe-effect? eff)
       (let [[req] (tu/unsubscribe-effect-args eff)]
         (request? req))))

(defn request-subscribe-effect-request
  [eff]
  ;; Note: only for the subscription effect (not the unsubscription)
  (assert (request-subscribe-effect? eff))
  (first (tu/subscribe-effect-args eff)))

(defn request-unsubscribe-effect-request
  [eff]
  ;; Note: only for the subscription effect (not the unsubscription)
  (assert (request-unsubscribe-effect? eff))
  (first (tu/unsubscribe-effect-args eff)))

(defn requests-emulator
  "Returns an effect reducer, that calls f for all ajax requests that
  are subscribed to in a test environment, and if f returns a
  response, then makes it an action emitted from the corresponding
  subscription item. Otherwise it is passed on."
  [f]
  (fn [env eff]
    (cond
      (request-subscribe-effect? eff)
      (let [req (request-subscribe-effect-request eff)]
        (if-let [resp (f req)]
          ;; TODO: merge-returned should be in core or tu
          (reacl-c.base/merge-returned (tu/subscription-start-return env eff)
                                       (tu/subscription-result-return env eff resp))
          (c/return :action eff)))
      
      (request-unsubscribe-effect? eff)
      (let [req (request-unsubscribe-effect-request eff)]
        ;; is it one of the requests we emulate here? Then drop
        ;; it (it's a noop anyway - no stop! fn passed to
        ;; subscription-start-return)
        (if (some? (f req))
          (c/return)
          (c/return :action eff)))
      
      :else
      (c/return :action eff))))

;; fetching data from server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-once
    "Returns an invisible item, that will execute the given Ajax
  request once, when mounted. When the request completes with an error
  or success, then `(f response)` is evaluated, which
  must return a [[reacl-c.core/return]] value."
    [req f]
    (-> (execute req)
        (c/handle-action f)))

(let [handler (fn [response]
                (assert (response? response))
                (c/return :state response))]
  (c/defn-dynamic fetch "Returns an invisible item, that will
  execute the given request whenever its state is or becomes nil, and
  set its state to the success or error response as soon as
  available."
    response [req]
  (if (some? response)
    (c/fragment)
    (fetch-once req handler))))

(defn throw-response-error [response]
  (let [error (response-value response)]
    (throw (ex-info (str "Ajax request failed: " (pr-str error)) {:type ::error :error error}))))

(defn show-response-value
  [resp f-ok & [f-error]]
  (if (response-ok? resp)
    (f-ok (response-value resp))
    (if f-error
      (f-error (response-value resp))
      (throw-response-error resp))))

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

(defn delivery-job
  "Returns a new delivery job, that you can put into the state backing
  up a [[delivery-queue]]."
  [req & [info]]
  (make-delivery-job (gensym "delivery-job") req info :pending nil))

(defn- cleaned-up-lens
  ([queue]
   (not (some completed? queue)))
  ([queue cleaned-up?]
   (if cleaned-up?
     (remove completed? queue)
     queue)))

(def ^:private
  delivery-queue-auto-cleanup
  (c/focus cleaned-up-lens
           (c/dynamic (fn [cleaned-up?]
                        ;; Note: it might be dangerous to do this with
                        ;; a 'once'; if it interferes with other state
                        ;; updates, then make it asynchronous.
                        (if (not cleaned-up?)
                          (c/once (c/return :state true))
                          c/empty)))))

(let [handler (fn [state lens a]
                (if (instance? DeliveryJob a)
                  ;; FIXME: use focus, or lift-lens (for index lenses)
                  (let [queue (lens/yank state lens)]
                    (c/return :state (lens/shove state lens (into (empty queue) (concat queue (list a))))))
                  (c/return :action a)))]
  (c/defn-dynamic ^:no-doc delivery-queue-handler state [e lens]
    (c/handle-action e (c/partial handler state lens))))

(let [h (fn [job a]
          (assert (response? a))
          (c/return :state (assoc job
                                  :status :completed
                                  :response a)))]
  (c/def-dynamic ^:private execute-job job
    (c/fragment (c/once (c/return :state (update job :status #(if (= :pending %) :running %))))
                (fetch-once (:req job)
                            (c/partial h job)))))

(defrecord ^:private JobLens [id]
  IFn
  (-invoke [_ queue] (some #(and (= (:id %) id) %) queue))
  (-invoke [_ queue upd]
    (assert (= id (:id upd)) "Must not change id.")
    (into (empty queue)
          (map (fn [job]
                 (if (= (:id job) id)
                   upd
                   job))
               queue))))

(c/def-dynamic ^:private delivery-queue-parallel-executor queue
  (apply c/fragment
         (map (fn [job]
                (-> (c/focus (JobLens. (:id job)) execute-job)
                    ;; Note: I think we depend on a key, so that jobs are not 'remounted'='restarted' when the queue changes :-/
                    ;; Could only change that by not using a subscription, I think.
                    (c/keyed (:id job))))
              (filter #(not (completed? %)) queue))))

(c/def-dynamic ^:private delivery-queue-sequential-executor queue
  (if-let [job (first (filter #(not (completed? %)) queue))]
    (c/focus (JobLens. (:id job)) execute-job)
    c/empty))

(defn delivery-queue
  "Returns an item that manages a sequence of Ajax requests in its
  state, under the given `lens`. You can add [[delivery-job]] values
  to it, or use [[deliver]] to get an action that can be emitted by
  `e` and adds jobs to the end of the queue.

  Jobs transition from a :pending state, over :running
  into :completed (successful or not).  When `(:auto-cleanup?
  options)` is true, sucessfully completed jobs are eventually removed
  from the queue automatically. When `(:parallel? options)` is true,
  then not only the first non-completed job is executed, but all are
  started in parallel (though the browser might actually only run 2 at
  the same time)."
  [e lens & [options]]
  ;; TODO: auto-cleanup as default?
  (c/fragment (delivery-queue-handler e lens) ;; adds delivery actions to the queue
              (c/focus lens
                       (c/fragment
                        ;; drives executing and waiting for a response
                        (if (:parallel? options) delivery-queue-parallel-executor delivery-queue-sequential-executor)
                        ;; removes completed jobs.
                        (if (:auto-cleanup? options) delivery-queue-auto-cleanup c/empty)))))

(defn hidden-delivery-queue
  "Like [[delivery-queue]], but with a hidden queue in local state."
  [e & [options]]
  ;; TOOD: add a fn that can translate it into user state?
  (c/local-state []
                 (delivery-queue (c/focus c/first-lens e)
                                 c/second-lens
                                 (assoc options
                                        :auto-cleanup? true))))

(defn deliver
  "Returns an action to add the given request to the end of the next
  queue [[delivery-queue]] up in the item tree. An arbitrary `info`
  value can be attached, identifying or describing the request."
  [req & [info]]
  ;; actions same as the jobs
  (delivery-job req info))
