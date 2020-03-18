(ns reacl-c-basics.ajax
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.base :as base]
            [reacl-c.dom :as dom]
            [active.clojure.cljs.record :as r :include-macros true]
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

;; fetching data from server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-once
    "Returns an invisible item, that will execute the given Ajax
  request once, when mounted. When the request completes with an error
  or success, then `(f state response)` is evaluated, which
  must return a [[reacl-c.core/return]] value."
    [req f]
    (-> (execute req)
        (c/handle-action f)))

(let [handler (fn [_ response]
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
  (if (not (base/returned? ret))
    ;; Note: must be a returned, but let reacl-c complain in the ordinary way.
    [ret state]
    (let [st (base/returned-state ret)]
      (if (= st base/keep-state)
        [ret state]
        [(lens/shove ret base/returned-state base/keep-state) st]))))

(let [->running (fn [f [state job]]
                  (let [job (lens/shove job delivery-job-status :running)
                        [ret state] (ret-state (f state job) state)]
                    (base/merge-returned (c/return :state [state job])
                                         ret)))
      ->completed (fn [f [state job] resp]
                    (let [job (-> job
                                  (lens/shove delivery-job-status :completed)
                                  (lens/shove delivery-job-response resp))
                          [ret state] (ret-state (f state job) state)]
                      (base/merge-returned (c/return :state [state nil]) ;; nil to remove it from queue
                                           ret)))]
  (c/defn-dynamic job-executor [st job] [f]
    (case (delivery-job-status job)
      :pending (c/once (c/partial ->running f))
      :running (fetch-once (delivery-job-request job)
                           (c/partial ->completed f))
      :completed c/empty)))

(let [->pending (fn [f [state queue] job]
                  ;; run :pending
                  (let [[ret state] (ret-state (f state job) state)]
                    (base/merge-returned (c/return :state [state (conj queue job)])
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
      run-jobs (fn [f [st0 queue]]
                 (apply c/fragment
                        (map (fn [id]
                               (-> (c/focus (c/partial job-lens id)
                                            (job-executor f))
                                   (c/keyed (str id))))
                             (map delivery-job-id queue))))]
  (defn delivery
    "Returns an item that manages execution of Ajax requests in its
  local state. Use [[deliver]] to get an action that can be emitted by
  `item` that adds a new delivery job to an internal queue.

  Jobs transition from a :pending state, over :running
  into :completed (successful or not), and for each transition `(f
  state job)` is evaluated, which must return
  a [[reacl-c.core/return]] value."
    [item & [f]]
    (c/local-state []
                   (c/fragment
                    (c/dynamic (c/partial run-jobs f))
                    (c/handle-action (c/focus c/first-lens item)
                                     (c/partial add-jobs (or f (c/constantly (c/return)))))))))
