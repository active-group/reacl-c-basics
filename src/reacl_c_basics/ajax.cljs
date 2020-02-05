(ns reacl-c-basics.ajax
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.cljs.record :as r :include-macros true]
            [active.clojure.lens :as lens :include-macros true]
            [ajax.core :as ajax]))

(defn check-options-invariants! [options]
  (assert (not (:handler options)))
  (assert (not (:error-handler options))))

(defrecord ^:private Request [f uri options])

(r/define-record-type ^:private Response
  (make-response ok? value request)
  response?
  [ok? response-ok?
   value response-value
   request response-request])

(def ^:no-doc empty-response (Response. true nil nil))

(defn ^:no-doc request [f uri & [options]]
  (check-options-invariants! options)
  (Request. f uri options))

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
                       (handler (make-response true response request)))
                     :error-handler
                     (fn [error]
                       (when (not= :aborted (:failure error))
                         (handler (make-response false error request)))))]
    (f uri nopts)))

(c/defn-subscription ^:private execute deliver! [request]
  (let [id (execute-request! request deliver!)]
    (fn []
      (ajax/abort id))))

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
  (let [error (response-value response)
        req (response-request response)]
    (throw (ex-info (str "Ajax request to " (:uri req) " failed.") {:type ::error :request req :error error}))))

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

;; TODO: not sure if this a good idea; maybe a timed cleanup is better?
(let [h (fn [lens old new]
          ;; FIXME: use focus, or lift-lens (for index lenses)
          (c/return :state (lens/shove new
                                       lens
                                       (let [queue (lens/yank new lens)]
                                         (into (empty queue) (remove #(and (completed? %)
                                                                           (response-ok? (:response %)))
                                                                     queue))))))]
  (defn ^:no-doc delivery-queue-auto-cleanup [item lens]
    (c/capture-state-change item (c/partial h lens))))

(let [handler (fn [state lens a]
                (if (instance? DeliveryJob a)
                  ;; FIXME: use focus, or lift-lens (for index lenses)
                  (let [queue (lens/yank state lens)]
                    (c/return :state (lens/shove state lens (into (empty queue) (concat queue (list a))))))
                  (c/return :action a)))]
  (c/defn-dynamic ^:no-doc delivery-queue-handler state [e lens]
    (c/handle-action e (c/partial handler state lens))))

(c/def-dynamic ^:private execute-job job
  (c/fragment (c/did-mount (c/return :state (update job :status #(if (= :pending %) :running %))))
              (fetch-once (:req job)
                          (fn [a]
                            (assert (response? a))
                            (c/return :state (assoc job
                                                    :status :completed
                                                    :response a))))))

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

(c/def-dynamic ^:private delivery-queue-executor queue
  (apply c/fragment
         (map (fn [job]
                (-> (c/focus (JobLens. (:id job)) execute-job)
                    ;; Note: I think we depend on a key, so that jobs are not 'remounted'='restarted' when the queue changes :-/
                    ;; Could only change that by not using a subscription, I think.
                    (c/keyed (:id job))))
              (filter #(not (completed? %)) queue))))

(defn delivery-queue
  "Returns an item that manages a sequence of Ajax requests in its
  state, under the given `lens`. You can add [[delivery-job]] values
  to it, or use [[deliver]] to get an action that can be emitted by
  `e`. Jobs in the :pending state are executed in parallel,
  transitioning into the :running state, and end in the :completed
  state (irrespective of success or failure). When `(:auto-cleanup?
  options)` is true, sucessfully completed jobs are eventually removed
  from the queue automaticall."
  [e lens & [options]]
  (cond-> (c/fragment (delivery-queue-handler e lens) ;; adds delivery actions to the queue
                      (c/focus lens delivery-queue-executor) ;; drives executing and waiting for a response
                      )
    (:auto-cleanup? options) (delivery-queue-auto-cleanup lens) ;; removes successfully completed jobs when new state bubbles up
    ))


(defn deliver
  "Returns an action to add the given request to the end of the next
  queue [[delivery-queue]] up in the item tree. An arbitrary `info`
  value can be attached, identifying or describing the request."
  [req & [info]]
  ;; actions same as the jobs
  (delivery-job req info))
