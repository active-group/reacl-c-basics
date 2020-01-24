(ns reacl-c-basics.ajax
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.cljs.record :as r :include-macros true]
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

(c/defn-subscription ^:private execute deliver! [request]
  (let [{f :f uri :uri options :options} request
        nopts (assoc options
                     :handler
                     (fn [response]
                       (deliver! (make-response true response request)))
                     :error-handler
                     (fn [error]
                       (when (not= :aborted (:failure error))
                         (deliver! (make-response false error request)))))
        id (f uri nopts)]
    (fn []
      (ajax/abort id))))

;; fetching data from server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [handler (fn [state response f args]
                (assert (response? response))
                (apply f state response args))]
  (defn fetch-once
    "Returns an invisible element, that will execute the given Ajax
  request once, when mounted. When the request completes with an error
  or success, then `(f state response & args)` is evaluated, which
  must return a [[reacl-c.core/return]] value."
    [req f & args]
    (-> (execute req)
        (c/handle-action handler f args))))

(let [handler (fn [state response]
                (assert (response? response))
                (c/return :state response))]
  (c/defn-dynamic fetch state [req]
    (if (some? state)
      (c/fragment)
      (fetch-once req handler))))

(c/def-dynamic ^:no-doc throw-on-error response
  (let [error (response-value response)
        req (response-request response)]
    (throw (ex-info (str "Ajax request to " (:uri req) " failed.") {:type ::error :request req :error error}))))

(c/defn-dynamic ^:no-doc show-response response [e-ok & [e-error]]
  (if (response-ok? response)
    e-ok
    (or e-error throw-on-error)))

(defn show-response-value
  "Returns an element showing the value of an Ajax response, with
  alternative elements for ok and error states. The error part
  defaults to an element that throws an ex-info error
  with :type :arbeitskarten.webui.util.ajax/error"
  [e-ok & [e-error]]
  (let [e-ok (c/focus e-ok response-value)
        e-error (when e-error
                  (c/focus e-error response-value))]
    (show-response e-ok e-error)))

(c/defn-dynamic maybe-show-response-value response
  [e-pending e-ok & [e-error]]
  (if (some? response)
    (show-response-value e-ok e-error)
    e-pending))

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

(def ^:no-doc delivery-queue-auto-cleanup
  (-> (c/did-update (c/fragment)
                    (fn [_] ::update))
      (c/handle-action (fn [queue _]
                         (if (some completed? queue)
                           (c/return :state (into (empty queue)
                                                  (remove completed? queue)))
                           (c/return))))))

(let [handler (fn [state a lens]
                (if (instance? DeliveryJob a)
                  (let [queue (lens state)]
                    (c/return :state (lens state (into (empty queue) (concat queue (list a))))))
                  (c/return :action a)))]
  (c/defn-named ^:no-doc delivery-queue-handler [e lens]
    (c/handle-action e handler lens)))

(defn ^:private do-execute-job [job]
  (js/console.log "executing job:" job)
  (execute (:req job)))

(def ^:private execute-job
  (-> (c/dynamic do-execute-job)
      (c/did-mount (constantly ::start))
      (c/handle-action (fn [job a]
                         (cond
                           (response? a)
                           (let [response a]
                             (c/return :state (assoc job
                                                     :status :completed
                                                     :response response)))
                           :else
                           (do (assert (= ::start a))
                               (c/return :state (update job :status #(if (= :pending %) :running %)))))))))

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

(c/def-dynamic ^:no-doc delivery-queue-executor queue
  (apply c/fragment
         (map (fn [job]
                (-> (c/focus execute-job (JobLens. (:id job)))
                    ;; Note: I think we depend on a key, so that jobs are not 'remounted'='restarted' when the queue changes :-/
                    ;; Could only change that by not using a subscription, I think.
                    (c/keyed (:id job))))
              (filter #(not (completed? %)) queue))))

(defn delivery-queue
  "Returns an element that manages a sequence of Ajax requests in its
  state, under the given `lens`. You can add [[delivery-job]] values
  to it, or use [[deliver]] to get an action that can be emitted by
  `e`. Jobs in the :pending state are executed in parallel,
  transitioning into the :running state, and end in the :completed
  state (irrespective of success or failure). When new jobs are added,
  completed jobs are removed from the queue."
  [e lens]
  ;; TODO: maybe options that can influence the cleanup of the queue? No cleanup, timed cleanup? Let someone react to completion and decide there?
  (c/fragment (delivery-queue-handler e lens) ;; adds delivery actions to the queue
              (c/focus delivery-queue-executor lens) ;; drives executing and waiting for a response
              (c/focus delivery-queue-auto-cleanup lens) ;; removes completed jobs on update.
              ))


(defn deliver
  "Returns an action to add the given request to the end of the next
  queue [[delivery-queue]] up in the element tree. An arbitrary `info`
  value can be attached, identifying or describing the request."
  [req & [info]]
  ;; actions same as the jobs
  (delivery-job req info))
