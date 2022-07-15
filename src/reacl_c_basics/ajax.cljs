(ns reacl-c-basics.ajax
  "This namespace contains utilities to do AJAX requests in a reacl-c application, based on `cljs-ajax`.

  At a glance, you first define requests with [[GET]], [[POST]]
  and [[PUT]].

  GET requests are usually there to get some data from a server and
  display it to the user. Typically you want to use [[fetch]] or a
  variant of it to do that, or the lower level [[execute]]. For
  example:

  ```
  (c/fragment (fetch (GET \"/resource\"))
              (c/dynamic pr-str))
  ```

  POST and PUT requests are typically sent in response to an action by
  the user. The [[delivery]] item and the [[deliver]] action are made
  to help with that:

  ```
  (delivery
   (dom/button
    {:onclick #(c/return :action (deliver (PUT \"/resource\" {:body v})))}))
  ```

  Note that all items cancel outstanding requests when they are
  removed from the dom tree.

  For details on the various options of the request constructors, see
  the documentation
  of [`cljs-ajax`](https://github.com/JulianBirch/cljs-ajax#getpostput).

  Also see [[reacl-c-basics.ajax-test-util]] for an easy way to
  simulate server responses in unit tests.
  "
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.cljs.record :as r :include-macros true]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [ajax.core :as ajax]))

(defn- check-options-invariants! [options]
  (assert (not (:handler options)))
  (assert (not (:error-handler options))))

(r/define-record-type ^:private Request
  {:rtd-record? true}
  (make-request f uri options)
  ^{:private false :arglists '([value]) :doc "Returns whether the given value is a request."} request?
  [f request-f
   uri request-uri
   options request-options])

(r/define-record-type ^:private Response
  {:rtd-record? true}
  (make-response ok? value)
  ^{:private false :arglists '([value]) :doc "Returns whether the given value is a response."} response?
  [ok? ^{:private false :arglists '([response]) :doc "Returns if the given response represents success, i.e. the status code was in the 2xx range."} response-ok?
   value ^{:private false :arglists '([response]) :doc "Returns the (parsed) response body of the given response if ok, or a map with `:status` and `:body` keys if not ok."} response-value])

(defn ok-response "Returns a response that is considered a success with the given value as its [[response-value]]." [result]
  (make-response true result))

(defn error-response "Returns a response that is considered a failure, with a map with `:status` and `:body` keys as its [[response-value]]." [status & [body]]
  (make-response false {:status status :body body}))

(defn ^:no-doc request [f uri & [options]]
  (check-options-invariants! options)
  (make-request f uri options))

(let [g (fn [f args resp]
          (apply f resp args))]
  (defn map-response
    "Convert every response to the given request through a call to `(f response & args)`."
    [req f & args]
    (let [f (if args (f/partial g f args) f)
          options (request-options req)
          f (if-let [f2 (:convert-response options)]
              (f/comp f f2)
              f)]
      (lens/shove req request-options (assoc options :convert-response f)))))

(let [g (fn [resp f & args]
          (if (response-ok? resp)
            (lens/shove resp response-value (apply f (response-value resp) args))
            resp))]
  (defn map-ok-response
    "Convert the [[response-value]] of every successfull responses to the given request through a
  call to `(f response & args)`."
    [req f & args]
    (apply map-response req g f args)))

(defn GET "Returns a GET request." [uri & [options]] (request ajax/GET uri options))
(defn HEAD "Returns a HEAD request." [uri & [options]] (request ajax/HEAD uri options))
(defn POST "Returns a POST request." [uri & [options]] (request ajax/POST uri options))
(defn PUT "Returns a PUT request." [uri & [options]] (request ajax/PUT uri options))
(defn DELETE "Returns a DELETE request." [uri & [options]] (request ajax/DELETE uri options))
(defn OPTIONS "Returns a OPTIONS request." [uri & [options]] (request ajax/OPTIONS uri options))
(defn TRACE "Returns a TRACE request." [uri & [options]] (request ajax/TRACE uri options))
(defn PATCH "Returns a PATCH request." [uri & [options]] (request ajax/PATCH uri options))
(defn PURGE "Returns a PURGE request." [uri & [options]] (request ajax/PURGE uri options))

(defn- execute-request! [request handler]
  (let [f (request-f request)
        uri (request-uri request)
        options (request-options request)
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

(c/defn-effect execute! "A subscription that will execute the given
request. If you want access to the response, use [[execute]]."
  [request]
  ;; we could return a promise of the result...?
  (execute-request! request (constantly nil)))

(c/defn-subscription execute "A subscription that will execute the
given request and deliver the response as soon as it is available."
  deliver! [request]
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
  set its state to the success or error response as soon as it is
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
  true. Updates its state to the response after each request
  completed (successful or not)."
  [req cond]
  (c/local-state nil (fetch-when+state req cond)))

(defn ^:no-doc throw-response-error [error] ;; TODO: deprecate/remove (used in web-ak)
  ;; error = {:status ...}
  (throw (ex-info (str "Ajax request failed: " (pr-str error)) {:type ::error :error error})))

(defn ^:no-doc show-response-value ;; TODO: deprecate/remove
  [resp f-ok & [f-error]]
  (let [res-value (response-value resp)]
    (if (response-ok? resp)
      (f-ok res-value)
      (if f-error
        (f-error res-value)
        (throw-response-error res-value)))))

(defn ^:no-doc maybe-show-response-value ;; TODO: deprecate/remove
  [resp e-pending f-ok & [f-error]]
  (if (nil? resp)
    e-pending
    (show-response-value resp f-ok f-error)))

;; delivering data to server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(r/define-record-type ^:private DeliveryJob
  {:rtd-record? true}
  (make-delivery-job id req info status response)
  ^{:private false :arglists '([value]) :doc "Returns whether the given value is a delivery job."} delivery-job?
  [id delivery-job-id
   req delivery-job-request
   info really-delivery-job-info
   status really-delivery-job-status
   response really-delivery-job-response])

(def ^{:private false :arglists '([job]) :doc "Returns the info object passed to [[deliver]]."}
  ;; Note: remove the two-arity fn from arglists actually makes that uncallable (not only documentation)
  delivery-job-info really-delivery-job-info)

(def ^{:private false :arglists '([job]) :doc "Returns the status of this job, `:pending`, `:running` or `:completed`."}
  delivery-job-status really-delivery-job-status)

(def ^{:private false :arglists '([job]) :doc "Returns the response for this job, or nil if the status is not `:completed`."}
  delivery-job-response really-delivery-job-response)

(defn- completed? [job]
  (= :completed (delivery-job-status job)))

(defn ^:no-doc delivery-job! ;; probably not useful to anybody.
  "Creates a new delivery job, that you can put into the queue backing
  up a [[delivery]] via the `:manage` option."
  [req & [info]]
  (make-delivery-job (gensym "delivery-job") req info :pending nil))

(defn deliver
  "Returns an action to add the given request to the end of the next
  queue [[delivery]] up in the item tree. An arbitrary `info`
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
                    (let [job (lens/shove job really-delivery-job-status :running)
                          [ret state] (ret-state (f state job) state)]
                      (c/merge-returned (c/return :state [state job])
                                        ret))
                    (c/return)))
      ->completed (fn [f [state job] resp]
                    (if (some? job)
                      (let [job (-> job
                                    (lens/shove really-delivery-job-status :completed)
                                    (lens/shove really-delivery-job-response resp))
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
  (def ^{:arglists '([item] [item options] [item transition manage])
         :doc "Returns an item that manages the execution of Ajax requests in its
  local state. Use [[deliver]] to get an action that can be emitted by
  `item` that adds a new delivery job to an internal queue.

  `:transition` option:
  Jobs transition from a :pending state,
  over :running into :completed (successful or not), and for each
  transition `(transition state job)` is evaluated on the returned
  item's state, which must return a [[reacl-c.core/return]] value.

  `:manage` option:
  Some control over the queue can be achieved by specifying `manage`
  which is called on the queue after it changed and must return an
  updated queue. Newest items are at the end of the queue.

  `:execute` option:
  The item function to actually execute the requests can also be
  explicitly set; it defaults to [[execute]].

  Note: `(delivery item transition manage)` is deprecated.
  "
         }
    delivery
    (fn
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
                       :manage manage})))))

