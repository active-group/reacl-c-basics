(ns reacl-c-basics.ajax-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tuc :include-macros true]
            [reacl-c.test-util.dom-testing :as dt]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(c/defn-subscription set-atom! deliver! [atom value]
  (reset! atom value)
  (fn [] nil))

(c/defn-subscription sync! deliver! [value]
  (deliver! value)
  (fn [] nil))

(c/defn-subscription async! deliver! [value]
  (let [id (js/setTimeout #(deliver! value) 0)]
    (fn []
      (js/clearTimeout id))))

(c/defn-subscription reveal deliver! [a]
  (reset! a deliver!)
  (fn [] (reset! a nil)))

(defn does-execute? [request item & [state]]
  (let [started (atom false)]
    (dt/rendering
     (-> item
         (tuc/map-subscriptions {(ajax/execute request) (set-atom! started true)}))
     :state state
     (fn [env]
       @started))))

(deftest fetch-once-start-test
  ;; request is executed on mount.
  (let [request (ajax/GET "http://invalid.invalid")]
    (is (does-execute? request (ajax/fetch-once request
                                                (fn [_ _]
                                                  (c/return)))))))

(deftest fetch-once-response-test
  ;; returns the response.
  (let [response (atom nil)
        resp (ajax/ok-response ::value)
        request (ajax/GET "http://invalid.invalid")]
    (dt/rendering
     (-> (ajax/fetch-once request
                          (fn [_ r]
                            (reset! response r)
                            (c/return)))
         (tuc/map-subscriptions {(ajax/execute request) (sync! resp)}))
     (fn [env]
       (is (= @response resp))))))

(deftest fetch-once-map-response-test
  (let [response (atom nil)
        resp1 (ajax/ok-response ::value)
        resp2 (ajax/error-response ::error)
        request (-> (ajax/GET "http://invalid.invalid")
                    (ajax/map-response (constantly resp2)))]
    (dt/rendering
     (-> (ajax/fetch-once request
                          (fn [_ r]
                            (reset! response r)
                            (c/return)))
         ;; TODO: should there be a helper fn to do the conversion?
         (tuc/map-subscriptions {(ajax/execute request) (sync! ((get-in request [:options :convert-response]) resp1))}))
     (fn [env]
       (is (= @response resp2))))))

(defn execute-dummy [result]
  (let [a (c/return :action result)]
    (fn [req f & args]
      ;; if result = nil, it never completes; otherwise immediately on mount.
      (if (some? result)
        (c/once (f/constantly a))
        (c/fragment)))))

(deftest fetch-test
  (let [request (ajax/GET "http://invalid.invalid")]
    ;; makes request when state is nil.
    (is (does-execute? request (ajax/fetch request) nil))

    ;; does not, when state is not nil.
    (is (not (does-execute? request (ajax/fetch request) :some)))))

(deftest fetch-when+state-test
  ;; does nothing if condition is false.
  (let [request (ajax/GET "http://invalid.invalid")]
    (is (not (does-execute? request (ajax/fetch-when request false) nil))))

  ;; fetch-when is just fetch-when+state without the loading-state.
  (is (= (ajax/fetch-when (ajax/GET "/url") true)
         (c/local-state nil (ajax/fetch-when+state (ajax/GET "/url") true))))
  
  ;; puts the response into a slot in the state, and refetches on condition.
  (let [request (ajax/GET "http://invalid.invalid")
        response1 (ajax/ok-response ::value)
        response2 (ajax/ok-response ::other-value)
        deliver! (atom nil)]

    (dt/rendering
     (-> (c/with-state-as [_ fetch?]
           (c/focus lens/first (ajax/fetch-when+state request fetch?)))
         (tuc/map-subscriptions {(ajax/execute request) (reveal deliver!)}))
     :state [[nil false] true]
     (fn [env]
       ;; pending initially
       (is (= (dt/current-state env) [[nil true] true]))
       ;; when request done, not pending anymore
       (@deliver! response1)
       (is (= (dt/current-state env) [[response1 false] true]))

       ;; does fetch again if condition changes to true.
       (dt/set-state! env [[response1 false] false])
       (dt/set-state! env [[response1 false] true])
       (@deliver! response2)
       (is (= (dt/current-state env) [[response2 false] true]))
       )))
  )

;; TODO show-response-value ?

(deftest request-equality-test
  (is (= (ajax/GET "/url") (ajax/GET "/url"))))

(deftest delivery-test
  (async done
         (-> (let [req  (ajax/GET "http://invalid.invalid")
                   resp (ajax/ok-response :v)
                   result (atom nil)

                   deliver! (atom nil)
                   states (atom [])]
               (testing "starts running a job and completes eventually"
                 (dt/rendering
                  (-> (ajax/delivery (dom/button {:data-testid "btn"
                                                  :onclick (fn [_ _]
                                                             (c/return :action (ajax/deliver req :info)))})
                                     (fn transition [state job]
                                       (swap! states conj (ajax/delivery-job-status job))
                                       (when (ajax/completed? job)
                                         (reset! result (ajax/delivery-job-response job)))
                                       (c/return)))
                      (tuc/map-subscriptions {(ajax/execute req) (async! resp)}))
                  (fn [env]
                    ;; nothing happens initially.
                    (is (= [] @states))

                    ;; deliver a job
                    (dt/fire-event (dt/get env (dt/by-test-id "btn")) :click)
                    (is (= [:pending :running] @states))

                    ;; getting completed state/response, asynchronously.
                    (new js/Promise.
                         (fn [resolve reject]
                           (js/setTimeout (fn []
                                            (is (= [:pending :running :completed] @states))
                                            (is (= resp @result))
                                            (resolve :done))
                                          1)))))))
             (.then (fn [_] (done))))))
