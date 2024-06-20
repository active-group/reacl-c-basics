(ns reacl-c-basics.ajax-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c-basics.ajax-test-util :as ajax-test-util]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tuc :include-macros true]
            [reacl-c-basics.jobs.core :as jobs]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c-basics.forms.core :as forms]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(c/defn-subscription set-atom! deliver! [atom value]
  (reset! atom value)
  (fn [] nil))

(c/defn-subscription sync! deliver! [value]
  (deliver! value)
  (fn [] nil))

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
         (tuc/map-subscriptions {(ajax/execute request) (sync! ((get (ajax/request-options request) :convert-response) resp1))}))
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
  ;; the ajax delivery is just jobs/handle-jobs with a default executer.
  (is (= (jobs/handle-jobs c/empty
                           {:execute ajax/execute
                            :predicate ajax/request?
                            :parallelity nil})
         (ajax/delivery c/empty))))

(deftest real-execute-test
  (async done
         (let [res (atom nil)
               p (new js/Promise (fn [resolve reject]
                                   (reset! res resolve)))]
           (dt/rendering
            (-> (ajax/execute (ajax/GET "http://invalid.invalid/"))
                (c/handle-action (fn [_ r]
                                   (@res r)
                                   r)))
            (fn [env]
              (.then p
                     (fn [res]
                       (is (ajax/response? res))
                       (done))))))))

(deftest form-test
  (let [post (fn [v] (ajax/POST "http://invalid.invalid/" {:params v}))]
    (dt/rendering
     (-> (ajax/form {:data-testid "form"
                     :request post}
                    (c/focus :foo (forms/input {:data-testid "input" :type "text"}))
                    (dom/input {:data-testid "submit" :type "submit"}))
         (ajax-test-util/emulate-requests
          {(post {:foo "baz"}) (ajax/make-response true :ok)}))
     :state {:foo "bar"}
     (fn [env]
       (let [input (dt/get env (dt/by-testid "input"))
             submit-btn (dt/get env (dt/by-testid "submit"))]

         ;; change value to baz
         (dt/fire-event input
                        :change {:target {:value "baz"}})

         ;; not published yet
         (is (= {:foo "bar"} (dt/current-state env)))

         ;; submit (with ok response)
         (dt/fire-event submit-btn :click)

         ;; state changed
         (is (= {:foo "baz"} (dt/current-state env)))))))

  (testing "onSubmit can change the value to be submitted"
    (let [post (fn [v] (ajax/POST "http://invalid.invalid/" {:params v}))
          requests (atom [])]
      (dt/rendering
       ;; Note: reacl-c.main.react/embed, which dom-testing uses,
       ;; currently has a bug/inconvenience, in that simultaneous
       ;; state changes are not seen be actions handlers (takes a
       ;; rendering roundtrip). Using isolate-state to work around
       ;; that here.
       (c/isolate-state
        {:foo "bar"}
        (-> (ajax/form {:data-testid "form"
                        :request post
                        :onSubmit (fn [state]
                                    (c/return :state (update state :foo str "zinga")))}
                       (c/focus :foo (forms/input {:data-testid "input" :type "text"}))
                       (dom/input {:data-testid "submit" :type "submit"}))
            (ajax-test-util/emulate-requests
             (fn [req]
               (swap! requests conj req)
               ({(post {:foo "bazzinga"}) (ajax/make-response true :ok)}
                req)))))
       ;; :state {:foo "bar"}
       (fn [env]
         (let [input (dt/get env (dt/by-testid "input"))
               submit-btn (dt/get env (dt/by-testid "submit"))]

           ;; change value to baz and submit
           (dt/fire-event input
                          :change {:target {:value "baz"}})
           (dt/fire-event submit-btn :click)

           ;; state changed, and request was made
           ;; ideally the request before the state change in :onSubmit isn't even tried;
           (is (= [(post {:foo "bazzinga"})] @requests))

           ;; (is (= {:foo "bazzinga"} (dt/current-state env)))
           ))))))
