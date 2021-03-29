(ns reacl-c-basics.ajax-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tuc :include-macros true]
            [reacl-c.test-util.test-renderer :as tu]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(defn after [ms thunk]
  (js/window.setTimeout thunk
                        ms))

(defn dummy-ajax [started raw-response]
  (fn [uri options]
    (when started (reset! started true))
    (when-let [[ok? response] raw-response]
      (js/window.setTimeout (fn []
                              (if ok?
                                ((:handler options) response)
                                ((:error-handler options) response)))
                            0))))

(defn maybe-execute-effects! [env ret]
  ;; TODO: should not require internals of reacl-c
  ;; TODO: maybe an :execute-effects? option to 'tu/env' (with a replacement handler?)
  (doseq [a (filter reacl-c.base/effect? (:actions ret))]
    ;; Note: generally, could return new 'returns'
    (tu/execute-effect! env a)))

(deftest fetch-once-start-test
  (async done
         (let [started (atom false)
               dummy (dummy-ajax started [true ::value])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [_ _]
                                                (js/window.setTimeout done 0)
                                                (c/return))))]
             (is (not @started))
             (maybe-execute-effects! env (tu/mount! env nil))
             (is @started)))))

(deftest fetch-once-ok-test
  (async done
         (let [dummy (dummy-ajax nil [true ::value])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [state response]
                                                (is (ajax/response-ok? response))
                                                (is (= ::value (ajax/response-value response)))
                                                (js/window.setTimeout done 0)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(deftest fetch-once-failure-test
  (async done
         (let [dummy (dummy-ajax nil [false {:v ::error}])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [state response]
                                                (is (not (ajax/response-ok? response)))
                                                (is (= {:v ::error} (ajax/response-value response)))
                                                (js/window.setTimeout done 0)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(deftest fetch-once-convert-test
  (async done
         (let [dummy (dummy-ajax nil [true ::value])]
           (let [env (tu/env (ajax/fetch-once (-> (ajax/request dummy "uri")
                                                  (ajax/map-response (fn [response]
                                                                       (is (ajax/response-ok? response))
                                                                       (is (= ::value (ajax/response-value response)))
                                                                       ::other-value)))
                                              (fn [state response]
                                                (is (= ::other-value response))
                                                (js/window.setTimeout done 0)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(deftest fetch-once-convert-2-test
  (async done
         (let [dummy (dummy-ajax nil [true ::value])]
           (let [env (tu/env (ajax/fetch-once (-> (ajax/request dummy "uri")
                                                  (ajax/map-ok-response (fn [value]
                                                                          (is (= ::value value))
                                                                          ::other-value)))
                                              (fn [state response]
                                                (is (ajax/response-ok? response))
                                                (is (= ::other-value (ajax/response-value response)))
                                                (js/window.setTimeout done 0)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(defn execute-dummy [result]
  (let [a (c/return :action result)]
    (fn [req f & args]
      ;; if result = nil, it never completes; otherwise immediately on mount.
      (if (some? result)
        (c/once (f/constantly a))
        (c/fragment)))))

(deftest fetch-test
  ;; fetch puts the response into a slot in the state.
  (let [resp (ajax/ok-response :value)
        env (tu/env (ajax/fetch (ajax/GET "/url")))]
    (tuc/provided [ajax/execute (execute-dummy resp)]
                  ;; fetch when state nil
                  (is (= (c/return :state resp)
                         (tu/mount! env nil)))
                  ;; do not fetch when some state.
                  (is (= (c/return)
                         (tu/mount! env resp))))))

(deftest fetch-when+state-test
  ;; fetch-when puts the response into a slot in the state, and refetches on condition.
  (let [resp (ajax/ok-response :value)
        env (tu/env (c/with-state-as [resp fetch?]
                      (c/focus lens/first
                               (ajax/fetch-when+state (ajax/GET "/url") fetch?))))]
    (tuc/provided [ajax/execute (execute-dummy resp)]
                  ;; fetch when state nil
                  (is (= (c/return :state [[resp false] true])
                         (tu/mount! env [[nil nil] true])))
                 
                  ;; do not fetch when some state.
                  (is (= (c/return)
                         (tu/mount! env [[nil nil] false])))

                  ;; fetch once later.
                  (is (= (c/return :state [[resp false] true])
                         (tu/update! env [[nil nil] true])))

                  ;; and refetch maybe.
                  (is (= (c/return)
                         (tu/update! env [[resp nil] false])))
                  ;; Note: the :state update would be optimized away,
                  ;; when the response is identical (it does another
                  ;; fetch nevertheless, but in this test we need a
                  ;; different response):
                  (let [resp2 (ajax/ok-response :value2)]
                    (tuc/provided [ajax/execute (execute-dummy resp2)]
                                  (is (= (c/return :state [[resp2 false] true])
                                         (tu/update! env [[resp nil] true])))))
                  )
    ;; and when request is not completed, loading state stays true
    (tuc/provided [ajax/execute (constantly c/empty)]
                  (is (= (c/return :state [[nil true] true])
                         (tu/mount! env [[nil nil] true]))))

    ;; fetch-when is just fetch-when+state without the loading-state.
    (is (= (ajax/fetch-when (ajax/GET "/url") true)
           (c/local-state nil (ajax/fetch-when+state (ajax/GET "/url") true))))
    ))

;; TODO show-response-value ?

(deftest request-equality-test
  (is (= (ajax/GET "/url") (ajax/GET "/url"))))

(deftest delivery-test
  (let [req  (ajax/GET "/url")
        job (ajax/delivery-job! req :info)

        resp (ajax/ok-response :value)

        program (c/name-id "program")
        prog (c/named program (dom/div))

        add-status (fn [states state job]
                     (let [st  (ajax/delivery-job-status job)]
                       (swap! states conj st)
                       (c/return :state (conj state st))))
        
        mk-env (fn [states]
                 (tu/env (ajax/delivery prog
                                        (f/partial add-status states))))]
    (testing "starts running a job and completes eventually"
      (let [states (atom [])
            env (mk-env states)]
        ;; Note: it completes immediately, because of our fetch-once-dummy
        (tuc/provided [ajax/execute (execute-dummy resp)]
                      (tu/mount! env [])

                      (let [r (tu/inject-action! (tu/find-named env program)
                                                 job)]
                        ;; currently not properly testable, due to limitation/bug of the React test-renderer.
                        #_(is (= (c/return :state [:pending :running :completed])
                                 r)))
                      ;; will go immediately from pending to running.
                      (is (= [:pending :running :completed]
                             @states)))))))
