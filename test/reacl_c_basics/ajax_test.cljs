(ns reacl-c-basics.ajax-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu :include-macros true]
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
                                                (done)
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
                                                (done)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(deftest fetch-once-failure-test
  (async done
         (let [dummy (dummy-ajax nil [false {:v ::error}])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [state response]
                                                (is (not (ajax/response-ok? response)))
                                                (is (= {:v ::error} (ajax/response-value response)))
                                                (done)
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
                                                (done)
                                                (c/return))))]
             (maybe-execute-effects! env (tu/mount! env nil))))))

(defn execute-dummy [result]
  (let [a (c/return :action result)]
    (fn [req f & args]
      ;; if result = nil, it never completes; otherwise immediately on mount.
      (if (some? result)
        (c/once (c/constantly a))
        (c/fragment)))))

(deftest fetch-test
  ;; fetch puts the response into a slot in the state.
  (let [resp (ajax/ok-response :value)
        env (tu/env (ajax/fetch (ajax/GET "/url")))]
    (tu/provided [ajax/execute (execute-dummy resp)]
                 ;; fetch when state nil
                 (is (= (c/return :state resp)
                        (tu/mount! env nil)))
                 ;; do not fetch when some state.
                 (is (= (c/return)
                        (tu/mount! env resp))))))

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
                                        (c/partial add-status states))))]
    (testing "starts running a job and completes eventually"
      (let [states (atom [])
            env (mk-env states)]
        ;; Note: it completes immediately, because of our fetch-once-dummy
        (tu/provided [ajax/execute (execute-dummy resp)]
                     (tu/mount! env [])

                     (let [r (tu/inject-action! (tu/find-named env program)
                                                job)]
                       ;; currently not properly testable, due to limitation/bug of the React test-renderer.
                       #_(is (= (c/return :state [:pending :running :completed])
                                r)))
                     ;; will go immediately from pending to running.
                     (is (= [:pending :running :completed]
                            @states)))))))
