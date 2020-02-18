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
  (is (= (ajax/GET "/url") (ajax/GET "/url")))
  )

(deftest delivery-queue-test
  (let [req  (ajax/GET "/url")
        job (ajax/delivery-job! req :info)

        resp (ajax/ok-response :value)

        program (c/name-id "program")
        prog (c/named program (dom/div)) 
        
        mk-env (fn [& [options]] (tu/env (ajax/delivery-queue prog :queue options)))]
    (testing "starts running an action"
      (let [env (mk-env)]
        (tu/provided [ajax/execute (execute-dummy nil)]
                     (tu/mount! env {:queue []})
                     (is (= (c/return :state {:queue [(assoc job :status :pending)]})
                            (tu/inject-action! (tu/find-named env program)
                                               job)))
                     (is (= (c/return :state {:queue [(assoc job :status :running)]})
                            (tu/update! env {:queue [(assoc job :status :pending)]})))))
      )
    (testing "completes eventually"
      (let [env (mk-env)]
        (tu/provided [ajax/execute (execute-dummy resp)]
                     ;; Note: it completes immediately, because of our fetch-once-dummy
                     (tu/mount! env {:queue []})
                     ;; Note: job = (async/deliver req), but not with an unknown id.
                     (is (= (c/return :state {:queue [(assoc job :status :pending)]})
                            (tu/inject-action! (tu/find-named env program)
                                               job)))
                     (is (= (c/return :state {:queue [(assoc job :status :completed :response resp)]})
                            (tu/update! env {:queue [(assoc job :status :pending)]})))))
      )
    (testing "does automatic cleanup"
      (let [env (mk-env {:auto-cleanup? true})]
        (tu/provided [ajax/execute (execute-dummy resp)]
                     ;; Note: it completes immediately, because of our fetch-once-dummy
                     (tu/mount! env {:queue []})
                     (let [r (tu/inject-action! (tu/find-named env program)
                                                job)]
                       (is (= (c/return :state {:queue [(assoc job :status :pending)]})
                              r))
                       (is (= (c/return :state {:queue []})
                              (tu/push!! env r))))))
      (let [env (mk-env {:auto-cleanup? false})]
        (tu/provided [ajax/execute (execute-dummy resp)]
                     (tu/mount! env {:queue []})
                     (is (= (c/return :state {:queue [(assoc job
                                                             :status :completed
                                                             :response resp)]})
                            (tu/push!! env (tu/inject-action! (tu/find-named env program)
                                                              job)))))))))


