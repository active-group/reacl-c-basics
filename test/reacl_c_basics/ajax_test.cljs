(ns reacl-c-basics.ajax-test
  (:require [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu :include-macros true]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
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

(deftest fetch-once-start-test
  (async done
         (let [started (atom false)
               dummy (dummy-ajax started [true ::value])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [_ _]
                                                (done)
                                                (c/return))))]
             (is (not @started))
             (tu/mount! env nil)
             (is @started)))))

(deftest fetch-once-ok-test
  (async done
         (let [dummy (dummy-ajax nil [true ::value])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [_ response]
                                                (is (ajax/response-ok? response))
                                                (is (= ::value (ajax/response-value response)))
                                                (done)
                                                (c/return))))]
             (tu/mount! env nil)))))

(deftest fetch-once-failure-test
  (async done
         (let [dummy (dummy-ajax nil [false {:v ::error}])]
           (let [env (tu/env (ajax/fetch-once (ajax/request dummy "uri")
                                              (fn [_ response]
                                                (is (not (ajax/response-ok? response)))
                                                (is (= {:v ::error} (ajax/response-value response)))
                                                (done)
                                                (c/return))))]
             (tu/mount! env nil)))))

(defn execute-dummy [result]
  (let [a (constantly result)
        h (fn [state _ f args]
            (js/console.log "Complete!")
            (apply f state result args))]
    (fn [req f & args]
      ;; if result = nil, it never completes; otherwise immediately on mount.
      (cond-> (c/fragment)
        (some? result) (c/did-mount a)))))

(deftest fetch-test
  ;; fetch puts the response into a slot in the state.
  (let [resp (#'ajax/make-response true :value :req)
        env (tu/env (ajax/fetch (ajax/GET "/url")))]
    (tu/provided [ajax/execute (execute-dummy resp)]
                 ;; fetch when state nil
                 (is (= (c/return :state resp)
                        (tu/mount! env nil)))
                 ;; do not fetch when some state.
                 (is (= (c/return)
                        (tu/mount! env resp))))))

;; TODO show-response-value ?

(deftest delivery-queue-test
  (let [mk-env #(tu/env (ajax/delivery-queue (dom/div) :queue))

        resp (#'ajax/make-response true :value :req)
        req  (ajax/GET "/url")

        job (ajax/delivery-job req :info :id)]
    (testing "starts running on mount"
      (let [env (mk-env)]
        (tu/provided [ajax/execute (execute-dummy nil)]
                     (is (= (c/return :state {:queue [(assoc job :status :running)]})
                            (tu/mount! env {:queue [job]})))))
      )
    ;; TODO: can this work? because it's a state change after mount, which might not work in test env?
    #_(testing "completes eventually"
      (let [env (mk-env)]
        (tu/provided [ajax/execute (execute-dummy resp)]
                     ;; Note: it completes immediately, because of our fetch-once-dummy
                     (tu/mount! env {:queue []})
                     ;; Note: job = (async/deliver req), but not with an unknown id.
                     (is (= (c/return :state {:queue [(assoc job :status :completed)]})
                            (tu/inject-action! (xpath/select-one (tu/get-component env) (xpath/>> ** "div"))
                                               job)))))
      )
    )
  )
