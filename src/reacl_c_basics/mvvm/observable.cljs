(ns ^:no-doc reacl-c-basics.mvvm.observable)

;; A partial implementation of the Observable API (not widespread in browsers; RxJS would have one too though)
;; https://wicg.github.io/observable/#observable-api

;; TODO: not quite the same; seems the api has a mutable subscriber instead of a subscription return for example.

(deftype ^:private Observable [subscribe])

(defn observable [subscribe]
  ;; subscribe should return a cancel function
  (Observable. subscribe))

(defn observable-from [v]
  ;; (observable-from observable) == observable
  (if (instance? Observable v)
    v
    ;; TODO: do some coercion, from promises for example?
    nil
    )
  )

(deftype ^:private Observer [next error complete])

(defn- make-observer [on-next on-error on-complete]
  ;; creates an observer; Note: either on-error or on-complete will be called once; or none at all.
  (Observer. on-next on-error on-complete))

(defn observer [on-next & [on-error on-complete]]
  (make-observer on-next
                 (or on-error (fn [err]))
                 (or on-complete (fn []))))

(deftype ^:private Subscription [unsubscribe ^:mutable closed])

(defn- comp-observers [c1 c2]
  (observer (fn [v]
              ((.-next c1) v)
              ((.-next c2) v))
            (fn [err]
              ((.-error c1) err)
              ((.-error c2) err))
            (fn []
              ((.-complete c1))
              ((.-complete c2)))))

(defn subscribe! [observable observer]
  ;; subscribes an observer to an observable, returning a subscription.
  (let [close (atom nil)
        sub (Subscription. (fn [this]
                             (when-not (.-closed this)
                               (set! (.-closed this) true)
                               (when-let [c @close]
                                 (c))))
                           false)]
    (let [res ((.-subscribe observable)
               (comp-observers (make-observer
                                (fn [val])
                                (fn [err]
                                  (set! (.-closed sub) true))
                                (fn []
                                  (set! (.-closed sub) true)))
                               observer))]
      (assert (or (nil? res) (fn? res) (ifn? res))
              "Observable.subscribe must return a close function or nil.")
      (reset! close res))
    sub))


(defn unsubscribe! [subscription]
  ;; ends a subscription.
  ((.-unsubscribe subscription) subscription))

(defn subscription-closed? [subscription]
  ;; returns whether a subscription has ended or is complete.
  (.-closed subscription))
