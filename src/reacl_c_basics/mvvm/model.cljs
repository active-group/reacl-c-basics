(ns reacl-c-basics.mvvm.model
  "Functions to define models for views.

  In general, a model is just arbitrary data, and functions on them.

  A read-only `property` is a pure function taking a model value.
  A writable `property` is a pure function with one arity for reading `([model] value)` and one for writing `([model new-value] => new-model)`.

  A `command` is a function taking a model and additional arguments, returning an updated model,
  or an asynchronous update:

  Asynchronous updates are create using [[async]], which takes a function of an 'update!' parameter:

  ```
  (def my-command (fn [model & args]
                    (async (fn [update!]
                             (subscribe! ... (fn [result] (update! conj result))))))

  ```

  The given function should return a subscription to an observable, or to a promise, created by the [[subscribe!]] function.

  For convenience, the above pattern can be shortened to:

  ```
  (def my-command (fn [model & args]
                    (watch ... conj)))
  ```
  
  "
  (:require [reacl-c-basics.mvvm.async :as async]
            [reacl-c-basics.mvvm.observable :as obs]))

;; TODO: test utils for testing async commands; maybe also for mocking them?

(defn async
  "Returns a value that turns a command into an asynchronous
  command. Calls `(f update!)` with an update function, which can be
  called later with a function that received the current model and must
  return a new model."
  [f]
  (async/async-command f))

(defn observable
  "Returns an observable. When subscribed to `(subscribe subscriber)` is called, which
  can call `(.next subscriber value)` several times and eventually call either
  `(.complete subscriber)` or `(.error subscriber)` at most once."
  [subscribe]
  (obs/observable subscribe))

;; TODO common observables, for timers, ajax, etc. atoms and promises?

#_(defn observable-from [v]
  ;; TODO: docstring, v may be a promise for example, or a list.
    (obs/observable-from v))

(defn subscribe! [observable on-next & [on-error on-complete]]
  (obs/subscribe! observable (obs/observer on-next on-error on-complete)))

(defn watch
  "Watch the given observable, integrating its values into the model.
  Will call `(next model value)` which must return a new
  model. Optionally also `(error model err)` and `(complete model)` if
  and when the observable ends.

  Must be used as the return value of a command."
  [observable next & [error complete]]
  (async (fn [update!]
           (subscribe! observable
                       (fn [value]
                         (update! next value))
                       (when error
                         (fn [err]
                           (update! error err)))
                       (when complete
                         (fn []
                           (update! complete)))))))

