(ns reacl-c-basics.mvvm.view
  "Functions to define views on view models."
  (:require [reacl-c-basics.mvvm.async :as async]
            [reacl-c-basics.mvvm.observable :as obs]
            [reacl-c.core :as c :include-macros true]
            [active.clojure.lens :as lens]))

(defrecord ^:private Run [command])

(defn- with-update* [f]
  (c/with-async (fn [return!]
                  (let [update! (fn [f & args]
                                  (return! (fn [model]
                                             (c/return :state (apply f model args)))))]
                    (f update!)))))

(defn- cleanup-subscriptions [subscriptions]
  (->> subscriptions
       (remove obs/subscription-closed?)
       (vec)))

(def ^:private unsubscribe-on-finish
  (c/cleanup (fn [subscriptions]
               (c/return ;; :state []
                         :action (c/effect (fn []
                                             (doseq [s subscriptions]
                                               (obs/unsubscribe! s))))))))

(defn- async-frame [item]
  (with-update* (fn [update!]
                  (c/with-state-as [model subscriptions :local []]
                    (-> (c/fragment (c/focus lens/first item)
                                    (c/focus lens/second unsubscribe-on-finish))
                        (c/handle-action (fn [state action]
                                           (if (instance? Run action)
                                             (let [[model subscriptions] state
                                                   subscriptions (cleanup-subscriptions subscriptions)]
                                               (let [new-sub (async/run (:command action) update!)]
                                                 (assert (some? new-sub) new-sub)
                                                 ;; TODO: test synchronous updates, resp. handle them differently?
                                                 (c/return :state [model (conj subscriptions new-sub)])))
                                             (c/return :action action)))))))))

(defn view "Render the given view with the given (initial) model." [model view] ;; call 'root' or so?
  (c/isolate-state model (async-frame view)))

(defn bind "Embed a different view on the sub-model defined by the given writable property on the current model." [property view]
  ;; Note: this is already sufficient to bind the inputs or forms from this lib.
  (c/focus property (async-frame view)))

(defn show "Disablays the current value of the given property, which should usually be a string." [property]
  (c/focus property (c/dynamic identity)))

(let [g (fn [model property f args]
          (apply f (property model) args))]
  (defn with-value* [property f & args] ;; TODO: macro version?
    (c/dynamic g property f args)))

(defn- command-ret [r]
  (if (async/async-command? r)
    (c/return :action (Run. r))
    (c/return :state r)))

(defn event ;; name... command? handle?
  "Returns an event handler that calls the given command with the model, the event itself and any additional args."
  [command & args]
  (fn [model event]
    (command-ret (apply command model event args))))

(defn init [command & args]
  ;; got to be careful here, as calling command is often not pure (creates a fn if async); so 'once' would emit it again.
  (c/with-state-as model
    (c/execute-effect (apply c/effect command model args)
                      (fn [_model r]
                        (command-ret r)))))

