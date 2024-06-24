(ns reacl-c-basics.forms.methods
  (:require [reacl-c-basics.forms.core :as core]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]))

(defrecord ^:private DefaultOnSubmit [ev])

(let [t-submit
      (fn [on-submit [state submit?] ev]
        (if (some? on-submit)
          (c/return :action (c/call-handler on-submit ev)
                    ;; Note: must be handled /after/ the user action.
                    :action (DefaultOnSubmit. ev))
          (c/return :action (DefaultOnSubmit. ev))))

      post-submit
      (fn [[state submit?] a]
        (cond
          (instance? DefaultOnSubmit a)
          (let [ev (:ev a)]
            (if (.-defaultPrevented ev)
              (c/return)
              (do (.preventDefault ev) ;; prevent browser default in any case
                  (c/return :state [state true]))))
          :else
          (c/return :action a)))
      
      t-complete
      (fn [on-complete [state submit?] result]
        (c/return :state [state false]
                  :action (c/call-handler on-complete result)))]
  (defn ^:private subscription-wrapper [subscription base-form attrs & content]
    ;; Note: these handler must have been bound in core/form (via defn-dom)
    (let [on-submit (:onSubmit attrs)
          on-complete (:onComplete attrs)]
      ;; TODO: maybe some :pending-class attr?
      (c/with-state-as [state submit? :local false]
        (-> (base-form (dom/merge-attributes (dissoc attrs :onComplete)
                                             {:onSubmit (f/partial t-submit on-submit)})
                       (c/fragment
                        (c/focus lens/first (apply dom/fieldset {:disabled submit?}
                                                   content))
                        (when submit?
                          (-> (subscription state)
                              (c/handle-action (f/partial t-complete on-complete))))))
            (c/handle-action post-submit))))))

(let [g (fn [f f-args state]
          (apply f state f-args))]
  (defn subscription
    "Returns a value suitable to use as a `:method` attribute in [[reacl-c-basics.forms.core/form]].

  When using this method in a form, then on form submit, `(f state &
  args)` is called per default, which must return a subscription item.
  That item is then used until it delivers a result.

  The `:onComplete` event handler is called with the result of the subscription.

  While the subscription result is pending, the content of the form is
  automatically disabled."
    [f & args]
    (core/new-method (f/partial subscription-wrapper (f/partial g f args)))))


(let [g (fn [f f-args state]
          (ajax/execute (apply f state f-args)))]
  (defn ajax
    "Returns a value suitable to use as a `:method` attribute in [[reacl-c-basics.forms.core/form]].

  This is similar to [[subcription]], but `(f state & args)` must
  return an ajax request, which is then executed to submit the form.

  The `:onComplete` event handler is called with the response of the ajax call

  While the ajax call is running, the content of the form is
  automatically disabled."
    [f & args]
    (core/new-method (f/partial subscription-wrapper (f/partial g f args)))))
