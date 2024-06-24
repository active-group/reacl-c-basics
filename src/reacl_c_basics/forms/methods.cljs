(ns reacl-c-basics.forms.methods
  (:require [reacl-c-basics.forms.core :as core]
            [reacl-c-basics.ajax :as ajax]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]))

(defn- unbind [handler]
  ;; hacky hacky... should be part of reacl-c
  (when handler
    (when (c/bound-handler? handler)
      ;; a bound handler is function, that when called returns a 'c/return :action CallHandler' thingy, which contains the original handler fn.
      (let [ch (handler nil)]
        (:f (first (c/returned-actions ch)))))))

(let [t-submit
      (fn [on-submit [state submit?] ev]
        (if (some? on-submit)
          (let [ret (on-submit state ev)
                ret-state (c/returned-state ret)
                submit? (not (.-defaultPrevented ev))]
            (when-not (.-defaultPrevented ev)
              (.preventDefault ev))
            (c/returned-state ret (if (= c/keep-state ret-state)
                                    [state submit?]
                                    [ret-state submit?])))
          (do (.preventDefault ev)
              (c/return :state [state true]))))
      
      t-complete
      (fn [on-complete [state submit?] result]
        (if (some? on-complete)
          (let [ret (on-complete state result)
                ret-state (c/returned-state ret)]
            (c/returned-state ret (if (= c/keep-state ret-state)
                                    [state false]
                                    [ret-state false])))
          (c/return :state [state false])))]
  (defn ^:private subscription-form [base-form subscription attrs & content]
    (let [on-submit  (unbind (:onSubmit attrs))
          on-complete (unbind (:onComplete attrs))]
      ;; TODO: maybe some :pending-class attr?
      (c/with-state-as [state submit? :local false]
        (base-form (dom/merge-attributes (dissoc attrs :onComplete)
                                         {:onSubmit (f/partial t-submit on-submit)})
                   (c/fragment
                    (c/focus lens/first (apply dom/fieldset {:disabled submit?}
                                               content))
                    (when submit?
                      (-> (subscription state)
                          (c/handle-action (f/partial t-complete on-complete))))))))))

(defn- subscription-wrapper [subscription base-form attrs & content]
  (apply subscription-form
         base-form
         subscription
         attrs
         content))

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
