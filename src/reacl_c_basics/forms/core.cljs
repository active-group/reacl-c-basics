(ns reacl-c-basics.forms.core
  "This namespace contains replacements for the basic form
  elements (input, select, textbox) but with corresponding
  states.

  The [[select]] item also supports any kind of values for the
  `:value` attribute of [[option]]s, not just strings.
  
  Additionally, the new attributes `:invalid` and `:validate` are
  supported for all of them, allowing for a declarative way to
  use `setCustomValidity` on the dom nodes.

  An attribute `:report-validity` can be set to true on input items
  and [[form]] to call `reportValidity` on them on every state
  change."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [type?]))

;; Note: this files contains some utils for forms.types, but they are
;; considered private here.

(c/defn-effect ^:private set-custom-validity! [ref msg]
  (let [input (c/deref ref)]
    (.setCustomValidity input msg)))

(defn- maybe-with-ref [ref f]
  (if (some? ref)
    (f ref)
    (c/with-ref f)))

(c/defn-effect ^:private report-validity*!
  [ref v]
  (let [elem (c/deref ref)]
    (.reportValidity elem)))

(let [f (fn [ref state]
          ;; pass state, so that is executed again when the state changes.
          (c/init (c/return :action (report-validity*! ref state))))]
  (defn- do-report-validity [ref]
    (c/dynamic (f/partial f ref))))

(let [add-validity-features
      (fn [f attrs content r]
        (c/fragment (apply f (-> attrs
                                 (assoc :ref r)
                                 (dissoc attrs :invalid :report-validity))
                           content)
                    (when (:invalid attrs)
                      (c/init (c/return :action (set-custom-validity! r (:invalid attrs)))))
                    (when (:report-validity attrs)
                      (do-report-validity r))))]
  (defn- with-invalid-attr [f attrs & content]
    ;; Note: when :invalid was set before, it must be set to "" (not nil) to remove that state.
    (if (or (:invalid attrs) (:report-validity attrs))
      (maybe-with-ref (:ref attrs)
                      (f/partial add-validity-features f attrs content))
      (apply f attrs content))))

(defn- input-base [f attrs]
  (with-invalid-attr f attrs))

(defn- with-validate-fn [value f attrs & content]
  (apply f (if (contains? attrs :validate)
             (dom/merge-attributes (when-let [f (:validate attrs)]
                                     ;; allow f to return nil, but still reset a previous msg then.
                                     {:invalid (or (f value) "")})
                                   (dissoc attrs :validate))
             attrs)
         content))

(let [on-change (fn [_ ev]
                  (.-value (.-target ev)))]
  (defn- value-base [f attrs]
    (c/with-state-as value
      (with-validate-fn value (partial input-base f)
        (dom/merge-attributes {:value value
                               :onChange on-change}
                              attrs)))))

(let [on-change (fn [_ ev]
                  (.-checked (.-target ev)))]
  (defn- checked-base [f attrs]
    (c/with-state-as checked
      (with-validate-fn checked (partial input-base f)
        (dom/merge-attributes {:checked (boolean checked)
                               :onChange on-change}
                              attrs)))))

(defn- static-base [f attrs]
  (c/static f attrs))

(def ^:private input-value-base (partial value-base dom/input))
(def ^:private input-checked-base (partial checked-base dom/input))

(def ^:private input-button-base (partial static-base dom/input))

(c/defn-effect ^:private reset-input! [input-ref]
  (when-let [d (c/deref input-ref)]
    (set! (.-value d) "")))

(let [on-change-single (fn [_ event]
                         (first (.. event -currentTarget -files)))
      on-change-multi (fn [_ event]
                        (.. event -currentTarget -files))
      maybe-reset-single (fn [value ref]
                           (when (nil? value)
                             (c/init (c/return :action (reset-input! ref)))))
      maybe-reset-multi (fn [value ref]
                          (when (empty? value)
                            (c/init (c/return :action (reset-input! ref)))))]
  (defn ^:private input-file-base
    [attrs]
    ;; The thing with file inputs is that they cannot be 'controlled'; the application cannot chose a file, only the user.
    ;; So we can only do a state change to what was selected, or to nil (reset selected file)
    (maybe-with-ref
     (:ref attrs)
     (fn [ref]
       (c/fragment (dom/input (dom/merge-attributes {:onChange (if (:multiple attrs)
                                                                 on-change-multi
                                                                 on-change-single)
                                                     :ref ref}
                                                    attrs))
                   (c/dynamic (if (:multiple attrs)
                                maybe-reset-multi
                                maybe-reset-single)
                              ref))))))

(defn ^:no-doc new-type [base-fn default-attrs mk-optional]
  ;; Note: base must not be (fundamentally) changed (or, only together with to-optional)
  {::base base-fn
   ::attributes default-attrs
   ::to-optional mk-optional})

(def ^:no-doc type-base ::base)
(def ^:no-doc type-attributes ::attributes)
(def ^:no-doc type-to-optional ::to-optional)

(let [g (fn [item]
          (c/focus (lens/default "") item))]
  (defn ^:no-doc string-optional
    [type]
    (-> type
        (assoc type-to-optional identity)
        (update type-base
                (fn [f]
                  (f/comp g f))))))

(defn ^:no-doc no-optional [type]
  (throw (js/Error. "This type cannot be made optional.")))

(defn ^:no-doc type? [v]
  (and (map? v) (some? (::base v))))

(defn ^:no-doc native-type [type]
  (assert (or (nil? type) (string? type)) type)
  (case type
    ("submit" "cancel")
    (new-type input-button-base {:type type} no-optional)
    
    ("checkbox" "radio")
    (new-type input-checked-base {:type type} no-optional)

    ("file")
    (new-type input-file-base {:type type} no-optional) ;; always optional
        
    (new-type input-value-base {:type type} string-optional)))

(defn ^:no-doc add-type-attributes [type attrs]
  (assert (type? type) (str "Not a type: " (pr-str type)))
  (-> type
      (update type-attributes dom/merge-attributes attrs)))

(defn- option-value-default-placeholder [value]
  (cond
    (nil? value) "" ;; must be the empty string, so that browsers can apply :required attribute for a value of nil.
    (string? value) value
    :else (pr-str value)))

(defn- select-single-value [value ph-map]
  (if-let [ph (get ph-map value)]
    ph
    (option-value-default-placeholder value)))

(defn- select-value [value multiple? ph-map]
  (if multiple?
    (to-array (map #(select-single-value % ph-map) value))
    (select-single-value value ph-map)))

(defn- placeholder-single-value [placeholder ph-map]
  (if-let [[value _] (first (filter #(= (second %) placeholder) ph-map))]
    value
    ;; should be there; complain?
    placeholder))

(defn- placeholder-multiple-values [placeholders ph-map]
  (->> (array-seq placeholders)
       (map #(placeholder-single-value % ph-map))
       (apply list)
       ;; return nil instead of empty list (design decision)
       (not-empty)))

(defrecord ^:private OptionValue [placeholder value])

(dom/defn-dom option
  "An item that can be used inside a [[select]] element to define a
  selectable option. The attribute `:value` can be any value, and the
  contents should be the text shown to the user."
  [attrs & contents]
  (let [v (:value attrs)
        placeholder (or (:placeholder attrs) ;; overridable, if pr-str is not suitable.
                        (option-value-default-placeholder v))]
    ;; replace :value with a (generated) string, and report the actual value as an action to the surrounding select.
    (c/fragment
     (apply dom/option (-> attrs
                           (assoc :value placeholder)
                           (dissoc :placeholder))
            contents)
     (when (not= v placeholder) ;; = not string?
       (c/init (c/return :action (OptionValue. placeholder v)))))))

(dom/defn-dom optgroup
  "An optgroup item usable to group [[option]] items in a [[select]]."
  [attrs & options]
  (apply dom/optgroup attrs options))

(let [on-change-single (fn [[value ph-map] ev]
                         (let [placeholder (.-value (.-target ev))]
                           (c/return :state [(placeholder-single-value placeholder ph-map) ph-map])))
      on-change-multiple (fn [[value ph-map] ev]
                           ;; Note: React's value property not usable in this case..
                           (let [placeholders (-> (js/Array.from (.-options (.-target ev)))
                                                  (.filter (fn [^js o]
                                                             (.-selected o)))
                                                  (.map (fn [^js o] (.-value o))))]
                             (c/return :state [(placeholder-multiple-values placeholders ph-map) ph-map])))
      add-placeholder (fn [[st ph-map] a]
                        (if (instance? OptionValue a)
                          (c/return :state [st (assoc ph-map (:value a) (:placeholder a))])
                          (c/return :action a)))]
  (dom/defn-dom select "A select element that allows [[option]]s to have
  arbitrary values. If the `:multiple` attribute is set, then the
  state of this item must be a list of values or nil.

  Also supports the new attributes `:invalid` and `:validate`."
    [attrs & options]
    ;; Note: the [[option]] elements from this namespace report their value and placeholder via an action.
    (c/with-state-as [value ph-map :local {}]
      (-> (apply (partial with-validate-fn value (partial with-invalid-attr dom/select))
                 (dom/merge-attributes
                  {:value (select-value value (:multiple attrs) ph-map)
                   :onChange (if (:multiple attrs) on-change-multiple on-change-single)}
                  attrs)
                 options)
          (c/handle-action add-placeholder)))))

(dom/defn-dom textarea
  "An item representing its string state in a textual multiline input element.
   
   Also supports the new attributes `:invalid` and `:validate`."
  [attrs]
  (value-base dom/textarea attrs))

(dom/defn-dom form
  "The same as `reacl-c.dom/form`, but with the additional attribute `:report-validity`."
  [attrs & content]
  (apply with-invalid-attr dom/form attrs content))

(defn- unbind [handler]
  ;; hacky hacky... should be part of reacl-c
  (when handler
    (when (c/bound-handler? handler)
      ;; a bound handler is function, that when called returns a 'c/return :action CallHandler' thingy, which contains the original handler fn.
      (let [ch (handler nil)]
        (:f (first (c/returned-actions ch)))))))

(let [t-submit (fn [on-submit [external internal] ev]
                 ;; call on-submit on internal (not yet committed) state, which it may change or not.
                 ;; publish internal => external unless preventDefault was called.
                 ;; (note in any case we eventually call preventDefault to prevent browser action)
                 (let [ret (if (some? on-submit)
                             (c/as-returned (on-submit internal ev))
                             (c/return))]
                   (let [new-internal (c/returned-state ret)]
                     (cond
                       ;; no modification of state, and no actual submit.
                       (and (= c/keep-state new-internal)
                            (.-defaultPrevented ev))
                       (c/returned-state ret c/keep-state)

                       :else
                       (let [new-internal (if (= c/keep-state new-internal)
                                            internal
                                            new-internal)
                             new-external (if (.-defaultPrevented ev)
                                            external
                                            (do (.preventDefault ev)
                                                new-internal))]
                         (c/returned-state ret [new-external new-internal]))))))
      t-reset (fn [on-reset [external internal] ev]
                ;; calls on-reset on internal (not yet resetted) state; if it returns a state, then that is used.
                ;; if it doesn't, then it depends on preventDefault: per default use external state as new internal one.
                ;; otherwise, don't change anything (cancels the reset)
                (let [ret (if (some? on-reset)
                            (c/as-returned (on-reset internal ev))
                            (c/return))]
                  (cond
                    ;; on-reset returned new (internal) state
                    (not= c/keep-state (c/returned-state ret))
                    (do (when-not (.-defaultPrevented ev)
                          (.preventDefault ev))
                        (c/returned-state ret [external (c/returned-state ret)]))

                    ;; no state changes at all
                    (.-defaultPrevented ev)
                    ret ;; must be keep-state here

                    ;; default, external => internal
                    :else
                    (c/returned-state ret [external external]))))]
  (dom/defn-dom transient-form
    "Same as [[form]], but the state of `content` is a copy of the form state,
     and form submit and form reset publish or discard the modified state
     respectively.

     Optionally, the event handlers `:onSubmit` and `:onReset` can be
     set, but work differently than for normal forms. Both are called
     with the internal, potentially modified state and an Event
     object. In `:onSubmit` you may change the state, before it is
     used as the new form state, unless you call `.preventDefault` on
     the event. If `:onReset` returns a state, then that is used as
     the resetted state. If `:onReset` does not modify the state, then
     the internal state is set to the form state, unless
     `.preventDefault` is called on the event."
    [attrs & content]
    (let [on-submit (unbind (:onSubmit attrs))
          on-reset (unbind (:onReset attrs))]
      (c/with-state-as external
        (c/local-state external
          (form (dom/merge-attributes attrs
                                      {:onSubmit (f/partial t-submit on-submit)
                                       :onReset (f/partial t-reset on-reset)})
                (c/focus lens/second
                         (apply c/fragment content))))))))

(defrecord ^:private Complete [state result])

(defn- complete-event [state result]
  #js {"submitted" state
       "result" result
       "defaultPrevented" false
       "preventDefault" (fn []
                          (this-as this
                            (set! (.-defaultPrevented this) true)))})

(let [ts-submit (fn [on-submit [state submit?] ev]
                  (let [ret (if (some? on-submit)
                              (c/as-returned (on-submit state ev))
                              (c/return))]
                    (cond
                      ;; submit cancelled; but new (unsubmitted) state may be set.
                      (.-defaultPrevented ev)
                      (c/returned-state ret
                                        (if (= c/keep-state (c/returned-state ret))
                                          c/keep-state
                                          [(c/returned-state ret) false]))

                      ;; don't publish yet, but set submit? flag.
                      :else
                      (do (.preventDefault ev)
                          (c/returned-state ret [(if (= c/keep-state (c/returned-state ret))
                                                   state
                                                   (c/returned-state ret))
                                                 true])))))
      
      ts-reset (fn [on-reset [state submit?] ev]
                 ;; Nothing special, only unwrap/wrap the submit flag
                 (let [ret (if (some? on-reset)
                             (c/as-returned (on-reset state ev))
                             (c/return))]
                   (c/returned-state ret (if (= c/keep-state (c/returned-state ret))
                                           c/keep-state
                                           [(c/returned-state ret) submit?]))))
      
      ts-inner-complete (fn [[state submit?] result]
                          ;; state is the submitted, but yet published state here
                          ;; need to pass the state up, to actually change the form state.
                          ;; but submit? flag must be changed locally.
                          (c/return :state [state false]
                                    :action (->Complete state result)))
      
      ts-outer-complete (fn [on-complete state a]
                          ;; state is the form state here; need to update that after subscription completes.
                          ;; Note: no way to 'suppress' the publishing here (could use a syntetic event and a preventDefault call?)
                          (cond
                            (instance? Complete a) ;; TODO: pass that as an event-like object?
                            (let [submitted-state (:state a)
                                  result (:result a)]
                              (if (some? on-complete)
                                (let [event (complete-event submitted-state result)
                                      ret (on-complete state event)]
                                  (cond
                                    (or (not= c/keep-state (c/returned-state ret))
                                        (.-defaultPrevented event))
                                    ret

                                    :else
                                    (c/returned-state ret submitted-state)))
                                (c/return :state submitted-state)))

                            :else
                            (c/return :action a)))]
  (dom/defn-dom subscription-form
    "A [[transient-form]] that is submitted asynchronously via a subscription item.

     Initially, the state of `content` is a copy of the state. On form
     submit, the subscription `((:subscription attrs) state)` is
     rendered, until it delivers a result. When that subscription
     delivers a result, the form state is set to the edited state. On
     form reset the edited state is discarded and reset to the form
     state.

     Optionally, the event handlers `:onSubmit`, `:onReset` and
     `:onComplete` can be set.

     The `:onSubmit` handler is called with the modified,
     to-be-submitted state and an Event object. You can change the
     internal state here, and you can call `.preventDefault` on the
     event to cancel the submission.

     The `:onReset` handler is called with the modified state and an
     Event object. If that returns a new state, then that is published
     as the new form state. If not, and unless `.preventDefault` is
     called, then the modified state is discarded and set to the form
     state.

     The `:onComplete` handler is called on the original, unsubmitted
     form state and an Event-like object. That object has a field
     `.-result`, containing the result of the subscription and a field
     `.-submitted` containing the submitted state, and
     `.preventDefault` method. If the handler returns a new state,
     then that is used as the new form state. If not, then the
     submitted state is used as the new form state, unless
     `.preventDefault` is called on the event.

     The form is disabled automatically while the asynchronous operation is
     in progress.

     See [[reacl-c-basics.ajax/form]] for a version of this that is
     specialized on submitting values via ajax."
    [attrs & content]
    ;; TODO: add :dirty-class as class to fieldset? (or add :onChange)?
    (let [subscription (:subscription attrs)
          on-submit  (unbind (:onSubmit attrs))
          on-complete (unbind (:onComplete attrs))
          on-reset (unbind (:onReset attrs))]
      (assert (some? subscription) "Missing :subscription attribute.")
      (-> (c/local-state
           false ;; submit? (Note: this will never change actually; only the transient copy of it)
           (transient-form (dom/merge-attributes (dissoc attrs :onComplete :subscription)
                                                 {:onSubmit (f/partial ts-submit on-submit)
                                                  :onReset (f/partial ts-reset on-reset)})
                           ;; Note: the items here will see the modified state (so also the submit flag we set in onSubmit)
                           (c/with-state-as [state submit?]
                             (c/fragment
                              (c/focus lens/first
                                       (apply dom/fieldset {:disabled submit?}
                                              content))
                              (when submit?
                                (-> (subscription state)
                                    (c/handle-action ts-inner-complete)))))))
          (c/handle-action (f/partial ts-outer-complete on-complete))))))


(defn- lift-type [t]
  (if (type? t)
    t
    (native-type t)))

(dom/defn-dom input
  "An item representing its state as the value of an input element.

   The `:type` attribute can influence the type of the state:
   - for `\"checkbox\"` and `\"radio\"` the state must be a boolean
   - for `\"file\"` it must be a `nil` or a `js/File` object
   - for `\"submit\"` and `\"cancel\"` it is ignored
   - for `nil` and all other strings, the state must be a string.

  Note that the `:type` can also be one of the types defined in [[reacl-c-basics.forms.types]].

  Also supports the new attributes `:invalid` and `:validate`."
  [attrs]
  (let [t (lift-type (:type attrs))]
    ((type-base t) (dom/merge-attributes (type-attributes t) (dissoc attrs :type)))))
