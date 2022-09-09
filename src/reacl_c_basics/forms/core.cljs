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
  (set! (.-value (c/deref input-ref)) ""))

(let [on-change-single (fn [_ event]
                         (first (.. event -currentTarget -files)))
      on-change-multi (fn [_ event]
                        (.. event -currentTarget -files))
      maybe-reset (fn [value ref]
                    ;; we could compare dom element with value here... but just ignore when non-nil comes in for now.
                    (when (or (nil? value) (empty? value))
                      (c/init (c/return :action (reset-input! ref)))))]
  (defn ^:private input-file-base
    [attrs]
    ;; The thing with file inputs is that they cannot be 'controlled'; the application cannot chose a file, only the user.
    ;; So we can only do a state change to what was selected, or to nil (reset selected file)
    (maybe-with-ref
     (:ref attrs)
     (fn [ref]
       (c/fragment (c/dynamic maybe-reset ref)
                   (dom/input (dom/merge-attributes {:onChange (if (:multiple attrs)
                                                                 on-change-multi
                                                                 on-change-single)
                                                     :ref ref}
                                                    attrs)))))))

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
