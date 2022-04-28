(ns reacl-c-basics.forms.core
  "The basic form elements (input, select, textbox) as items with
  corresponding states. The [[select]] item also supports any kind
  of values for the [[option]] `:value` attribute, not just strings.
  
  Additionally, the additional attributes `:invalid` and `:validate`
  are supported for all of them, allowing for a declarative way to
  use [[setCustomValidity]] on the dom nodes."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [type?]))

;; TODO: where does :multiple apply; how to support it? files?
;; TODO: input type=submit and cancel ? we usually use buttons though.

(c/defn-effect ^:private set-custom-validity! [ref msg]
  (let [input (c/deref ref)]
    (.setCustomValidity input msg)))

(defn- maybe-with-ref [ref f]
  (if (some? ref)
    (f ref)
    (c/with-ref f)))

(defn- with-invalid-attr [f attrs & content]
  ;; Note: when :invalid was set before, it must be set to "" (not nil) to remove that state.
  (if-let [msg (:invalid attrs)]
    (maybe-with-ref (:ref attrs)
                    (fn [r]
                      (c/fragment (apply f (-> attrs
                                               (assoc :ref r)
                                               (dissoc attrs :invalid)) content)
                                  (c/init (c/return :action (set-custom-validity! r (:invalid attrs)))))))
    (apply f attrs content)))

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
    [attrs]
    (c/with-state-as checked
      (with-validate-fn checked (partial input-base f)
        (dom/merge-attributes {:checked (boolean checked)
                               :onChange on-change}
                              attrs)))))


(def ^:private input-value-base (partial value-base dom/input))
(def ^:private input-checked-base (partial checked-base dom/input))

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
        (update type-base
                (fn [f]
                  (f/comp g f))))))

(defn ^:no-doc no-optional [type]
  (throw (js/Error. "This type cannot be made optinoal.")))

(defn ^:no-doc type? [v]
  (and (map? v) (some? (::base v))))

(defn ^:no-doc native-type [type]
  (assert (or (nil? type) (string? type)) type)
  (case type
    ("checkbox" "radio")
    (new-type input-checked-base {:type type} no-optional)
        
    (new-type input-value-base {:type type} string-optional)))

(defn ^:no-doc add-type-attributes [type attrs]
  (assert (type? type) (str "Not a type: " (pr-str type)))
  (-> type
      (update-in type-attributes dom/merge-attributes attrs)))

(defn- option-value-placeholder [value & [ph-map]]
  (if-let [ph (get ph-map value)]
    ph
    (cond
      (nil? value) "" ;; must be the empty string, so that browsers can apply :required attribute for a value of nil.
      (string? value) value
      :else (pr-str value))))

(defrecord ^:private OptionValue [placeholder value])

(dom/defn-dom option
  "An item the can be used inside a [[select]] element to define a
  selectable option. The attribute `:value` can be any value, and the
  contents should be the text shown to the user."
  [attrs & contents]
  (let [v (:value attrs)
        placeholder (or (:placeholder attrs) ;; overridable, if pr-str is not suitable.
                        (option-value-placeholder v))]
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

(let [on-change (fn [[value ph-map] ev]
                  (let [options (.-options (.-target ev))
                        placeholder (.-value (.-target ev))
                        value (if-let [[value _] (first (filter #(= (second %) placeholder) ph-map))]
                                value
                                ;; should be there; complain?
                                placeholder)]
                    (c/return :state [value ph-map])))
      add-placeholder (fn [[st ph-map] a]
                        (if (instance? OptionValue a)
                          (c/return :state [st (assoc ph-map (:value a) (:placeholder a))])
                          (c/return :action a)))]
  (dom/defn-dom select "A select element that allows options to have arbitrary values."
    [attrs & options]
    ;; Note: the [[option]] elements from this namespace report their value and placeholder via an action.
    (c/with-state-as [value ph-map :local {}]
      (-> (apply (partial with-validate-fn value (partial with-invalid-attr dom/select))
                 {:value (option-value-placeholder value ph-map)
                  :onChange on-change}
                 options)
          (c/handle-action add-placeholder)))))

(dom/defn-dom textarea
  "An item representing its string state in a textual multiline input element."
  [attrs]
  (value-base dom/textarea attrs))

(dom/defn-dom form
  "The same as [[reacl-c.dom/form]]."
  [attrs & content]
  (apply dom/form attrs content))

(defn- lift-type [t]
  (if (type? t)
    t
    (native-type t)))

(dom/defn-dom input
  "An item representing its state in an input element depending on the given `:type` attribute."
  [attrs]
  (let [t (lift-type (:type attrs))]
    ((type-base t) (dom/merge-attributes (type-attributes t) (dissoc attrs :type)))))
