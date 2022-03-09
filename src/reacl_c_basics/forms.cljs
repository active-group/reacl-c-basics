(ns reacl-c-basics.forms
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.functions :as f]
            [reacl-c-basics.core :as core :include-macros true]))

(defn- checked-state [_ e]
  (.. e -target -checked))

(dom/defn-dom checkbox [attrs & children]
  (c/with-state-as checked
    (apply dom/input (dom/merge-attributes {:type "checkbox"
                                            :checked checked
                                            :onchange checked-state}
                                           attrs)
           children)))

(dom/defn-dom radio [attrs & children]
  (apply checkbox (dom/merge-attributes {:type "radio"}
                                        attrs)
         children))

(defn- value-state [_ e]
  (c/return :state (.. e -target -value)))

(defn ^:private input-value [f fixed-attrs attrs children]
  (c/with-state-as value
    (apply f (dom/merge-attributes fixed-attrs
                                   {:value value
                                    :onchange value-state}
                                   attrs)
           children)))

(dom/defn-dom input-string [attrs & children]
  (input-value dom/input {:type "text"} attrs children))

(defn- input-parsed-lens*
  ([parse restrict [value text]]
   text)
  ([parse restrict [value old-text] new-text]
   (let [mod-text (restrict old-text new-text)
         v (parse mod-text)]
     [v mod-text])))

(defn ^:no-doc input-parsed* [parse restrict & args]
  ;; state = [value text] on the currently parsed value (or nil if not possible), and the actual text entered/shown.
  ;; (parse string) => value
  ;; (restrict prev-text next-text) => string, preventing some input while typing.
  (let [[attrs content] (core/split-dom-attrs args)]
    (c/focus (f/partial input-parsed-lens* parse restrict)
             (apply input-string attrs content))))

(defn- input-parsed-lens
  ([unparse [value {text :text focused? :focused?}]]
   [value (if focused?
            text
            (unparse value))])
  ([unparse [value {text :text focused? :focused?}] [new-value new-text]]
   [new-value {:text (if focused?
                       new-text
                       (unparse new-value))
               :focused? focused?}]))

(defrecord ^:private SetFocused [value])

(let [set-focused-a (fn [v state _]
                      (c/return :action (SetFocused. v)))
      set-focused (fn [parse unparse state a]
                    (if (instance? SetFocused a)
                      (c/return :state (cond-> (assoc-in state [1 :focused?] (:value a))
                                         ;; on focus, init local :text state.
                                         (:value a)
                                         (assoc-in [1 :text] (unparse (first state)))
                                         ;; on blur, set value again.
                                         (not (:value a))
                                         (assoc 0 (parse (get-in state [1 :text])))
                                         ))
                      (c/return :action a)))]
  (defn ^:no-doc input-parsed [parse unparse restrict & args]
    ;; this keeps the text the user entered, iff and as long as the input has the focus, and changes it to (unparse value) otherwise.
    ;; Note: use input-parsed* if you need to distinguish between 'no input' and 'invalid input'.
    (let [[attrs content] (core/split-dom-attrs args)]
      (c/local-state {:text ""
                      :focused? false}
                     (-> (c/focus (f/partial input-parsed-lens unparse)
                                  (apply input-parsed* parse restrict
                                         (assoc attrs
                                                :onfocus (f/partial set-focused-a true)
                                                :onblur (f/partial set-focused-a false))
                                         content))
                         (c/handle-action (f/partial set-focused parse unparse)))))))

(defn- parse-number [s]
  ;; Note: "" parses as NaN although it's not isNaN; parseFloat ignores trailing extra chars; but isNaN does not:
  (let [x (when (not (js/isNaN s))
            (js/parseFloat s))]
    (if (js/isNaN x)
      nil
      x)))

(defn- unparse-number [v]
  (if v (str v) ""))

(defn- unrestricted [old new]
  new)

(dom/defn-dom input-number [attrs & content]
  (apply input-parsed parse-number unparse-number unrestricted
         (dom/merge-attributes {:type "number"}
                               attrs)
         content))

(def ^:private integer-regex #"^[-]?\d*$")

(defn ^:no-doc parse-int [s]
  ;; see parse-number above - we additionally set a regex :pattern attribute (allegedly helps mobiles to show a number input)
  ;; and use the same here, because otherwise we would parse invalid inputs.
  (let [x (when (and (not (js/isNaN s))
                     (.test integer-regex s))
            (js/parseInt s))]
    (if (js/isNaN x)
      nil
      (if (integer? x) ;; probably always the case
        x
        nil))))

(defn ^:no-doc unparse-int [v]
  (if v (str v) ""))

(defn ^:no-doc restrict-int [old new]
  (apply str (filter #(.test #"[0-9-]" %) new)))

(dom/defn-dom input-int [attrs & content]
  (apply input-parsed parse-int unparse-int
         ;; Note with type 'number' we don't even see a lot of the 'invalid' inputs; so restrict-int makes only sense for :text
         ;; But that should not be used - on smart phones, the number pad usually only shows up for :type "number".
         (if (= "text" (:type attrs))
           restrict-int
           unrestricted)
         (dom/merge-attributes {:pattern (str integer-regex)}
                               attrs)
         content))

(dom/defn-dom textarea [attrs & children]
  (input-value dom/textarea {} attrs children))

(c/defn-item ^:private select-opt-list :static [options]
  (apply c/fragment options))

(defn- select-string* [attrs options]
  (input-value dom/select {}
               attrs [(select-opt-list options)]))

;; select and options, extended to work with arbitrary clojure values.
(defn- option-value-lens
  ([values v]
   (if (contains? values v)
     (pr-str v)
     v))
  ([values p s] (if-let [l (not-empty (filter #(= s (pr-str %)) values))]
                  (first l)
                  ;; selected value not in list? keep previous
                  p)))

(defrecord ^:private Option [attrs args])
(defrecord ^:private OptGroup [attrs args])

(defn- map-option-tags [options f]
  (map (fn [opt]
         (cond
           (instance? Option opt)
           (apply dom/option (update (:attrs opt) :value f) (:args opt))

           (instance? OptGroup opt)
           (apply dom/optgroup (:attrs opt) (map-option-tags (:args opt) f))

           ;; don't mess with reacl-c internal representation of other things:
           :else opt))
       options))

(defn- get-option-values [options]
  (mapcat (fn [opt]
            (cond
              (instance? Option opt) [(:value (:attrs opt))]

              (instance? OptGroup opt) (get-option-values (:args opt))

              ;; don't mess with reacl-c internal representation of other things:
              :else nil))
          options))

(defn select-string
  "Like [[dom/select]], but the selected (string) value is the state
  of the returned item."
  [attrs & options]
  (let [[attrs options] (if-not (dom/dom-attributes? attrs)
                          [{} (cons attrs options)]
                          [attrs options])]
    (select-string* attrs (map-option-tags options identity))))

(defn select
  "Like dom/select, but with the selected value as the state of the
  returned item, and if [[option]] and [[optgroup]] are used, then
  this works with (almost) arbitrary clojure values as selectalbe
  values. See [[select-string]] if the values are only strings."
  [attrs & options]
  (let [[attrs options] (if-not (dom/dom-attributes? attrs)
                          [{} (cons attrs options)]
                          [attrs options])
        values (get-option-values options)
        options_ (map-option-tags options pr-str)]
    (c/focus (f/partial option-value-lens (set values))
             (select-string* attrs options_))))

(defn option
  "Like dom/option, but can only be used for [[select]] or [[select-string]]."
  [attrs & args]
  ;; an option without a value does not really make sense.
  (assert (dom/dom-attributes? attrs))
  (assert (contains? attrs :value))
  (Option. attrs args))

(defn optgroup
  "Like dom/optgroup, but can only be used for [[select]] or [[select-string]]."
  [attrs & args]
  (if-not (dom/dom-attributes? attrs)
    (OptGroup. {} (cons attrs args))
    (OptGroup. attrs args)))

(dom/defn-dom submit-button [attrs & content]
  (apply dom/button (dom/merge-attributes {:type "submit"} attrs)
         content))

(dom/defn-dom reset-button [attrs & content]
  (apply dom/button (dom/merge-attributes {:type "reset"} attrs)
         content))

(defn- submitter [f value ev]
  (.preventDefault ev)
  (f value))

(c/defn-item form [attrs & content]
  ;; :onreset a (c/return) value, automatically added when a :default is set.
  ;; :onsubmit a (fn [value]) => (c/return).
  (apply dom/form
         (cond-> (merge (when-let [default (:default attrs)]
                          {:onreset (f/constantly (c/return :state default))})
                        attrs)
           true (dissoc :default)
           (:onsubmit attrs) (assoc :onsubmit (f/partial submitter (:onsubmit attrs))))
         content))

(dom/defn-dom local-form [attrs & content]
  ;; :default should be the default state value.
  ;; :onsubmit should be a fn of submitted value => return
  ;; :onreset is added and resets to the default value.
  (c/isolate-state (:default attrs)
                   (apply form attrs
                          content)))
