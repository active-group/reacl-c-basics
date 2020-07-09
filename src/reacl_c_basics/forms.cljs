(ns reacl-c-basics.forms
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [active.clojure.functions :as f]
            [reacl-c-basics.core :as core :include-macros true]))

(defn- checked-state [_ e]
  (.. e -target -checked))

(c/defn checkbox [& args]
  (c/with-state-as checked
    (let [[attrs children] (core/split-dom-attrs args)]
      (apply dom/input (merge {:type "checkbox"
                               :checked checked
                               :onchange checked-state}
                              attrs)
             children))))

(c/defn radio [& args]
  (let [[attrs children] (core/split-dom-attrs args)]
    (apply checkbox (merge {:type "radio"}
                           attrs)
           children)))

(defn- value-state [_ e]
  (c/return :state (.. e -target -value)))

(c/defn ^:private input-value [f fixed-attrs args]
  (c/with-state-as value
    ;; Note: args must not be var-args, to workaround various (!) clojurescript bugs with >= 21 args and IFns.
    (let [[attrs children] (core/split-dom-attrs args)]
      (apply f (merge fixed-attrs
                      {:value value
                       :onchange value-state}
                      attrs)
             children))))

(defn input-string [& args]
  (input-value dom/input {:type "text"} args))

(defn- input-parsed-lens*
  ([parse restrict [value text]]
   text)
  ([parse restrict [value old-text] new-text]
   (let [mod-text (restrict old-text new-text)
         v (parse mod-text)]
     [v mod-text])))

(c/defn ^:no-doc input-parsed* [parse restrict & args]
  (c/with-state-as [value text]
    ;; state = [value text] on the currently parsed value (or nil if not possible), and the actual text entered/shown.
    ;; (parse string) => value
    ;; (restrict prev-text next-text) => string, preventing some input while typing.
    (let [[attrs content] (core/split-dom-attrs args)]
      (c/focus (f/partial input-parsed-lens* parse restrict)
               (apply input-string attrs content)))))

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
  (c/defn ^:no-doc input-parsed [parse unparse restrict & args]
    (c/with-state-as value
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
                           (c/handle-action (f/partial set-focused parse unparse))))))))

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

(c/defn input-number [& args]
  (let [[attrs content] (core/split-dom-attrs args)]
    (apply input-parsed parse-number unparse-number unrestricted
           (merge {:type "number"}
                  attrs)
           content)))

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

(c/defn input-int [& args]
  (let [[attrs content] (core/split-dom-attrs args)]
    (apply input-parsed parse-int unparse-int
           ;; Note with type 'number' we don't even see a lot of the 'invalid' inputs; so restrict-int makes only sense for :text
           ;; But that should not be used - on smart phones, the number pad usually only shows up for :type "number".
           (if (= "text" (:type attrs))
             restrict-int
             unrestricted)
           (merge {:pattern #(or % (str integer-regex))}
                  attrs)
           content)))

(defn textarea [& args]
  (input-value dom/textarea {} args))

(c/defn ^:private select-opt-list :static [options]
  (apply c/fragment options))

(defn- select-string* [attrs options]
  (input-value dom/select {}
               [attrs (select-opt-list options)]))

(defn select-string [& args]
  (let [[attrs options] (core/split-dom-attrs args)]
    (select-string* attrs options)))

;; select and options, extended to work with arbitrary clojure values.
(defn- pr-str-lens
  ([values v] (pr-str v))
  ([values p s] (if-let [l (not-empty (filter #(= s (pr-str %)) values))]
                  (first l)
                  ;; selected value not in list? keep previous
                  p)))

(defn select [& args]
  ;; Note: use select-string if options are strings already (much faster)
  (let [[attrs options] (core/split-dom-attrs args)
        values (map (fn [opt]
                      ;; TODO: add dom/element-attrs, element-type or something? as a lens?
                      (assert (= "option" (:type opt)))
                      (:value (:attrs opt)))
                    options)
        _ (assert (= (count values) (count (set (map pr-str values)))) "Two or more options have the same 'pr-str' representation.")
        options_ (map (fn [opt] (update-in opt [:attrs :value] pr-str))
                      options)]
    (c/focus (f/partial pr-str-lens values)
             (select-string* attrs options_))))

(defn option [& args]
  (apply dom/option args))

(core/defn-dom submit-button [attrs & content]
  (apply dom/button (merge {:type "submit"} attrs)
         content))

(core/defn-dom reset-button [attrs & content]
  (apply dom/button (merge {:type "reset"} attrs)
         content))

(defn- submitter [f value ev]
  (.preventDefault ev)
  (f value))

(c/defn form [attrs & content]
  ;; :onreset a (c/return) value, automatically added when a :default is set.
  ;; :onsubmit a (fn [value]) => (c/return).
  (apply dom/form
         (cond-> (merge (when-let [default (:default attrs)]
                          {:onreset (f/constantly (c/return :state default))})
                        attrs)
           true (dissoc :default)
           (:onsubmit attrs) (assoc :onsubmit (f/partial submitter (:onsubmit attrs))))
         content))

(c/defn local-form [& args]
  ;; :default should be the default state value.
  ;; :onsubmit should be a fn of submitted value => return
  ;; :onreset is added and resets to the default value.
  (let [[attrs content] (core/split-dom-attrs args)]
    (c/isolate-state (:default attrs)
                     (apply form attrs
                            content))))
