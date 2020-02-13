(ns reacl-c-basics.forms
  (:require [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c-basics.core :as core :include-macros true]))

(defn- checked-state [_ e]
  (c/return :state (.. e -target -checked)))

(c/defn-dynamic checkbox checked [& args]
  (let [[attrs children] (core/split-dom-attrs args)]
    (apply dom/input (merge {:type "checkbox"
                             :value checked
                             :onchange checked-state}
                            attrs)
           children)))

(c/defn-named radio [& args]
  (let [[attrs children] (core/split-dom-attrs args)]
    (apply checkbox (merge {:type "radio"}
                           attrs)
           children)))

(defn- value-state [_ e]
  (c/return :state (.. e -target -value)))

(c/defn-dynamic ^:private input-value value [f fixed-attrs & args]
  (let [[attrs children] (core/split-dom-attrs args)]
    (apply f (merge fixed-attrs
                    {:value value
                     :onchange value-state}
                    attrs)
           children)))

(defn input-string [& args]
  (apply input-value dom/input {:type "text"} args))

(defn- input-parsed-lens
  ([parse restrict unparse [value {text :text pub-value :pub}]]
   ;; keep with the text the user entered, until a value comes in from outside that the input did not publish itself.
   (if (not= value pub-value)
     (unparse value)
     text))
  ([parse restrict unparse [value {text :text pub-value :pub}] new-text]
   (if (not= text new-text)
     (let [mod-text (restrict text new-text)
           new (parse mod-text)]
       [new {:text mod-text :pub new}])
     [value {:text text :pub pub-value}])))

(c/defn-dynamic ^:no-doc input-parsed value [parse unparse restrict & args]
  ;; (unparse state) => string
  ;; (parse string) => value
  ;; (restrict string) => string, preventing some input while typing.
  (c/local-state {:text (unparse value)
                  :pub value}
                 (c/focus (c/partial input-parsed-lens parse restrict unparse)
                          (apply input-string args))))

(defn- parse-number [s]
  ;; Note: "" parses as NaN although it's not isNaN; parseFloat ignores trailing extra chars; but isNaN does not:
  (let [x (when (not (js/isNaN s))
            (.parseFloat js/Number s))]
    (if (js/isNaN x)
      nil
      x)))

(defn- unparse-number [v]
  (if v (str v) ""))

(defn- unrestricted [old new]
  new)

(c/defn-named input-number [& args]
  (let [[attrs content] (core/split-dom-attrs args)]
    (apply input-parsed parse-number unparse-number unrestricted
           (merge {:type "number"}
                  attrs)
           content)))

(def ^:private integer-regex #"^[-]?\d*$")

(defn- parse-int [s]
  ;; see parse-number above - we additionally set a regex :pattern attribute (allegedly helps mobiles to show a number input)
  ;; and use the same here, because otherwise we would parse invalid inputs.
  (let [x (when (and (not (js/isNaN s))
                     (.test integer-regex s))
            (.parseInt js/Number s))]
    (if (js/isNaN x)
      nil
      (if (integer? x) ;; probably always the case
        x
        nil))))

(defn- unparse-int [v]
  (if v (str v) ""))

(defn- restrict-int-chars [old new]
  (apply str (filter #(.test #"[0-9-]" %) new)))

(c/defn-named input-int [& args]
  (let [[attrs content] (core/split-dom-attrs args)]
    (apply input-parsed parse-int unparse-int
           ;; Note with type 'number' we don't even see a lot of the 'invalid' inputs; so restrict-int-chars makes only sense for :text
           ;; But that should not be used - on smart phones, the number pad usually only shows up for :type "number".
           (if (= "text" (:type attrs))
             restrict-int-chars
             unrestricted)
           (merge {:pattern #(or % integer-regex)}
                  attrs)
           content)))

(defn textarea [& args]
  (apply input-value dom/textarea {} args))

;; select and options, extended to work with arbitrary clojure values.
(defn- pr-str-lens
  ([values v] (pr-str v))
  ([values p s] (if-let [l (not-empty (filter #(= s (pr-str %)) values))]
                  (first l)
                  ;; selected value not in list? keep previous
                  p)))

(defn select [& args]
  (let [[attrs options] (core/split-dom-attrs args)
        values (map (fn [opt]
                      ;; TODO: add dom/element-attrs, element-type or something? as a lens?
                      (assert (= "option" (:type opt)))
                      (:value (:attrs opt)))
                    options)
        _ (assert (= (count values) (count (set (map pr-str values)))) "Two or more options have the same 'pr-str' representation.")
        options_ (map (fn [opt] (update-in opt [:attrs :value] pr-str))
                      options)]
    (c/focus (c/partial pr-str-lens values)
             (apply input-value dom/select {} (cons attrs options_)))))

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

(c/defn-named form [attrs & content]
  ;; :onreset a (c/return) value, automatically added when a :default is set.
  ;; :onsubmit a (fn [value]) => (c/return).
  (apply dom/form
         (cond-> (merge (when-let [default (:default attrs)]
                          {:onreset (c/constantly (c/return :state default))})
                        attrs)
           true (dissoc :default)
           (:onsubmit attrs) (assoc :onsubmit (c/partial submitter (:onsubmit attrs))))
         content))

(c/defn-named local-form [& args]
  ;; :default should be the default state value.
  ;; :onsubmit should be a fn of submitted value => return
  ;; :onreset is added and resets to the default value.
  (let [[attrs content] (core/split-dom-attrs args)]
    (c/isolate-state (:default attrs)
                     (apply form attrs
                            content))))
