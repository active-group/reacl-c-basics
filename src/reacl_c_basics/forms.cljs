(ns reacl-c-basics.forms
  (:require [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c-basics.core :as core :include-macros true]))

(defn- checked-state [e]
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

(defn- value-state [e]
  (c/return :state (.. e -target -value)))

(c/defn-dynamic ^:private input-value value [f & args]
  (let [[attrs children] (core/split-dom-attrs args)]
    (apply f (merge {:type "text"
                     :value value
                     :onchange value-state}
                    attrs)
           children)))

(defn input-string [& args]
  (apply input-value dom/input args))

(defn textarea [& args]
  (apply input-value dom/textarea args))

(core/defn-dom submit-button [attrs & content]
  (apply dom/button (merge {:type "submit"} attrs)
         content))

(core/defn-dom reset-button [attrs & content]
  (apply dom/button (merge {:type "reset"} attrs)
         content))

(defn- submitter [f value ev]
  (.preventDefault ev)
  (f value))

(c/defn-dynamic form value [attrs & content]
  ;; :onreset a (c/return) value, automatically added when a :default is set.
  ;; :onsubmit a (fn [value]) => (c/return).
  (apply dom/form
         (cond-> (merge (when-let [default (:default attrs)]
                          {:onreset (c/constantly (c/return :state default))})
                        attrs)
           (:onsubmit attrs) (assoc :onsubmit (c/partial submitter (:onsubmit attrs) value)))
         content))

(c/defn-named local-form [default & args]
  ;; :onsubmit should be a fn of submitted value => return
  ;; :onreset is added and resets to the default value.
  (let [[attrs content] (core/split-dom-attrs args)]
    (c/isolate-state default
                     (apply form (assoc attrs :default default)
                            content))))
