(ns reacl-c-basics.forms.parsed
  "Utility to create specialized input fields via parsing of the native value."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.forms.core :as core]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [clojure.string :as str]))

(defn parse-error
  "Returns the error to be thrown on parse errors."
  [msg & [value]]
  (js/Error. msg))

(let [state-change
      (fn [parse onerror _ [value local]]
        (try (let [text (or (:text local) "")
                   v (parse text)]
               (c/merge-returned (c/return :state [v {:text text :previous v :error nil}])
                                 (if (some? (:error local))
                                   (c/call onerror nil)
                                   (c/return))))
             (catch :default e
               (c/merge-returned (c/return :state [value (assoc local :error e)])
                                 (c/call onerror e)))))
      
      actions
      (fn [unparse onerror [value local] a]
        (if (= ::reset a)
          (cond-> (c/return :state [value (assoc local
                                                 :error nil
                                                 :text (unparse value))])
            (:error local) (c/merge-returned (c/call onerror nil)))
          (c/return :action a)))]
  (defn ^:no-doc input-parsed-base
    "Special attributes:
   :base must be a fn[attrs] creating the unparsed input
   :unparse must return a string
   if :parse throws, (:onParseError exn) is emitted, otherwise (:onParseError nil)."
    [attrs]
    (c/with-state-as [value local :local {:text nil :previous nil :error nil}]
      (let [parse (:parse attrs)
            unparse (:unparse attrs)]
        (c/fragment
         ;; init initially, or reinit when new value from outside.
         (when (or (not= (:previous local) value)
                   (nil? (:text local)))
           (c/init [value {:text (unparse value) :previous value :error nil}]))
       
         (-> (c/focus (lens/>> lens/second :text (lens/default ""))
                      ((:base attrs) (dom/merge-attributes {:onBlur (f/constantly (c/return :action ::reset))}
                                                           (dissoc attrs :onParseError :base :parse :unparse))))
             (c/handle-state-change (f/partial state-change parse (:onParseError attrs)))
             (c/handle-action (f/partial actions unparse (:onParseError attrs)))))))))

(defn- restricted-lens
  ([f value] value)
  ([f old new] (f old new)))

(defn- restricted-input [f attrs]
  (c/focus (f/partial restricted-lens f)
           (core/input attrs)))

(dom/defn-dom input-parsed
  "Use attributes :parse and :unparse, and optionally :restrict"
  [attrs]
  ;; Note: different error signaling than old forms/input-parsed (old one was returning nil; new one: exception)
  (input-parsed-base (dom/merge-attributes {:base (if-let [f (:restrict attrs)]
                                                    (f/partial restricted-input f)
                                                    core/input)
                                            :type "text"}
                                           (dissoc attrs :restrict))))
