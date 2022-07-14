(ns reacl-c-basics.forms.parsed
  "Utility to create specialized input fields via parsing of the native value."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.forms.core :as core]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]))

(defn parse-error
  "Returns the error to be thrown on parse errors."
  [msg & [value]]
  (js/Error. msg))

(let [state-change
      (fn [parse onerror _ [value local]]
        (try (let [text (:text local)
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
    (let [parse (:parse attrs)
          unparse (:unparse attrs)]
      ;; Note that :text may not be a string, if :base is something 'exotic'.
      (c/with-state-as [value local :local {:text (unparse nil) :previous nil :error nil}]
        (c/fragment
         ;; init initially, or reinit when new value from outside.
         (when (not= (:previous local) value)
           (c/init [value {:text (unparse value) :previous value :error nil}]))
       
         (-> (c/focus (lens/>> lens/second :text)
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
  "A [[core/input]] element that parses the user input into a different type of value.

   An attribute `:parse` must be set to a function that takes the
   native value of the input and parses it into something else, or it
   may throw a [[parse-error]] if that is not possible.

   An attribute `:unparse` must be set to a function to reverse the
   parsing, and it must return a default empty value (usually an empty
   string) for the input when called with `nil`.

   Note that the state of the returned item is always set to the parse
   result (unless parsing throws), but the native value of input
   remains unchanged until the input 'blurs' (loses focus). Only then
   the native value is set to the result of the unparse function. This
   allows the user to have an irregular intermediate value while
   editing, e.g. replacing the \"2\" in an integer input that shows
   \"200\".

   The optional `:restrict` attribute can be set to a function that
   restricts the possible native value of the input while the user
   edits it. It is called with a previous value and a new value upon a
   change made by the user, and must return the actual new native
   value for the input."
  [attrs]
  (assert (some? (:parse attrs)) "Missing required attribute :parse.")
  (assert (some? (:unparse attrs)) "Missing required attribute :unparse.")
  ;; Note: different error signaling than old forms/input-parsed (old one was returning nil; new one: exception)
  (input-parsed-base (dom/merge-attributes {:base (if-let [f (:restrict attrs)]
                                                    (f/partial restricted-input f)
                                                    core/input)
                                            :type "text"}
                                           (dissoc attrs :restrict))))
