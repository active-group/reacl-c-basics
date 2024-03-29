(ns reacl-c-basics.forms.validation
  "EXPERIMENTAL

  Utilities for a more customized display of form validity.

  Note: the basic validity of input fields can be defined with the
  `:invalid` and `:validate` attribtues
  of [[reacl-c-basics.forms.core/input]] etc.

  Note: the validation message for native validations (`:pattern`,
  `:required` etc) can be modified by using `setCustomValidity` in
  `:onInvalid` events depending on the `validity` property.

  For visual styling there are the `:required` and `:options` CSS
  pseudo-classes, as well as `:valid` and `:invalid`.

  For showing validation problems differently than the browser does by
  default, use [[with-validity]] and the derived
  utils [[form-with-validity]] or [[append-validity]].

  To trigger the display of validation problems before submitting a
  form, use [[report-validity!]].

  See https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Constraint_validation
  "
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.forms.core :as forms]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f]))

;; Note: this is still not 'perfect', esp. for the way to hide validation messages again; when to do that in general?

(c/defn-effect report-validity! "Like [[check-validity!]],
  but validation errors are also shown to the user. The browser will
  show them natively in a popup, or you can customize the visual
  presentation with [[with-validity]]. This will automatically be
  called when the user tries to submit a form."
  [ref]
  (let [elem (c/deref ref)]
    (.reportValidity elem)))

(c/defn-effect check-validity!
  "Checks whether the referenced form or input element is valid. If
  not, the browser fires a cancelable invalid event at the element,
  and this effect returns false."
  [ref]
  (let [elem (c/deref ref)]
    (.checkValidity elem)))

(c/defn-effect validation-message
  "Gets the current validation message from a reference to a form or
  input element."
  [ref]
  (let [elem (c/deref ref)]
    (.-validationMessage elem)))

(let [get-msg (fn [element]
                (not-empty (.-validationMessage element)))
      oninvalid (fn [state ev]
                  (.preventDefault ev) ;; do not show native validity popup of browser
                  (c/return :action [::msg (get-msg (.-target ev))]))
      handle-reset (fn [state a]
                     (if (= ::reset a)
                       (c/return :state [(first state) nil])
                       (c/return :action a)))
      dyn (fn [[_ validation-message] f]
            (-> (c/focus lens/first
                         ;; apparently, there is no even when an input becomes valid again :-/
                         ;; offer the user a ::reset action for that.
                         (f {:onInvalid oninvalid}
                            validation-message
                            ::reset))
                (c/handle-action handle-reset)))
      set-msg (fn [[state _] a]
                (if (and (vector? a) (= ::msg (first a)))
                  (c/return :state [state (second a)])
                  (c/return :action a)))]
  (defn with-validity
    "An item that will call `(f attrs validation-msg reset-action)`, where
  attrs contains and `:onInvalid` event handler that must be used in a
  form, input element or anything that contains input
  elements. `validation-msg` will then be non-nil when form validation
  fails and the problem should be displayed to the user. You can emit
  'reset-action' to reset the validation message to nil.

  This turns off the default behaviour of the browser to show these
  messages."
    [f]
    ;; Note: message only 'appears' after form submit (or reportValidity); no initialization.
    (c/local-state
     nil
     (-> (c/dynamic dyn f)
         (c/handle-action set-msg)))))

(let [g (fn [item-f attrs1 f attrs2 msg reset-action]
          (item-f (dom/merge-attributes attrs2 attrs1)
                  (f msg reset-action)))]
  (defn- item-with-validity
    [item-f attrs f]
    (with-validity (f/partial g item-f attrs f))))

(defn form-with-validity
  "A form item with contents `(f validation-msg reset-action)`, where
  `validation-msg` is nil or the description of validation errors that
  prevent the forms from being submitted."
  ([f]
   (form-with-validity {} f))
  ([attrs f]
   (item-with-validity forms/form attrs f)))

(defn div-with-validity
  "A div item with contents `(f validation-msg reset-action)`, where
  `validation-msg` is nil or the description of validation errors in
  one of the contained input elements."
  ([f]
   (div-with-validity {} f))
  ([attrs f]
   (item-with-validity dom/div attrs f)))

(let [g (fn [item f msg]
          (c/fragment item (f msg)))]
  (defn append-validity
    "A div item with the given child item (which should usually be an
  input element), followed by `(f validation-msg reset-action)` where
  `validation-msg` is any validation error in that input element."
    ([item f]
     (append-validity {} item f))
    ([attrs item f]
     (div-with-validity attrs (f/partial g item f)))))

(let [g (fn [f item msg]
          (c/fragment (f msg) item))]
  (defn prepend-validity
    "A div item with the given child item (which should usually be an
  input element), prepended with `(f validation-msg reset-action)`
  where `validation-msg` is any validation error in that input
  element."
    ([f item]
     (prepend-validity {} f item))
    ([attrs f item]
     (div-with-validity attrs (f/partial g f item)))))
