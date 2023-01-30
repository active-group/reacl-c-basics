(ns reacl-c-basics.forms.bootstrap
  "Functions and items to utilize Bootstrap 5 forms and input styles and utilities."
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [reacl-c-basics.forms.core :as forms]
            [active.clojure.lens :as lens]
            [active.clojure.functions :as f])
  (:refer-clojure :exclude [range]))

(let [h (fn [user-onsubmit state ev]
          (if (.checkValidity (.-target ev))
            (c/call user-onsubmit ev)
            (do (.preventDefault ev)
                (c/return :state [(first state) true]))))]
  (dom/defn-dom validated-form*
    "Like [[validated-form]], but with a public 'was validated' state,
   which is set when an invalid form is attempted to be
   submitted. Unlike [[validated-form]] that state is not reset
   automatically."
    [attrs & content]
    (c/with-state-as [_ was-validated?]
      (forms/form (dom/merge-attributes
                   {:noValidate true
                    :class (if was-validated? "was-validated" "")
                    :onSubmit (f/partial h (:onSubmit attrs))}
                   (dissoc attrs :onSubmit))
                  (apply c/fragment content)))))

(let [h (fn [user-onsubmit state ev]
          (c/merge-returned (c/return :state [(first state) false])
                            (c/call user-onsubmit ev)))]
  (dom/defn-dom validated-form
    "A form, that disables the default html5 form validation, and adds
   the 'was-validated' class to the form in case the submit of an invalid
   form is attempted. Removes that class when a valid form is submitted.

   To declare the validity of form inputs, use `:required`,
   `:pattern`, `:invalid`, `:validate` etc. attributes on the input
   items. To show a more detailed error message to the user,
   add [[invalid-feedback]] or [[invalid-tooltip]] next to the input
   elements."
    [attrs & content]
    (assert (nil? (:onsubmit attrs)) "Use :onSubmit instead of :onsubmit")
    (c/local-state false
                   (validated-form* (dom/merge-attributes
                                     {:onSubmit (f/partial h (:onSubmit attrs))}
                                     (dissoc attrs :onSubmit))
                                    (c/focus lens/first (apply c/fragment content))))))

(dom/def-dom invalid-feedback dom/div {:class "invalid-feedback"})
(dom/def-dom valid-feedback dom/div {:class "valid-feedback"})

(dom/def-dom invalid-tooltip dom/div {:class "invalid-tooltip"})
(dom/def-dom valid-tooltip dom/div {:class "valid-tooltip"})

(dom/def-dom label dom/label {:class "form-label"})
(dom/def-dom ^{:doc "A form label that is also a column, to be used to make 'horizontal forms'. Also add a column class like 'col-2' to this item."}
  col-label dom/label {:class "col-form-label"})

(defn- form-control [attrs & content]
  (let [class (if (or (:readOnly attrs) (:readonly attrs))
                "form-control-plaintext"
                "form-control")]
    (apply forms/input (dom/merge-attributes {:class class} attrs)
           content)))

;; Note: unfortunately will not work with :type enum and other special types.
(dom/def-dom input form-control {})
(dom/def-dom textarea form-control {})

(dom/def-dom text dom/div {:class "form-text"})

(dom/def-dom select forms/select {:class "form-select"})

;; also for form-check: form-check-inline form-check-reverse

(dom/def-dom check dom/div {:class "form-check"})
(dom/def-dom check-input forms/input {:class "form-check-input"})
(dom/def-dom check-label dom/label {:class "form-check-label"})

(dom/def-dom switch dom/div {:class "form-check form-switch"})

(dom/def-dom ^:private check-button-input forms/input {:type "checkbox" :class "btn-check"})
(dom/def-dom ^:private check-button-label dom/label {:class "btn"})

(dom/defn-dom toggle-button "A button that act like a checkbox form input, toggling its boolean state."
  [attrs & contents]
  (c/fragment (check-button-input {:id (:id attrs)
                                   :name (:name attrs)
                                   :autoComplete (:autoComplete attrs)})
              (check-button-label (dom/merge-attributes {:for (:id attrs)}
                                                        (dissoc attrs :id :name :autoComplete)))))

(dom/def-dom range forms/input {:class "form-range"})

(dom/def-dom group dom/div {:class "input-group"})
(dom/def-dom group-text dom/span {:class "input-group-text"})

(dom/def-dom floating dom/div {:class "form-floating"})
