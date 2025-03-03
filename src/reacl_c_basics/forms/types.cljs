(ns reacl-c-basics.forms.types
  "Input types extend over the `:type` attribute
  of [[reacl-c-basics.forms.core/input]] items, giving more control
  over the data types of the state value, and to some extend over the
  ui controls to enter those values. The type system defined here is
  also extensible.

  For example, you can ask the user for an integer via

  ```
  (c/isolate-state 0
    (core/input {:type types/integer}))
  ```
  "
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom :include-macros true]
            [active.clojure.functions :as f]
            [reacl-c-basics.forms.core :as core]
            [reacl-c-basics.forms.parsed :as parsed]
            [clojure.string :as str])
  (:refer-clojure :exclude [extend-type boolean]))

(defn new-type
  "Creates a new input type, given a basic dom
  function (e.g. core/input), a map of default attributes (e.g. a
  string value for the `:type` attribute), and a function that takes
  this type and returns a type that also accepts `nil` as a value. The
  `to-optional` function may throw, if the type cannot be extended to
  accept `nil`."
  [base-fn default-attrs to-optional]
  (core/new-type base-fn default-attrs to-optional))

(defn add-attributes
  "Adds additional default attributes for the dom element used for the given type."
  [type attrs]
  (core/add-type-attributes type attrs))

(defn update-type-base
  "Calls f with the current base function and replaces it with the result."
  [type f]
  (update type core/type-base f))

(defn update-to-optional
  [type f]
  "Calls f with the current `to-optional` function and replaces it with the result."
  (update type core/type-to-optional f))

(defn native-type
  "Creates an input type for one of the native 'types' supported by a
  dom/input, i.e. \"text\", \"number\", \"checkbox\" etc. Note that
  not all types are supported by all browsers."
  [type]
  (core/native-type type))

(let [parse (fn [par s]
              (if (empty? s)
                nil
                (par s)))
      unparse (fn [unp v]
                (if (nil? v)
                  ""
                  (unp v)))]
  (defn- parsed-optional
    [type]
    (-> type
        (assoc core/type-to-optional identity)
        (update core/type-attributes
                (fn [attrs]
                  (-> attrs
                      (update :parse
                              (fn [p]
                                (f/partial parse p)))
                      (update :unparse
                              (fn [u]
                                (f/partial unparse u)))))))))

(defn parsed-type
  "Creates an input that requires parsing and unparsing of the native
  value entered by the user (usually a
  string). See [[parsed/input-parsed]] for details on the parse and
  unparse functions."
  [base-type parse unparse]
  ;; Note: not sure how/if this works when base-type is not a native-type
  (assert (core/type? base-type) (str "Not a type: " (pr-str base-type)))
  (new-type parsed/input-parsed-base
            (dom/merge-attributes {:base (core/type-base base-type)
                                   :parse parse
                                   :unparse unparse}
                                  (core/type-attributes base-type))
            parsed-optional))

(defn- make-parsed [type]
  (if (some? (:parse (core/type-attributes type)))
    type
    (parsed-type type identity identity)))

(defn extend-type
  "Creates an input type that can allow for more values than the given
  one. The function `parse` will be called with the parse function of
  the given type and the string entered by the user, and the function
  `unparse` alike."
  [type parse unparse]
  (-> (make-parsed type)
      (update-in [core/type-attributes :parse]
                 (fn [g]
                   (f/partial parse g)))
      (update-in [core/type-attributes :unparse]
                 (fn [g]
                   (f/partial unparse g)))))

(defn optional
  "Adds 'nil' as a possible value to the given input type. Note that
  this is may not possible for all types."
  [type]
  ((core/type-to-optional type) type))

(let [call (fn [parse s]
             (parse s))]
  (defn restrict-type
    "Creates an input that allows to restrict the parsed value of the
  given type further, by threading it though the given function `f`,
  which may throw [[parsed/parse-error]]. Note that it can be more
  intuitive for the user to set the input element attribute `:invalid`
  in some cases."
    [type f]
    (extend-type type
      (f/comp f call)
      call)))

(def ^{:doc "An input type for a number."} number
  (-> (parsed-type (native-type "number")
                   (fn parse [s]
                     ;; Note: not called for 'optional parsed' and an empty.
                     ;; Note: "" parses as NaN although it's not isNaN; parseFloat ignores trailing extra chars; but isNaN does not:
                     (let [x (when (not (js/isNaN s))
                               (js/parseFloat s))]
                       (if (or (nil? x) (js/isNaN x))
                         (throw (parsed/parse-error "Not a number." s))
                         x)))
                   str)
      (add-attributes {:step "any"})))

(def ^{:doc "An input type for an optional number."} opt-number (optional number))

(def ^{:doc "An input type that requires the user to enter an integer
number. For an input type that for example just removes decimal
places, use [[integer]]."} strict-integer
  (-> number
      (restrict-type (fn [v]
                       (if (integer? v)
                         v
                         (throw (parsed/parse-error "Not an integer." v)))))
      (add-attributes {:step "1"})))
(def ^{:doc "An optional [[strict-integer]]."} opt-strict-integer (optional strict-integer))

(def ^{:doc "An input type for a integer number."}
  integer
  (-> number
      (restrict-type long)
      (add-attributes {:step "1"})))

(def ^{:doc "An input type for an optional [[integer]]."}
  opt-integer (optional integer))

(def ^{:doc "An input type for a string."}
  string (native-type "text"))

(def ^{:doc "An input type for an optional [[string]]."}
  opt-string (optional string))

(def ^{:doc "An input type for an optional, but non-empty string,
  i.e. the value is never an empty string. This also trims the text
  entered, so pure whitespace becomes `nil` as well."}
  opt-non-empty-string
  (-> opt-string
      (restrict-type
       (fn [v]
         (let [r (str/trim v)]
           (if (empty? r)
             nil
             r))))))

(def ^{:doc "An input for a boolean value, represented as a checkbox."}
  boolean (native-type "checkbox"))

(def ^{:doc "An input type for a string, represented in a multiline textbox element."}
  multiline-string (new-type core/textarea {} core/string-optional))

(defn- simple-select
  "A simplified [[select]] item, that allows for an `:options`
  attribute to define a sequence of `[value label]` tuples instead of
  using [[option]] items directly."
  [attrs]
  (apply core/select (dissoc attrs :options :optional?)
         (when (and (:optional? attrs) (not (some nil? (map first (:options attrs)))))
           (core/option {:value nil} ""))
         (map (fn [[value label]]
                (core/option {:value value} label))
              (:options attrs))))

(defn- simple-select-optional [type]
  (-> type
      (assoc core/type-to-optional identity)
      (update core/type-attributes
              assoc :optional? true)))

(defn enum
  "An input type for one of a set of arbitrary values. The argument
  `values-labels` must be a a sequence of tuples `[value text]`
  specifying the value selected and a string that represents the value
  for the user."
  [values-labels]
  (new-type simple-select {:options values-labels}
            simple-select-optional))

(defn string-enum
  "An input type for one of the given strings."
  [strings]
  (enum (map (partial repeat 2) strings)))

(let [parse (fn [decimals parse-num s]
              (js/parseFloat (.toFixed (parse-num s) decimals)))
      unparse (fn [decimals unparse-num v]
                (if (nil? v)
                  ""
                  ;; Note: this might lead to not actually showing the real value to the user
                  (.toFixed v decimals)))]
  (defn fixnum
    "An input type for a float, but when entered and displayed, exactly the given number of
  decimal places are used or shown respectively."
    [decimals]
    (extend-type number
      (f/partial parse decimals)
      (f/partial unparse decimals))))

(let [parse (fn [decimals parse-fixnum s]
              (long (* (double (parse-fixnum s))
                       (js/Math.exp 10 decimals))))
      unparse (fn [decimals unparse-fixnum v]
                (if (nil? v)
                  ""
                  (unparse-fixnum (/ (double v)
                                     (js/Math.exp 10 decimals)))))]
  (defn decimal
    "A input type for an integer, but represented and entered as `(/ v (^ 10
  decimals)`. E.g. 234 with 2 decimals is shown and entered as
  \"2.34\"."
    [decimals]
    (extend-type (fixnum decimals)
      (f/partial parse decimals)
      (f/partial unparse decimals))))
