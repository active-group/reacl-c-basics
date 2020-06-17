(ns reacl-c-basics.core
  (:require [reacl-c.core :as c :include-macros true]))

(defmacro defn-dom
  "Defines a function similar to the dom functions, in that the first
  argument is an optional attributes map. If the defined function is
  called without an attribute map, then `{}` will be passed
  implicitly."
  [name [attrs & args] & body]
  `(let [f# (fn [~attrs ~@args] ~@body)]
     (def ~(vary-meta name assoc :arglists '([& content] [attrs & [content]]))
       (reacl-c-basics.core/dom-like f#))))
