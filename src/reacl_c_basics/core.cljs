(ns reacl-c-basics.core
  (:require [reacl-c.dom :as dom]))

(defn ^:no-doc split-dom-attrs [args]
  (if (and (not-empty args)
           (dom/dom-attributes? (first args)))
    [(first args) (rest args)]
    [{} args]))

(defn ^:no-doc dom-like [f]
  (fn [& args]
    (let [[attrs children] (split-dom-attrs args)]
      (apply f attrs children))))
