(ns reacl-c-basics.mvvm.model-view-test
  (:require [reacl-c-basics.mvvm.model :as model]
            [reacl-c-basics.mvvm.view :as view]
            [reacl-c.test-util.dom-testing :as dt :include-macros true]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [cljs-async.test :as at :include-macros true]
            [cljs.test :as t :include-macros true]))

;; test utils

(defn atom-obs-once [a]
  (model/observable (fn atom-obs-sub [subscriber]
                      (add-watch a :key
                                 (fn [k r o n]
                                   (.next subscriber n)
                                   (.complete subscriber)))
                      (fn []
                        (remove-watch a :key)))))

(defn simulate-ajax [id]
  ;; do something asynchronous
  (let [a (atom nil)]
    (js/window.setTimeout (fn []
                            (reset! a {:id id :text "Hello"}))
                          10)
    (atom-obs-once a)))

;; model namespace

(def empty-model {:current nil})

(def current-article :current)

(def current-article-text (comp :text :current))

(defn select-article! [model id]
  ;; Note: don't really have to do this check usually; 'init' does it
  ;; already; but if it is used in a button or so, then the behavious changes.
  (if (not= id (:article-id (:current model)))
    (model/watch (simulate-ajax id)
                 (fn [model a] (assoc model :current a)))
    model))

;; view namespace

(defn view-article-4711 []
  (c/fragment (view/init select-article! 4711)
              (dom/div {} (view/show current-article-text))))

;; test

(at/deftest ttt
  (dt/rendering
   (view/view empty-model (view-article-4711))
   (fn [env]
     (dt/find env (dt/by-text "Hello")))))
 
