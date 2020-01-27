(ns reacl-c-basics.routing
  (:require [reacl-c.core :as c]
            [reacl-c.browser :as browser]
            [reacl2.core :as rcore :include-macros true]
            [reacl-basics.pages.core :as p]))

;; Note: use together with reacl-basics.pages.routes and reacl-basics.pages.ring

(rcore/defclass ^:private reacl-page* this state [f & args]
  ;; TODO: redirect messages?
  
  render
  (browser/render (rcore/bind this)
                  (apply f args)))

(defn- reacl-page [f opt state & args]
  ;; Note: reacl-basics still uses the old style, and maybe core/defc is not fully compatible as it seems
  ;; TODO: remove this hack when reacl-basics is on reacl-2.2 style.
  ;; FIXME: no clue why args are doubly nested here... getting a list of a list or a vector???
  (apply reacl-page* (assoc-in opt [:map :app-state] state)
         f (first (first args))))

(defn page
  "Creates a page definition, via a function that is called with all
  page params of the route (if any) and an optional map of query
  params, and which must return an element representing that page."
  [f]
  (p/page (c/partial reacl-page f)))

(defn ^:no-doc history-router
  "For testing with a mocked history implementation."
  [history-impl pages]
  (browser/lift reacl-basics.pages.router/history-router history-impl pages))

(defn html5-history-router
  "An element that renders to one of the pages from the given map of
  routes to pages, depending on the current browser location, which is
  monitored for changes. It also handles [[goto]] actions emitted from
  within, modifying the browser history accordingly."
  [pages]
  (browser/lift p/html5-history-router pages))

(defn goto
  "Returns an action to go to the given path."
  [path]
  (p/goto path))
