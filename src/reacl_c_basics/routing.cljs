(ns reacl-c-basics.routing
  (:require [reacl-c.core :as c]
            [reacl-c.browser :as browser]
            [reacl2.core :as rcore :include-macros true]
            [active.clojure.functions :as f]
            [reacl-basics.pages.routes :as routes]
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

(defn- page
  "Creates a page definition, via a function that is called with all
  page params of the route (if any) and an optional map of query
  params, and which must return an item representing that page."
  [f]
  (p/page (f/partial reacl-page f)))

(defn- lift-pages-map [pages]
  ;; this adds the 'reacl' wrapper, and allows to put ordinary functions into the map.
  (into {} (map (fn [[r p]]
                  [r (page p)])
                pages)))

(defn ^:no-doc history-router
  "For testing with a mocked history implementation."
  [history-impl pages]
  (browser/lift reacl-basics.pages.router/history-router history-impl (lift-pages-map pages)))

(defn html5-history-router
  "An item that renders to one of the pages from the given map of
  routes to pages, depending on the current browser location, which is
  monitored for changes. It also handles [[goto]] actions emitted from
  within, modifying the browser history accordingly."
  [pages]
  (browser/lift p/html5-history-router (lift-pages-map pages)))

(defn goto
  "Returns an action to go to the given path."
  [path]
  (p/goto path))

(defn href
  "Return an url string for the given route, with arguments for the
  positional and maybe a map for query params as required by the given
  route."
  [route & params]
  (apply routes/href route params))
