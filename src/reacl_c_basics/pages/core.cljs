(ns reacl-c-basics.pages.core
  (:require [reacl-c.core :as c :include-macros true]
            [active.clojure.functions :as f]
            [reacl-c-basics.pages.routes :as routes]
            [reacl-c-basics.pages.router :as router]
            [reacl-c-basics.pages.history :as history]))

;; Note: use together with reacl-c-basics.pages.routes and reacl-c-basics.pages.ring

(c/defn-item ^:no-doc history-router
  "For testing with a mocked history implementation."
  [history-impl pages]
  (router/history-router history-impl pages))

(c/defn-item html5-history-router
  "An item that renders to one of the pages from the given map of
  routes to pages, depending on the current browser location, which is
  monitored for changes. It also handles [[goto]] actions emitted from
  within, modifying the browser history accordingly. A page must be a
  function taking the arguments of the route, returning an item."
  [pages]
  (history-router (history/html5-history) pages))

(defn goto
  "Returns an action that causes the browser to go to the given
  path. Must be emitted from an item below a history-router item."
  [path]
  (router/goto path))

(defn href
  "Return an url string for the given route, with arguments for the
  positional and maybe a map for query params as required by the given
  route. Note that this is the same as calling the route as a function."
  [route & params]
  (apply routes/href route params))
