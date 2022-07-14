(ns reacl-c-basics.pages.core
  (:require [reacl-c.core :as c :include-macros true]
            [active.clojure.functions :as f]
            [reacl-c-basics.pages.routes :as routes]
            [reacl-c-basics.pages.router :as router]
            [reacl-c-basics.pages.history :as history]))

;; Note: use together with reacl-c-basics.pages.routes and reacl-c-basics.pages.ring

(c/defn-item history-router
  "Like [[html5-history-router]], but with a custom History implementation."
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

(let [do-goto (fn [_ uri]
                (set! (.-href (.-location js/window)) uri)
                (c/return))]
  (c/defn-item router
    "An item that renders to one of the pages from the given map of
  routes to pages, depending on the pathname and query of the current
  window location URI. Unlike [[html5-history-router]] this does not
  capture/monitor location changes, but it handles [[goto]] actions."
    [pages]
    (-> (router/dispatch pages (let [l (.-location js/window)]
                                 (str (.-pathname l) (.-query l))))
        (router/handle-goto do-goto))))

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

(c/defn-subscription unload-confirmation "While this subscription is
active, the browser shall warn the user about losing data if they
leave the page, offering to stay on the page instead. If `disabled` is
true, this does nothing." deliver! [& [disabled]]
  ;; Note: when using html5-history, the 'back button' may not be
  ;; considered 'leaving the page' by the browser (because it thinks
  ;; it's just an anchor on the same page)
  (if-not disabled
    (let [f (fn [ev]
              (set! (.-returnValue (or ev (.-event js/window)))
                    true)
              (.preventDefault ev))]
      (js/window.addEventListener "beforeunload" f)
      (fn []
        (js/window.removeEventListener "beforeunload" f)))
    
    (fn [] nil)))

