(ns reacl-c-basics.pages.routes
  "Functions to define and use the routes in a client side routing application.

  A typical usage would be to define the routes of an application in a
  shared `cljc` file, and use them with
  a [[core/html5-history-router]] in the client, and
  with [[ring/wrap-client-routes]] on the server side to deliver the
  client code for all of these routes.

  You can also call routes as a function, passing the defined path and
  query params, and get a 'href' back that be use in anchor tags to
  let the user navigate to the pages of the application."
  (:require [clout.core :as clout]
            [clojure.string :as str]
            [cemerick.url :as url]))

#?(:cljs
   (defprotocol ^:private IRoutable
     (-unparse-uri [this params] "Return uri for this routable, using params as path or query params.")
     (-parse-uri [this uri] "Return vector of positional and a map of query params if this uri matches"))

   :clj
   (defprotocol ^:private IRoutable
     (^:private -route-matches [this request])))

#?(:cljs
   (defn href
     "Returns a URI path for the given route and concrete values for all
  parameters. The last parameter may be a map of query params.

  This is the same as calling the route as a function."
     [route & params]
     (-unparse-uri route params)))

#?(:cljs
   (defn parse
     "If the given URI matches the given route, returns a vector of all path arguments and optionally a map of query params."
     [route uri]
     (-parse-uri route uri)))

#?(:clj
   (defn route-matches [route request]
     (-route-matches route request)))

(defn routable?
  "If `v` is a value returned by [[route]] of bound to a name with [[defroute]]."
  [v]
  (satisfies? IRoutable v))

(defn- parse-request [c-pattern request]
  (let [b (clout/route-matches c-pattern request)]
    (when b
      (vec (concat (map b (:keys c-pattern))
                   (some-> (not-empty (into {} (map (fn [[k v]] [(keyword k) v]) (:query-params request))))
                           (vector)))))))

(def ^:private compile-cached
  ;; should be ok when only used for the finite amount of 'defroutes' in code.
  ;; but really worth it; it's a heavy computation.
  (memoize clout/route-compile))

(defn- unparse-uri [pattern & args]
  (let [{:keys [source re keys absolute?]} (compile-cached pattern)
        _ (assert (or (= (count args) (count keys))
                      (= (count args) (inc (count keys)))))
        positional (map vector
                        keys
                        (take (count keys) args))
        params (if (= (count args) (count keys))
                 nil
                 (last args))]
    (let [s (reduce (fn [s [k v]]
                      (str/replace s (str k) (url/url-encode (str v))))
                    source
                    positional)]
      (if (empty? params)
        s
        (str s "?" (str/join "&" (map (fn [[k v]]
                                        (str (url/url-encode (name k)) "=" (url/url-encode (str v))))
                                      params)))))))

(defrecord ^{:private true :no-doc true} Route [pattern]
  #?@(:cljs
      [IFn
       (-invoke [this] (href this))
       (-invoke [this a] (href this a))
       (-invoke [this a b] (href this a b))
       (-invoke [this a b c] (href this a b c))
       (-invoke [this a b c d] (href this a b c d))
       (-invoke [this a b c d e] (href this a b c d e))
       (-invoke [this a b c d e f] (href this a b c d e f))
       (-invoke [this a b c d e f g] (href this a b c d e f g))
       (-invoke [this a b c d e f g h] (href this a b c d e f g h))
       (-invoke [this a b c d e f g h i] (href this a b c d e f g h i))
       (-invoke [this a b c d e f g h i j] (href this a b c d e f g h i j))
       (-invoke [this a b c d e f g h i j k] (href this a b c d e f g h i j k))
       (-invoke [this a b c d e f g h i j k l] (href this a b c d e f g h i j k l))
       (-invoke [this a b c d e f g h i j k l m] (href this a b c d e f g h i j k l m))
       (-invoke [this a b c d e f g h i j k l m n] (href this a b c d e f g h i j k l m n))
       (-invoke [this a b c d e f g h i j k l m n o] (href this a b c d e f g h i j k l m n o))
       (-invoke [this a b c d e f g h i j k l m n o p] (href this a b c d e f g h i j k l m n o p))
       (-invoke [this a b c d e f g h i j k l m n o p q] (href this a b c d e f g h i j k l m n o p q))
       (-invoke [this a b c d e f g h i j k l m n o p q r] (href this a b c d e f g h i j k l m n o p q r))
       (-invoke [this a b c d e f g h i j k l m n o p q r s] (href this a b c d e f g h i j k l m n o p q r s))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t] (href this a b c d e f g h i j k l m n o p q r s t))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest] (apply href this a b c d e f g h i j k l m n o p q r s t rest))
       IRoutable
       (-unparse-uri [_ args]
                     (apply unparse-uri pattern args))
       (-parse-uri [this uri]
                   (let [furl (url/url uri)]
                     ;; in particular, we must remove the query params to use clout:
                     (parse-request (compile-cached pattern)
                                    {:uri (:path furl)
                                     :query-params (:query furl)})))]
      :clj
      [IRoutable 
       (-route-matches [_ request] (parse-request (clout/route-compile pattern) request))]))

(defn route "Returns a route for the given URL pattern. Note: Prefer [[defroute]] below." [pattern]
  ;; Note: equal patterns don't make equal compiled routes :( Storing only the original pattern as a remedy.
  (assert (clout/route-compile pattern) (str "Invalid route pattern: " pattern))
  (Route. pattern))

(def ^:no-doc current-routes (atom nil)) ;; used in macro generated code.

(defmacro defroutes ;; TODO: add variant with a prefix, or a (context ...) macro?.
  [name & defs]
  `(do
     (reset! current-routes #{})
     ~@defs
     (def ~name @current-routes)
     (reset! current-routes nil)))

(defmacro defroute
  "Defines a var with the given `name` to be a route according the given `pattern`.
   Adds it to the a list of routes, if written inside a [[defroutes]] macro.

A pattern can be a fixed path `\"/path/to/page\"`, or can have one or more path arguments `\"/article/:id\"`.

For example:

```clojure
(defroute r0 \"/home\")

(href r0) => \"/home\"
(parse r0 \"/home\") => []
(r0 {:lang \"de\"}) => \"/home?lang=de\"
(parse r0 \"/home?lang=de\") => [{:lang \"de\"}]

(defroute r1 \"/path/:id\")

(r1 \"123\") => \"/path/123\"
(parse r1 \"/path/123\") => [\"123\"]
(parse r1 \"/other/path\") => nil
```
"
  [name pattern]
  (let [positional (:keys (clout/route-compile pattern))
        pargs (mapv (fn [_] (gensym "arg")) positional)
        oargs (mapv (comp symbol clojure.core/name) positional)]
    `(def ~(vary-meta name assoc
                          :doc (str "Route with the pattern " (pr-str pattern) ".")
                          ;;:arglists (list oargs (conj oargs `'query-params)) --- Note: has some bug for clj.
                          )
       (let [r# (route ~pattern)]
         (when @current-routes (swap! current-routes conj r#))
         r#))))
