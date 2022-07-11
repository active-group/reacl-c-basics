(ns reacl-c-basics.cljss
  (:require #?(:clj [cljss.core :as css])
            #?(:clj [cljss.builder :as builder])
            #?(:cljs [cljss.core :as css :include-macros true])
            [reacl-c.dom :as dom]))

#?(:clj (defmacro defstyled "Defines a css-styled variant of the given dom function, i.e. a function taking an optional attributes map as the first arguments. The [[reacl-c.dom]] functions suffice."
          [name dom-f styles]
          (let [cls-name#       (css/var->cls-name name)
                cmp-name#       (css/var->cmp-name name)
                [tag# static# vals# attrs#] (css/->styled dom-f styles cls-name#)
                create-element# `#(apply ~dom-f %1 %2)]
            `(do
               (def ~name
                 (reacl-c-basics.cljss/styled ~cls-name# ~static# ~vals# ~attrs# ~create-element#))
               ;; React specific - could use named items?
               #_(set! ~name ~'-displayName ~cmp-name#)))))

#?(:cljs
   (defn ^:no-doc styled [cls static vars attrs create-element]
     (let [internal (css/-styled cls static vars attrs create-element)]
       (fn [& args]
         (if (dom/dom-attributes? (first args))
           (apply internal args)
           (apply internal {} args))))))

#?(:clj
   (defmacro defstyles "Defines a function returning string with one or more class names for
  the given css style. Same as [[cljss.core/defstyles]]." [name args styles]
     `(css/defstyles ~name ~args ~styles)))

;; defn-dom + style?

#?(:clj (defmacro ^:no-doc css-1 [styles]
          (let [cls (str "css-" (hash styles))]
            `(css/css ~@(builder/build-styles cls styles)))))

(:clj (defmacro css "Returns a string of one or more css class names for the given class
  names or css styles." [& styles]
        ;; TODO: shouldn't the whole string should be constructible at compile time?
        `(apply str (interpose " " (list ~@(map (fn [st]
                                                  (if (string? st)
                                                    st
                                                    `(css-1 ~st)))
                                                styles))))))
