(ns reacl-c-basics.cljss-test
  (:require [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [clojure.string :as str]
            [reacl-c-basics.cljss :as cljss :include-macros true]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

;; Note: requires dom/a browser.

(deftest defstyled-test
  (cljss/defstyled my-header dom/h1
    {:color "blue"})

  ;; figure out which generates css-class name was used; (would be possible less hacky too)
  (let [cname (:className (:attrs (my-header)))]

    (is (not-empty cname))

    (is (= (dom/h1 {:className cname} (dom/div)) (my-header {} (dom/div))))
    (is (= (dom/h1 {:className cname
                    :onClick :foo})
           (my-header {:onClick :foo})))
    (is (= (dom/h1 {:className cname} (dom/div)) (my-header (dom/div))))

    (is (= (dom/h1 {:className (str "foo " cname)})
           (my-header {:className "foo"})))

    ;; cljss joins all classes into :className
    (is (= (dom/h1 {:className (str "foo " cname)})
           (my-header {:class "foo"})))
  
    (is (= (my-header {:onClick :foo} (dom/div)) (my-header {:onClick :foo} (dom/div))))
    ))

(deftest css-test
  (is (= "css-285293225" (cljss/css {:color "blue"})))

  (is (= "boot css-285293225" (cljss/css "boot" {:color "blue"})))
  
  (is (= (cljss/css {:color "blue"}) (cljss/css {:color "blue"})))

  (let [y "blue"]
    (is (= "css-454111966 vars--1646932754" (cljss/css {:color y}))))

  #_(let [foo {:color "black"}]
    (is (= "" (cljss/css foo))))

  (let [f (fn [y] (cljss/css {:color y}))]
    (is (not= (f "black") (f "white"))))

  ;; (dom/div {:class (cljss/css {:border "1px solid black"})})
  )
