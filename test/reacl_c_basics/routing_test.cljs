(ns reacl-c-basics.routing-test
  (:require [reacl-c-basics.routing :as routing]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu :include-macros true]
            [reacl-c.test-util.xpath :as xpath :include-macros true]
            [reacl-basics.pages.routes :as routes :include-macros true]
            [reacl-basics.pages.history :as history]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest history-router-test
  (routes/clear-routes!)

  (routes/defroute home "/home")
  (routes/defroute person "/person/:id")

  (let [home-page (fn [] (c/dynamic #(dom/div "Homepage " %)))
        person-page (fn [id & [params]]
                      (dom/div "Person: " (str id) " " (str (:a params))))

        pages {home (routing/page home-page)
               person (routing/page person-page)}]
    
    (async done
           (let [hist-nav! (atom nil)
                 current-path (atom "/home")
                 test-history
                 (reify history/History
                   (push! [_ path]
                     (reset! current-path path))
                   (get-current [_]
                     @current-path)
                   (start! [_ nav-path! path-exists?]
                     (reset! hist-nav! nav-path!))
                   (stop! [_]
                     (reset! hist-nav! nil)))

                 main (routing/history-router test-history pages)

                 env (tu/env main)

                 doms-with-tag (fn [tag]
                                 (xpath/select-all (tu/get-component env)
                                                   (xpath/>> ** tag)))
                 dom-content (fn [c]
                               (apply str (array-seq (.-children c))))]

             (tu/mount! env "state")

             ;; "/home" initially, showing the homepage with the application state:
             (is (= ["Homepage state"]
                    (map dom-content (doms-with-tag "div"))))
             
             ;; emulate a user click on an anchor to go to person page:
             (js/window.setTimeout
                (fn []
                  (@hist-nav! "/person/123?a=42")
                  (is (= ["Person: 123 42"]
                         (map dom-content (doms-with-tag "div"))))
                  (done))
                0)))))
