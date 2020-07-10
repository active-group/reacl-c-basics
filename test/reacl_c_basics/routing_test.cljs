(ns reacl-c-basics.routing-test
  (:require [reacl-c-basics.pages.core :as routing]
            [reacl-c.core :as c]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.core :as tu :include-macros true]
            [reacl-c-basics.pages.routes :as routes :include-macros true]
            [reacl-c-basics.pages.history :as history]
            [reacl-basics.pages.history :as rhistory]
            [cljs.test :refer (is deftest testing async) :include-macros true]))

(deftest history-router-test
  (routes/defroutes all-routes
    (routes/defroute home "/home")
    (routes/defroute person "/person/:id"))

  (is (= all-routes #{home person}))
  
  (let [home-page (fn [] (c/dynamic #(dom/div "Homepage " %)))
        person-page (fn [id & [params]]
                      (dom/div "Person: " (str id) " " (str (:a params))))

        pages {home home-page
               person person-page}]

    (is (= "/home" (routing/href home)))
    (is (= "/person/42" (routing/href person 42)))
    (is (= "/person/42?a=foo%20bar" (routing/href person 42 {:a "foo bar"})))
    
    (async done
           (let [hist-nav! (atom nil)
                 current-path (atom "/home")
                 test-history
                 (reify rhistory/History
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
                                 (tu/find-all env tag))
                 dom-content (fn [c]
                               (apply str (array-seq (.-children c))))]

             (tu/mount! env "state")

             ;; "/home" initially, showing the homepage with the application state:
             (is (= ["Homepage state"]
                    (map dom-content (doms-with-tag (dom/div)))))
             
             ;; emulate a user click on an anchor to go to person page:
             (js/window.setTimeout
              (fn []
                (@hist-nav! "/person/123?a=42")
                (is (= ["Person: 123 42"]
                       (map dom-content (doms-with-tag (dom/div)))))
                (done))
              0)))))
