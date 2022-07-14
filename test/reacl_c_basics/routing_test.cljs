(ns reacl-c-basics.routing-test
  (:require [reacl-c-basics.pages.core :as routing]
            [reacl-c.core :as c :include-macros true]
            [reacl-c.dom :as dom]
            [reacl-c.test-util.dom-testing :as dt]
            [reacl-c-basics.pages.routes :as routes :include-macros true]
            [reacl-c-basics.pages.history :as history]
            [reacl-c-basics.pages.r-history :as rhistory]
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
                     (reset! hist-nav! nil)))]

             (dt/rendering
              (routing/history-router test-history pages)
              :state "state"
              (fn [env]

                ;; "/home" initially, showing the homepage with the application state:
                (is (some? (dt/get env (dt/by-text "Homepage state"))))

                ;; emulate a user click on an anchor to go to person page:
                (@hist-nav! "/person/123?a=42")
                (-> (dt/find env (dt/by-text "Person: 123 42"))
                    (.then (fn [_]
                             (done))
                           (fn [e]
                             (is (nil? e))
                             (done))))))))))
