(defproject de.active-group/reacl-c-basics "0.7.0"
  :description "Library with utilities often useful when writing web applications with Reacl-C."
  
  :url "http://github.com/active-group/reacl-c-basics"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [de.active-group/active-clojure "0.33.0"]
                 [de.active-group/reacl-c "0.7.0"]
                 [de.active-group/reacl-basics "0.1.2"]
                 [reacl "2.2.2"]
                 [cljs-ajax "0.7.5"]]

  :plugins [[lein-codox "0.10.7"]
            [lein-auto "0.1.3"]]

  :profiles {:dev {:dependencies [[codox-theme-rdash "0.1.2"]
                                  [com.bhauman/figwheel-main "0.2.0"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   :source-paths ["src" "test" #_"examples"]
                   :resource-paths ["target" "resources"]}}

  :clean-targets ^{:protect false} [:target-path]

  ;; open http://localhost:9500/figwheel-extra-main/auto-testing for the tests.
  ;; open http://localhost:9500/figwheel-extra-main/todo and others for the examples
  :aliases {"fig" ["trampoline" "with-profile" "+dev,+test" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}

  :codox {:language :clojure ;; :clojurescript
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/reacl-c/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :auto {:default {:paths ["src" "test" #_"examples"]}}
  )
