(defproject de.active-group/reacl-c-basics "0.12.0-SNAPSHOT"
  :description "Library with utilities often useful when writing web applications with Reacl-C."
  
  :url "http://github.com/active-group/reacl-c-basics"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [de.active-group/reacl-c "0.11.7"]
                 [de.active-group/active-clojure "0.41.0"]
                 [venantius/accountant "0.2.5"]
                 [clout "2.2.1"]
                 [com.cemerick/url "0.1.1"]
                 [cljs-ajax "0.8.4"]
                 [clj-commons/cljss "1.6.4"]]

  :plugins [[lein-codox "0.10.7"]
            [lein-auto "0.1.3"]]

  :profiles {:dev {:dependencies [[de.active-group/cljs-async "2.0.0"]
                                  [de.active-group/reacl-c-testing "0.1.0"]]}
             :shadow [:dev {:dependencies [[de.active-group/reacl-c "0.12.1"]
                                           [thheller/shadow-cljs "2.11.7"]
                                           [binaryage/devtools "1.0.2"]]
                            :source-paths ["src" "test" "examples"]
                            :resource-paths ["target" "resources"]}]
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]}}

  :clean-targets ^{:protect false} [:target-path]

  :aliases {"dev" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "watch" "test"]
            "build-test" ["with-profile" "shadow" "run" "-m" "shadow.cljs.devtools.cli" "compile" "ci"]}

  :codox {:language :clojurescript
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/reacl-c-basics/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :auto {:default {:paths ["src" "test"]}}
  )
