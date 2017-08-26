(defproject dankreek/rektify "0.0.4-SNAPSHOT"
  :description "ClojureScript library to manipulate any JS object model in a declarative way."
  :url "https://github.com/dankreek/rektify"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["releases" {:url "https://clojars.org/repo"
                              :sign-releases false
                              :username :env/CLOJARS_USERNAME
                              :password :env/CLOJARS_PASSWORD}]]

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.660"]]

  :plugins [[lein-figwheel "0.5.13"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-codox "0.10.3"]]

  :jar-exclusions [#"public/.*"]

  ;:source-paths ["src"]

  :test-paths ["test/cljs" "test/js"]

  :codox {:language :clojurescript
          :source-paths ["src"]}

  :cljsbuild {:builds
              [{:id "test"
                :source-paths ["src" "test/cljs"]
                :compiler {:main runners.doo
                           :asset-path "/js/out"
                           :output-to "target/test.js"
                           :output-dir "target/cljstest/public/js/out"
                           :optimizations :whitespace
                           :libs ["test/js/test.classes.js"]}}

               {:id "dev"

                :figwheel {:open-urls ["http://localhost:3449/index.html"]}

                :compiler {:main rektify.core
                           :source-paths ["src"]
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/rektify.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true
                           :preloads [devtools.preload]}}
               ;; lein cljsbuild once min
               {:id "min"
                :compiler {:output-to "resources/public/js/compiled/rektify.js"
                           :source-paths ["src"]
                           :main rektify.core
                           :optimizations :advanced
                           :pretty-print false}}]}

  :figwheel {:css-dirs ["resources/public/css"]}

  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.4"]
                                  [figwheel-sidecar "0.5.13" :exclusions [org.clojure/tools.nrepl]]
                                  [com.cemerick/piggieback "0.2.2"]]
                   :plugins [[lein-doo "0.1.6"]]
                   :source-paths ["src" "dev"]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :clean-targets ^{:protect false} ["pom.xml"
                                                     "resources/public/js/compiled"
                                                     :target-path]}}

  :aliases {"test" ["doo" "phantom" "test" "once"]
            "test-phantom" ["doo" "phantom" "test"]
            "dev-test" ["test-phantom"]}
  )
