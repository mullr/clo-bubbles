(defproject clo-bubbles "0.1.0-alpha1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/cljs"]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.58"]
                 [cljsjs/react "0.13.3-1"]
                 [reagent "0.5.0"]
                 [funcool/cats "0.6.1"]
                 [funcool/promesa "0.4.0"]
                 [org.clojure/tools.reader "0.10.0-alpha3"]]

  :plugins [[lein-cljsbuild "1.0.6"]]

  :min-lein-version "2.5.1"

  :cljsbuild {:builds {:app {:source-paths ["src/cljs"]
                             :compiler {:output-to     "app/js/p/app.js"
                                        :output-dir    "app/js/p/out"
                                        :asset-path    "js/p/out"
                                        :optimizations :none
                                        :pretty-print  true
                                        :cache-analysis true}}}}

  :clean-targets ^{:protect false} [:target-path "out" "app/js/p"]

  :figwheel {:css-dirs ["app/css"]}

  :profiles {:dev {:cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {:source-map true
                                                         :main       "clo-bubbles.dev"
                                                         :verbose true}
                                              :figwheel {:on-jsload "clo-bubbles.core/mount-root"}}}}

                   :plugins [[lein-ancient "0.6.7"]
                             [lein-kibit "0.1.2"]
                             [lein-cljfmt "0.3.0"]
                             [lein-figwheel "0.3.7"]]}
             :production {:cljsbuild {:builds {:app {:compiler {:optimizations :advanced
                                                                :main          "clo-bubbles.prod"
                                                                :cache-analysis false
                                                                :closure-defines {:goog.DEBUG false}
                                                                :externs ["node_modules/closurecompiler-externs/path.js"
                                                                          "node_modules/closurecompiler-externs/fs.js"
                                                                          "externs/misc.js"]
                                                                :pretty-print false}
                                                     :source-paths ["env/prod/cljs"]}}}}}
  )
