(defproject active-group.zip "0.1.0-SNAPSHOT"
  :description "Functional hierarchical compositional zippers"
  :url "http://github.com/active-group/reacl/zippers"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2280" :scope "provided"]]

  :plugins [[com.keminglabs/cljx "0.4.0"]
            [lein-cljsbuild "1.0.3"]
            [org.bodil/lein-nashorn "0.1.2"]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}
                  
                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/clj"
                   :rules :clj}
                  
                  {:source-paths ["test/cljx"]
                   :output-path "target/generated/test/cljs"
                   :rules :cljs}]}

  :source-paths ["target/generated/src/clj"]

  :test-paths ["target/generated/test/clj"]

  :cljsbuild {:builds {:dev {:source-paths ["target/classes"]
                             :compiler {:output-to "target/main.js"
                                        :optimizations :whitespace
                                        :pretty-print true}}}}

  :hooks [cljx.hooks])
