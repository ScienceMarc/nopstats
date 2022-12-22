(defproject nopstats "1.2.1"
  :description "NoP stat gatherer"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://spdx.org/licenses/MIT.html"}
  :dependencies [
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/data.json "2.4.0"]
                 [clj-http "3.12.3"]
                 ]
  :main ^:skip-aot nopstats.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
