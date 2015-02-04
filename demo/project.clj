(defproject demo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
    [cooljure                   "0.1.21"]
    [prismatic/schema           "0.3.3"]
    [prismatic/hiphip           "0.2.0"]
    [org.clojure/clojure        "1.6.0"] ]
  :main ^:skip-aot demo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ^:replace []
)
