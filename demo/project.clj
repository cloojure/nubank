(defproject demo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [
    [org.clojure/clojure        "1.6.0"]
    [cooljure                   "0.1.21"]
    [prismatic/schema           "0.3.3"]
    [prismatic/hiphip           "0.2.0"]
    [compojure                  "1.1.6"]
    [hiccup                     "1.0.5"]
    [ring-server                "0.3.1"]
  ]
  :plugins [[lein-ring "0.8.12"]]
  :ring {:handler   demo.handler/app
         :init      demo.handler/init
         :destroy   demo.handler/destroy}
  :profiles
  { :uberjar        { :aot :all }
    :production     { :ring {:open-browser? false, :stacktraces? false, :auto-reload? false} }
    :dev            { :dependencies [ [ring-mock "0.1.5"]
                                      [ring/ring-devel "1.3.1"] ]
                    }
  }
  :main ^:skip-aot demo.graph
  :target-path "target/%s"
  :jvm-opts ^:replace []
)
