(ns demo.routes.home
  (:require [compojure.core     :refer :all]
            [demo.views.layout  :as layout]
            [cooljure.parse     :as coolp]
  ))

(defn home []
  (layout/common [:h1 "Hello World!"]))

(defn add-edge 
  [n1 n2]
  (let [n1  (coolp/parse-int n1)
        n2  (coolp/parse-int n2) ]
    (println "received: (add-edge" n1 n2 ")" )
    (layout/common 
      [:h1 "Add Edge" 
      [:p (format "n1: %3d" n1)]
      [:p (format "n2: %3d" n2)]
      ] )))

(defroutes home-routes
  (GET   "/" [] (home))
  (GET   "/add-edge/:n1/:n2" [n1 n2] (add-edge n1 n2))
)
