(ns demo.routes.home
  (:use cooljure.core)
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
    (if (and (<= 0 n1 500) (<= 0 n2 500))
      (do 
        (demo.graph/add-edge [n1 n2] )
        (layout/common 
          [:h1 "Added Edge" 
            [:p (format "n1=%d" n1)]
            [:p (format "n2=%d" n2)]
          ] ))
      (do 
        (layout/common 
          [:h1 "Error - Invalid Node Values" 
            [:p "Node values must be in the range [0 <= node < 500]"]
            [:p (format "n1=%d" n1)]
            [:p (format "n2=%d" n2)]
          ] ))
    )))

(defroutes home-routes
  (GET   "/" [] (home))
  (GET   "/add-edge/:n1/:n2" [n1 n2] (add-edge n1 n2))
)
