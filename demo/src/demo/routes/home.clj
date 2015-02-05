(ns demo.routes.home
  (:use cooljure.core)
  (:require [compojure.core     :refer :all]
            [demo.views.layout  :as layout]
            [demo.graph         :as graph]
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
        (graph/add-edge [n1 n2] )
        (layout/common 
          [:h1 "Added Edge" 
            [:p (format "n1=%d" n1)]
            [:p (format "n2=%d" n2)]
          ] ))
      (do 
        ; #todo add error response code
        (layout/common 
          [:h1 "Error - Invalid Node Values" 
            [:p "Node values must be in the range [0 <= node <= 500]"]
            [:p (format "n1=%d" n1)]
            [:p (format "n2=%d" n2)]
          ] ))
    )))

(defn show-graph []
  (if (< 0 (count @graph/graph))
    (let [out     (for [entry @graph/graph]
                    [:p (println-str entry) ] )
    ]
      (apply layout/common out)
    )
    ; else
    (layout/common
      [:h1 "Graph is empty"] )
  ))

(defroutes home-routes
  (GET   "/" [] (home))
  (GET   "/add-edge/:n1/:n2" [n1 n2] (add-edge n1 n2))
  (GET   "/calc-closeness" [] (graph/calc-closeness))
  (GET   "/graph" [] (show-graph))
  (GET   "/reset" [] (graph/reset))
)
