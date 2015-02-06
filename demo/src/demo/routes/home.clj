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
  [id-1 id-2]
  (println "received: (add-edge" id-1 id-2 ")" )
  (do 
    (graph/add-edge id-1 id-2)
    (layout/common 
      [:h1 "Added Edge" 
        [:p (format "id-1=%s" id-1)]
        [:p (format "id-2=%s" id-2)]
      ] )))

(defn show-graph []
  (if (< 0 (count (graph/all-nodes)))
    (let [out   (for [entry (graph/get-graph) ]
                  [:h3 (print-str entry) ] )
    ]
      (apply layout/common out)
    )
    ; else
    (layout/common
      [:h1 "Graph is empty"] )
  ))

(defn closeness []
  (apply layout/common 
    (for [entry (graph/sorted-closeness) ]
      [:h3 (print-str entry) ]
    )))

(defn add-fraud
  "Add a node to the fraudulent list, returning the list"
  [node]
  (let [node (coolp/parse-int node) ]
    (println "Added Fraud node:" node)
    (graph/add-fraud node)
    (layout/common 
      [:h1 (str "Added Fraud node:" node)]
      [:h2 (print-str "All fraud nodes:" (graph/get-fraud-nodes))]
    )))

(defn fraud-nodes 
  "Returns a list of all fraud nodes"
  []
  (layout/common 
    [:h1 "All fraud nodes:"]
    [:h2 (print-str (graph/get-fraud-nodes)) ]
  ))

(defn reset 
  "Reset server to empty graph"
  []
  (graph/reset)
  (layout/common 
    [:h1 "System reset performed"]
    [:h2 (print-str "Nodes:" (count (graph/all-nodes))) ]
    [:h2 (print-str "Fraud Nodes:" (count (graph/get-fraud-nodes))) ]
  ))

(defroutes home-routes
  (GET   "/"                    []          (home))
  (GET   "/add-edge/:n1/:n2"    [n1 n2]     (add-edge n1 n2))
  (GET   "/closeness"           []          (closeness))
  (GET   "/graph"               []          (show-graph))
  (GET   "/reset"               []          (reset))
  (GET   "/add-fraud/:node"     [node]      (add-fraud node))
  (GET   "/fraud-nodes"         []          (fraud-nodes))
)
