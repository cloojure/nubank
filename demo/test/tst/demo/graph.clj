(ns tst.demo.graph
  (:use demo.graph)
  (:use clojure.test 
        cooljure.core )
  (:require 
    [schema.test    :as s-tst]
    [demo.array     :as array]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest parse-edge-t
  (is (= [1 2]              (parse-edge " 1 2 " )))
  (is (= [123 45]           (parse-edge "123 45" )))
  (is (thrown? Exception    (parse-edge " 1 2 3 " )))
  (is (thrown? Exception    (parse-edge [1 2] )))
)

(deftest add-edge-t
  (demo.graph/reset)
  (add-edge [1 2])
  (is (= { 1 #{2}
           2 #{1} }
         @graph))
  
  (demo.graph/reset)
  (doseq [edge  [[1 2] [2 3]]] 
    (add-edge edge))
  (is (= { 1 #{2}
           2 #{1 3}
           3 #{2} }
         @graph))

  (demo.graph/reset)
  (doseq [edge [[1 2] [2 3] [2 4] [3 4] [4 5]]]
    (add-edge edge))
  (is (= { 1 #{2}
           2 #{1 3 4}
           3 #{2 4}
           4 #{2 3 5}
           5 #{4} }
         @graph))
)

(deftest symmetry-t
  (demo.graph/reset)
  (doseq [edge [[1 2] [2 3] [2 4] [3 4] [4 5]]]
    (add-edge edge))
  (let [nodes (all-nodes) ]
    (is (= nodes #{1 2 3 4 5}))
    (doseq [node nodes]
      (do
        (is (contains? @graph node))
        (doseq [nbr (neighbors node)]
          (is (connected? node nbr))
          (is (connected? nbr node))
        )))))

(deftest shortest-graph-t
  (println "shortest-graph-t #1")
  (let [text  " 0 1
                1 2"
  ]
  (binding [*spy* true]
    (demo.graph/reset)
    (load-graph text)
    (spyx graph)
    (let [
      spath     (shortest-path)
      -- (println "spath")
      -- (array/disp spath)

      target    [ [0 1 2]
                  [1 0 1]
                  [2 1 0] ]
      cness    (closeness spath)          
      cness-t  [1/3 1/2 1/3]
    ]
      (is (array/symmetric? spath))
      (println "target")
      (array/disp target)

      (is (= spath target))
      (is (= cness cness-t))
    )))

  (println "shortest-graph-t #2")
  (let [text  " 0 1 
                1 2 
                2 0 
                0 3 
                3 4 
                4 5 
                5 3"
  ]
  (binding [*spy* true]
    (demo.graph/reset)
    (load-graph text)
    (let [
      spath     (shortest-path)
      target    [ [0 1 1 1 2 2]
                  [1 0 1 2 3 3]
                  [1 1 0 2 3 3]
                  [1 2 2 0 1 1]
                  [2 3 3 1 0 1]
                  [2 3 3 1 1 0] ]
      cness     (closeness spath)
      cness-t   [1/7 1/10 1/10 1/7 1/10 1/10]
    ]
      (is (array/symmetric? spath))
      (is (= spath target))
      (is (= cness cness-t))
    )))
)

(deftest calc-penalty-t
  (reset! demo.graph/fraud-nodes #{0})
  (let [node-dist       [ [0 1 2]
                          [1 0 1]
                          [2 1 0] ]
      closeness-maps    [ {:node 0 :closeness 1/3} 
                          {:node 1 :closeness 1/2}
                          {:node 2 :closeness 1/3} ]
      closeness-fraud   (fraud-adjust closeness-maps node-dist)

      closeness-goal    [ {:node 0 :closeness 0.0} 
                          {:node 1 :closeness 0.25}
                          {:node 2 :closeness 0.25} ]
  ]
    (is (= closeness-fraud closeness-goal))
  )

  (reset! demo.graph/fraud-nodes #{0})
  (let [node-dist       [ [0 1 1 1 2 2]
                          [1 0 1 2 3 3]
                          [1 1 0 2 3 3]
                          [1 2 2 0 1 1]
                          [2 3 3 1 0 1]
                          [2 3 3 1 1 0] ]
      closeness-maps    [ {:node 0 :closeness 1/7 }
                          {:node 1 :closeness 1/10}
                          {:node 2 :closeness 1/10}
                          {:node 3 :closeness 1/7 }
                          {:node 4 :closeness 1/10}
                          {:node 5 :closeness 1/10}
                          ]
      closeness-fraud   (fraud-adjust closeness-maps node-dist)

      closeness-goal    [ {:node 0 :closeness 0.0 } 
                          {:node 1 :closeness 1/20}
                          {:node 2 :closeness 1/20} 
                          {:node 3 :closeness 1/14} 
                          {:node 4 :closeness 0.075} 
                          {:node 5 :closeness 0.075} ]

    fraud-vals      (mapv #(double (:closeness %)) closeness-fraud)
    goal-vals       (mapv #(double (:closeness %)) closeness-goal)
    success         (mapv #(rel= %1 %2 :digits 5)  fraud-vals goal-vals)
  ]
    (is (every? truthy? success))
  )
)

