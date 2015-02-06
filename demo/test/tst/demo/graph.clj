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
  (reset)
  (add-edge [1 2])
  (is (= { 1 #{2}
           2 #{1} }
         (get-graph)))
  
  (reset)
  (doseq [edge  [[1 2] [2 3]]] 
    (add-edge edge))
  (is (= { 1 #{2}
           2 #{1 3}
           3 #{2} }
         (get-graph)))

  (reset)
  (doseq [edge [[1 2] [2 3] [2 4] [3 4] [4 5]]]
    (add-edge edge))
  (is (= { 1 #{2}
           2 #{1 3 4}
           3 #{2 4}
           4 #{2 3 5}
           5 #{4} }
         (get-graph)))
)

(deftest symmetry-t
  (reset)
  (doseq [edge [[1 2] [2 3] [2 4] [3 4] [4 5]]]
    (add-edge edge))
  (let [nodes (all-nodes) ]
    (is (= nodes #{1 2 3 4 5}))
    (doseq [node nodes]
      (do
        (is (contains? (get-graph) node))
        (doseq [nbr (neighbors node)]
          (is (connected? node nbr))
          (is (connected? nbr node))
        )))))

(deftest add-fraud-t
  (reset)
  (is (= #{} (get-fraud-nodes)))

  (add-fraud 0)
  (add-fraud 99)
  (is (= #{0 99} (get-fraud-nodes)))
)

(deftest shortest-path-t
  (reset)
  (let [text  " 0 1
                1 2"
        -- (load-graph text)
        spath     (shortest-path)
        spath-t   [ [0 1 2]
                    [1 0 1]
                    [2 1 0] ]

        cness    (calc-closeness)          
        cness-t  [1/3 1/2 1/3]
  ]
    (is (array/symmetric? spath))
    (is (= spath spath-t))
    (is (= cness cness-t))
  )

  (reset)
  (let [text  " 0 1 
                1 2 
                2 0 
                0 3 
                3 4 
                4 5 
                5 3"
        -- (load-graph text)
        spath     (shortest-path)
        spath-t   [ [0 1 1 1 2 2]
                    [1 0 1 2 3 3]
                    [1 1 0 2 3 3]
                    [1 2 2 0 1 1]
                    [2 3 3 1 0 1]
                    [2 3 3 1 1 0] ]

        cness     (calc-closeness)
        cness-t   [1/7 1/10 1/10 1/7 1/10 1/10]
  ]
    (is (array/symmetric? spath))
    (is (= spath spath-t))
    (is (= cness cness-t))
  )
)

(deftest fraud-adjust-t
  (reset)
  (let [node-dist       [ [0 1 2]
                          [1 0 1]
                          [2 1 0] ]
      closeness-raw     [1/3 1/2 1/3]
      closeness-fraud   (fraud-adjust closeness-raw node-dist)
      closeness-goal    [1/3 1/2 1/3]
  ]
    (is (= closeness-fraud closeness-goal))
  )

  (reset)
  (add-fraud 0)
  (let [node-dist       [ [0 1 2]
                          [1 0 1]
                          [2 1 0] ]
      closeness-raw     [1/3 1/2 1/3]
      closeness-fraud   (fraud-adjust closeness-raw node-dist)
      closeness-goal    [0.0 0.25 0.25]
  ]
    (is (= closeness-fraud closeness-goal))
  )

  (reset)
  (let [node-dist       [ [0 1 1 1 2 2]
                          [1 0 1 2 3 3]
                          [1 1 0 2 3 3]
                          [1 2 2 0 1 1]
                          [2 3 3 1 0 1]
                          [2 3 3 1 1 0] ]
      closeness-raw     [1/7 1/10 1/10 1/7 1/10 1/10 ]
      closeness-fraud   (fraud-adjust closeness-raw node-dist)
      closeness-goal    [1/7 1/10 1/10 1/7 1/10 1/10 ]
      success           (mapv #(rel= %1 %2 :digits 5)  closeness-fraud closeness-goal)
  ]
    (is (every? truthy? success))
  )

  (reset)
  (add-fraud 0)
  (let [node-dist       [ [0 1 1 1 2 2]
                          [1 0 1 2 3 3]
                          [1 1 0 2 3 3]
                          [1 2 2 0 1 1]
                          [2 3 3 1 0 1]
                          [2 3 3 1 1 0] ]
      closeness-raw     [ 1/7  1/10 1/10 1/7  1/10  1/10 ]
      closeness-fraud   (fraud-adjust closeness-raw node-dist)
      closeness-goal    [ 0.0  1/20 1/20 1/14 0.075 0.075 ]
      success           (mapv #(rel= %1 %2 :digits 5)  closeness-fraud closeness-goal)
  ]
    (is (every? truthy? success))
  )
)

(deftest calc-closeness-t
  (reset)
  (let [text  " 0 1
                1 2"
        -- (load-graph text)
        cness           (calc-closeness)          
        cness-goal      [1/3 1/2 1/3]
  ]
    (is (= cness cness-goal)))

  (reset)
  (add-fraud 0)
  (let [text  " 0 1
                1 2"
        -- (load-graph text)
        cness           (calc-closeness)          
        cness-goal      [0.0 0.25 0.25]
  ]
    (is (= cness cness-goal)))

  (reset)
  (let [text  " 0 1 
                1 2 
                2 0 
                0 3 
                3 4 
                4 5 
                5 3"
        -- (load-graph text)
        cness           (calc-closeness)
        cness-goal      [1/7 1/10 1/10 1/7 1/10 1/10 ]
        success         (mapv #(rel= %1 %2 :digits 5)  cness cness-goal)
  ]
    (is (every? truthy? success)))

  (reset)
  (add-fraud 0)
  (let [text  " 0 1 
                1 2 
                2 0 
                0 3 
                3 4 
                4 5 
                5 3"
        -- (load-graph text)
        cness           (calc-closeness)
        cness-goal      [ 0.0  1/20 1/20 1/14 0.075 0.075 ]
        success         (mapv #(rel= %1 %2 :digits 5)  cness cness-goal)
      ]
    (is (every? truthy? success)))
)

