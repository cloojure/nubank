(ns tst.demo.graph
  (:use demo.graph)
  (:use clojure.test 
        cooljure.core )
  (:require 
    [schema.test    :as s-tst]
    [demo.array     :as array]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest node-idx-t
  (reset)
  (is (= 0 (node-idx  0 )))
  (is (= 1 (node-idx  1 )))
  (is (= 2 (node-idx "2")))
  (is (= 3 (node-idx "3")))
  (is (= 4 (node-idx :x )))
  (is (= 5 (node-idx :y )))

  (is (= 0 (node-idx  0 )))
  (is (= 1 (node-idx  1 )))
  (is (= 2 (node-idx "2")))
  (is (= 3 (node-idx "3")))
  (is (= 4 (node-idx :x )))
  (is (= 5 (node-idx :y )))
)

(deftest add-edge-t
  (reset)
  (load-graph "0 1")
  (is (= { 0 #{1}
           1 #{0} }
         (get-graph)))
  
  (reset)
  (load-graph "a b")
  (is (= { 0 #{1}
           1 #{0} }
         (get-graph)))
  
  (reset)
  (load-graph   "a b
                 b c")
  (is (= { 0 #{1}
           1 #{0 2}
           2 #{1} }
         (get-graph)))

  (reset)
  (load-graph   "0 1
                 1 2
                 1 3
                 2 3
                 3 4" )
  (is (= { 0 #{1}
           1 #{0 2 3}
           2 #{1 3}
           3 #{1 2 4}
           4 #{3} }
         (get-graph)))
  (let [nodes (all-nodes) ]
    (is (= nodes #{0 1 2 3 4}))
    (doseq [node nodes]
      (is (contains? (get-graph) node))
      (doseq [nbr (neighbors node)]
        (is (connected? node nbr))
        (is (connected? nbr node))
      )))
)

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

