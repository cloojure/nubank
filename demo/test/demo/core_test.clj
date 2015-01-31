(ns demo.core-test
  (:use demo.core)
  (:use clojure.test 
        cooljure.core )
  (:require 
    [schema.test :as s-tst]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest parse-edge-t
  (is (= [1 2]              (parse-edge " 1 2 " )))
  (is (= [123 45]           (parse-edge "123 45" )))
  (is (thrown? Exception    (parse-edge " 1 2 3 " )))
  (is (thrown? Exception    (parse-edge [1 2] )))
)

(deftest accum-edges-t
 (is (= { 1 #{2}
          2 #{1} }
        (accum-edges {} [1 2])))
 (is (= { 1 #{2}
          2 #{1 3}
          3 #{2} }
        (reduce accum-edges {} [[1 2] [2 3]] )))
 (is (= { 1 #{2}
          2 #{1 3 4}
          3 #{2 4}
          4 #{2 3 5}
          5 #{4} }
        (reduce accum-edges 
                {} 
                [[1 2] [2 3] [2 4] [3 4] [4 5]] )))
)

(deftest symmetry-t
  (let [
    graph    (reduce accum-edges 
                {} 
                [[1 2] [2 3] [2 4] [3 4] [4 5]] )
    nodes    (all-nodes graph)
  ]
    (is (= nodes #{1 2 3 4 5}))
    (doseq [node nodes]
      (do
        (is (contains? graph node))
        (doseq [nbr (neighbors graph node)]
          (is (connected? graph node nbr))
          (is (connected? graph nbr node))
        )))))

