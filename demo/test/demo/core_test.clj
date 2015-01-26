(ns demo.core-test
  (:use clojure.test 
        demo.core
        cooljure.core )
  (:require 
    [schema.test        :as s-tst]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest parse-edge-t
  (is (= [1 2] (parse-edge " 1 2 " )))
  (is (= [123 45] (parse-edge "123 45" )))
  (is (thrown? Exception (parse-edge " 1 2 3 " )))
  (is (thrown? Exception (parse-edge [1 2] )))
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