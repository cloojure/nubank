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
