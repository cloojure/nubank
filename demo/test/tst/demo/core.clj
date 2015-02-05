(ns tst.demo.core
  (:use demo.core)
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
  (demo.core/reset)
  (add-edge [1 2])
  (is (= { 1 #{2}
           2 #{1} }
         @graph))
  
  (demo.core/reset)
  (doseq [edge  [[1 2] [2 3]]] 
    (add-edge edge))
  (is (= { 1 #{2}
           2 #{1 3}
           3 #{2} }
         @graph))

  (demo.core/reset)
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
  (demo.core/reset)
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
    (demo.core/reset)
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
    (demo.core/reset)
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

