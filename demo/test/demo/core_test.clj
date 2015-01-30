(ns demo.core-test
  (:use clojure.test 
        demo.core
        cooljure.core )
  (:require 
    [schema.test        :as s-tst]
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

(deftest arrays-t
  (let [a34     (newArray 3 4 :a) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (num-rows a34)))
    (is (= 4    (count (a34 0))  (num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= :a %) a34f))
    (is (every?  #(= :a %)  (for [ii (range (num-rows a34))
                                  jj (range (num-cols a34)) ]
                              (array-get a34 ii jj)))))

  (let [a34     (newArray 3 4) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (num-rows a34)))
    (is (= 4    (count (a34 0))  (num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= 0 %) a34f))
    (is (every?  #(= 0 %)   (for [ii (range (num-rows a34))
                                  jj (range (num-cols a34)) ]
                              (array-get a34 ii jj)))))

  (let [a34     (atom (newArray 3 4)) ]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 array-set ii jj (str ii jj))))
    (newline)
    (println "a34:")
    (disp-array @a34) 
    (is (=  (flatten @a34)
            (for [ii (range 3)
                  jj (range 4) ]
              (str ii jj)))))
)
