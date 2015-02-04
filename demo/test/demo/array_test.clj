(ns demo.array-test
  (:use demo.array)
  (:use clojure.test 
        cooljure.core )
  (:require 
    [demo.array     :as array]
    [schema.test    :as s-tst]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest arrays-t
  (let [a34     (create 3 4 :a) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (num-rows a34)))
    (is (= 4    (count (a34 0))  (num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= :a %) a34f))
    (is (every?  #(= :a %)  (forv [ii (range (num-rows a34))
                                   jj (range (num-cols a34)) ]
                              (get-elem a34 ii jj)))))

  (let [a34     (create 3 4) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (num-rows a34)))
    (is (= 4    (count (a34 0))  (num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= 0 %) a34f))
    (is (every?  #(= 0 %)   (forv [ii (range (num-rows a34))
                                   jj (range (num-cols a34)) ]
                              (get-elem a34 ii jj)))))

  (let [a34     (atom (create 3 4)) ]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 set-elem ii jj (str ii jj))))
;   (println \newline "a34:") (disp @a34) 
    (dotimes [ii 3]
      (dotimes [jj 4]
        (let [target    (str ii jj)
              actual    (get-elem @a34 ii jj) ]
        (when-not (= target actual)
          (is false (str "set/get mismatch: " target " " actual)))))))
)

(deftest symmetric-t
  (is (symmetric?   [[1 2]
                     [2 1]]))
  (is (symmetric?   [[1 2 3]
                     [2 4 5]
                     [3 5 6]]))
)

