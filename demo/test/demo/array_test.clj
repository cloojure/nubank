(ns demo.array-test
  (:use clojure.test 
        cooljure.core )
  (:require 
    [demo.array     :as array]
    [schema.test    :as s-tst]
  ))

(use-fixtures :once s-tst/validate-schemas)

(deftest arrays-t
  (let [a34     (array/create 3 4 :a) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (array/num-rows a34)))
    (is (= 4    (count (a34 0))  (array/num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= :a %) a34f))
    (is (every?  #(= :a %)  (for [ii (range (array/num-rows a34))
                                  jj (range (array/num-cols a34)) ]
                              (array/get-elem a34 ii jj)))))

  (let [a34     (array/create 3 4) 
        a34f    (flatten a34) ]
    (is (= 3    (count  a34)     (array/num-rows a34)))
    (is (= 4    (count (a34 0))  (array/num-cols a34)))
    (is (= 12   (count  a34f)))
    (is (every?  #(= 0 %) a34f))
    (is (every?  #(= 0 %)   (for [ii (range (array/num-rows a34))
                                  jj (range (array/num-cols a34)) ]
                              (array/get-elem a34 ii jj)))))

  (let [a34     (atom (array/create 3 4)) ]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 array/set-elem ii jj (str ii jj))))
;   (println \newline "a34:") (array/disp @a34) 
    (dotimes [ii 3]
      (dotimes [jj 4]
        (let [target    (str ii jj)
              actual    (array/get-elem @a34 ii jj) ]
        (when-not (= target actual)
          (is false (str "set/get mismatch: " target " " actual)))))))
)
