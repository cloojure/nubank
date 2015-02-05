(ns tst.demo.darr
  (:use demo.darr)
  (:use clojure.test 
        cooljure.core ))

(deftest arrays-t
  (newline)
  (println "darr-test")
  (let [a34     (create 3 4) ]
    (newline)
    (println "a34 #1")
    (disp a34)
    (is (= 3    (alength  a34)          (num-rows a34)))
    (is (= 4    (alength (aget a34 0))  (num-cols a34)))
    (is (every?  #(=  0.0 %)  (forv [ii (range (num-rows a34))
                                     jj (range (num-cols a34)) ]
                                (get-elem a34 ii jj)))))

  (let [a34     (create 3 4 42.0) ]
    (newline)
    (println "a34 #2")
    (disp a34)
    (is (= 3    (alength  a34)          (num-rows a34)))
    (is (= 4    (alength (aget a34 0))  (num-cols a34)))
    (is (every?  #(= 42.0 %)  (forv [ii (range (num-rows a34))
                                     jj (range (num-cols a34)) ]
                                (get-elem a34 ii jj)))))

  (let [a34     (create 3 4)]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (set-elem a34 ii jj (double (+ (* ii 10) jj)))))
    (newline)
    (println "a34 #3")
    (disp a34)
    (let [target    [ [  0.0  1.0  2.0  3.0 ]
                      [ 10.0 11.0 12.0 13.0 ]
                      [ 20.0 21.0 22.0 23.0 ] ]
          tsts  (forv [ii (range 3)
                       jj (range 4) ]
                    (=  (get-elem a34 ii jj)
                        (get-in target [ii jj])))
    ]
      (is (every? truthy? tsts))))
)


