(ns demo.array
  (:require
    [clojure.string     :as str]
    [cooljure.parse     :as coolp]
    [schema.core        :as s]
    [schema.test        :as s-tst] )
  (:use 
    [cooljure.core] )
  (:gen-class))

(s/set-fn-validation! true)

(def Array
  "A 2-D array of values (a vector of vectors)."
  [[s/Any]] )

(s/defn create :- Array
  "([nrows ncols] [nrows ncols init-val])
  Return a new Array of size=[nrows ncols] initialized to zero (or init-val if supplied)"
  ( [nrows  :- s/Int
     ncols  :- s/Int]
      (create nrows ncols 0))
  ( [nrows      :- s/Int
     ncols      :- s/Int
     init-val   :- s/Any]
    (forv [ii (range nrows)]
      (into [] (repeat ncols init-val)))))

(s/defn num-rows :- s/Int
  "Returns the number of rows of an Array."
  [-array :- Array]
  (count -array))

(s/defn num-cols :- s/Int
  "Returns the number of cols of an Array."
  [-array :- Array]
  (count (-array 0)))

; #todo -> elem-set/elem-get
(s/defn set-elem :- Array
  "Puts a value into an Array element, returning the updated Array."
  [ -array  :- Array
    ii      :- s/Int
    jj      :- s/Int
    newVal  :- s/Any]
  {:pre [ (<= 0 ii) (< ii (num-rows -array))
          (<= 0 jj) (< jj (num-cols -array)) ] }
  (assoc-in -array [ii jj] newVal))

(s/defn get-elem :- s/Any
  "Gets an Array element"
  [ -array  :- Array
    ii      :- s/Int
    jj      :- s/Int ]
  {:pre [ (<= 0 ii) (< ii (num-rows -array))
          (<= 0 jj) (< jj (num-cols -array)) ] }
  (get-in -array [ii jj]))

(s/defn symmetric? :- s/Bool
  "Gets an Array element"
  [ -array  :- Array ]
  (let [nrows (num-rows -array)
        ncols (num-cols -array) ]
    (and  (= nrows ncols)
          (every? truthy? 
            (for [ii (range nrows)
                  jj (range ncols) ]
              (= (get-elem -array ii jj)
                 (get-elem -array jj ii)))))))

(defn disp [-array]
  (dotimes [ii (num-rows -array)]
    (dotimes [jj (num-cols -array)]
      (print (format "%8s" (get-elem -array ii jj))))
    (newline)))


;  (let [
;    num-rows    3
;    num-cols    4
;    work        (make-array Long/TYPE num-rows num-cols)
;  ]
;    (println "start")
;    (println "rows" (count work))
;    (println "cols" (count (aget work 0)))
;    (newline)
;    (dotimes [ii num-rows]
;      (dotimes [jj num-cols]
;        (aset work ii jj  (+ (* 10 ii) jj))))
;    (disp-array work)
;    (println "done")
;  )
;
; (defn is-array? [x] 
;   (-> x class .isArray))
; 
; (defn disp-array [-array]
;   (let [num-rows    (count -array)
;         num-cols    (count (aget -array 0)) 
;   ]
;     (dotimes [ii num-rows]
;       (do
;         (dotimes [jj num-cols]
;           (print (format "%4d" (aget -array ii jj))))
;         (newline)))))

