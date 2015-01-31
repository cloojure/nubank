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
    (into [] 
      (for [ii (range nrows)]
        (into [] (repeat ncols init-val)))))
  )

(s/defn num-rows :- s/Int
  "Returns the number of rows of an Array."
  [arg :- Array]
  (count arg))

(s/defn num-cols :- s/Int
  "Returns the number of cols of an Array."
  [arg :- Array]
  (count (arg 0)))

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

(defn disp [-array]
  (dotimes [ii (num-rows -array)]
    (dotimes [jj (num-cols -array)]
      (print (format "%4s" (get-elem -array ii jj))))
    (newline)))

