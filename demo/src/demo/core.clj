(ns demo.core
  (:require
    [clojure.string     :as str]
    [cooljure.parse     :as coolp]
    [schema.core        :as s]
    [schema.test        :as s-tst] )
  (:use [cooljure.core] )
  (:gen-class))

(s/set-fn-validation! true)


(def edges-filename "edges.txt")

(def Node s/Int)

(def Edge 
  "A vector like [n1 n2] indicated that nodes n1 & n2 are connected by an edge."
  [ (s/one Node "n1")
    (s/one Node "n2") ] )

(def Graph 
  "A map composed of entries like [n0 #{n1 n2 n3...}], such that an edge exists that 
  connects n0 to each of n1, n2, n3 ..."
  { Node #{Node} } )

(def Array
  "A 2-D array of values (an array of arrays)."
  [[s/Any]] )

(s/defn newArray
  "([nrows ncols] [nrows ncols init-val])
  Return a new Array of size=[nrows ncols] initialized to zero (or init-val if supplied)"
  ( [nrows ncols]
      (newArray nrows ncols 0))
  ( [nrows ncols init-val]
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

(s/defn array-set :- Array
  "Puts a value into an Array element, returning the updated Array."
  [ -array  :- Array
    ii      :- s/Int
    jj      :- s/Int
    newVal  :- s/Any]
  {:pre [ (<= 0 ii) (< ii (num-rows -array))
          (<= 0 jj) (< jj (num-cols -array)) ] }
  (assoc-in -array [ii jj] newVal))

(s/defn array-get :- s/Any
  "Gets an Array element"
  [ -array  :- Array
    ii      :- s/Int
    jj      :- s/Int ]
  {:pre [ (<= 0 ii) (< ii (num-rows -array))
          (<= 0 jj) (< jj (num-cols -array)) ] }
  (get-in -array [ii jj]))

(defn disp-array [-array]
  (dotimes [ii (num-rows -array)]
    (dotimes [jj (num-cols -array)]
      (print (format "%4s" (array-get -array ii jj))))
    (newline)))

(s/defn parse-edge :- Edge
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(s/defn accum-edges :- Graph
  "Update an Graph with a new edge."
  [ graph       :- Graph 
    edge            :- Edge ]
  (let [ [n1 n2]  edge ]
    (as-> graph result
      (update-in result [n1]  (fnil conj (sorted-set))  n2)
      (update-in result [n2]  (fnil conj (sorted-set))  n1))))

(s/defn all-nodes :- #{Node}
  "Returns a set of all nodes in the graph"
  [graph :- Graph]
  (into (sorted-set)
    (flatten
      [ (keys graph)  (map seq (vals graph)) ] )))

(s/defn neighbors :- #{Node}
  "Returns the set of the neighbors for a node"
  [ graph   :- Graph
    node    :- Node ]
  {:pre [ (contains? graph node) ] }
  (graph node))

(s/defn connected? :- s/Bool
  "Returns true if two nodes are connected by an edge."
  [ graph   :- Graph
    n1      :- Node
    n2      :- Node ]
  {:pre [ (contains? graph n1) 
          (contains? graph n2) ] }
  (let [connected1   (contains? (neighbors graph n1) n2)
        connected2   (contains? (neighbors graph n2) n1) 
  ]
    (assert (= connected1 connected2))
    connected1 ))

(defn make-matrix 
  [num-rows num-cols]
  (into []
    (for [ii (range num-rows)]
      (vec (repeat num-cols 0)))))

(defn -main []
  (let [
    num-rows    3
    num-cols    4
    work        (atom (make-matrix num-rows num-cols))
  ]
    (println "start")
    (spyx @work)
    (println "rows" (count @work))
    (println "cols" (count (@work 0)))
    (disp-array @work)
    (newline)
    (dotimes [ii num-rows]
      (dotimes [jj num-cols]
        (swap! work assoc-in [ii jj]  (+ (* 10 ii) jj))))
    (disp-array @work)
    (println "done")
  )

  #_(let [
    num-rows    3
    num-cols    4
    work        (make-array Long/TYPE num-rows num-cols)
  ]
    (println "start")
    (println "rows" (count work))
    (println "cols" (count (aget work 0)))
    (newline)
    (dotimes [ii num-rows]
      (dotimes [jj num-cols]
        (aset work ii jj  (+ (* 10 ii) jj))))
    (disp-array work)
    (println "done")
  )

  #_(let [
    edge-lines      (str/split-lines (slurp edges-filename))
    edges           (mapv parse-edge edge-lines)
    -- (s/validate [Edge] edges)
    -- (spyx edges)
    graph           (reduce accum-edges (sorted-map) edges)
  ]
    (spyx graph)
    (spyx (all-nodes graph))
  )
)
    
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
; 
