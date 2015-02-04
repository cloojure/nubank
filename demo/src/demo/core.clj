(ns demo.core
  (:require
    [clojure.string     :as str]
    [cooljure.parse     :as coolp]
    [demo.array         :as array]
    [schema.core        :as s]
    [schema.test        :as s-tst] )
  (:use [cooljure.core] )
  (:gen-class))

(s/set-fn-validation! true)

(def ^:dynamic *spy* false)

(def Node s/Int)

(def Edge 
  "A vector like [n1 n2] indicated that nodes n1 & n2 are connected by an edge."
  [ (s/one Node "n1")
    (s/one Node "n2") ] )

(def Graph 
  "A map composed of entries like [n0 #{n1 n2 n3...}], such that an edge exists that 
  connects n0 to each of n1, n2, n3 ..."
  { Node #{Node} } )

(s/defn parse-edge :- Edge
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(s/defn accum-edges :- Graph
  "Update an Graph with a new edge symmetrically n1->n2 and n2->n1"
  [ graph   :- Graph 
    edge    :- Edge ]
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
    (assert (= connected1 connected2)) ; must be symmetric
    connected1 ))


(defn load-edges [text]
  (let [
    edge-lines      (str/split-lines text)
    edges           (mapv parse-edge edge-lines)
  ]
    (s/validate [Edge] edges)
    (when *spy* (spyx edges))
    edges ))

(defn load-graph [text]
  (let [
    edges   (load-edges text)
    graph   (reduce accum-edges (sorted-map) edges)
  ]
    (when *spy* (spyx graph))
    (when *spy* (spyx (all-nodes graph)))
    graph))

(defn shortest-path [graph]
  (let [
    N           (count (all-nodes graph))
    dist        (atom (array/create N N 1e99))
  ]
    (doseq [ ii (keys graph) ]
      (swap! dist array/set-elem ii ii 0))
    (doseq [ ii (keys graph)
             jj (neighbors graph ii) ]
      (swap! dist array/set-elem ii jj 1))
    (array/disp @dist)

    (dotimes [kk N]
      (dotimes [ii N]
        (dotimes [jj N]
          (let [dist-sum    (+ (array/get-elem @dist ii kk)
                               (array/get-elem @dist kk jj))
                dist-ij     (array/get-elem @dist ii jj)
          ]
          (when (< dist-sum dist-ij)
            (swap! dist array/set-elem ii jj dist-sum)))))
    )
    (newline)
    (println "dist:")
    (array/disp @dist)
    (assert (array/symmetric? @dist))
  ))

(def edges-filename "edges.txt")

(defn -main []
; (tst-array)

  (binding [*spy* true]
    (let [
      text        (slurp edges-filename)
      graph       (load-graph text)
    ]
      (shortest-path graph)
    )))
    
