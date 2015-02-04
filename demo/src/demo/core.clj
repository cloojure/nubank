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


(s/defn load-edges :- [Edge]
  [text :- s/Str]
  (let [
    edge-lines      (str/split-lines text)
    edges           (mapv parse-edge edge-lines)
  ]
    (s/validate [Edge] edges)
    (when *spy* (spyx edges))
    edges ))

(s/defn load-graph  :- Graph
  [text :- s/Str]
  (let [edges   (load-edges text)
        graph   (reduce accum-edges (sorted-map) edges)
  ]
    (when *spy* (spyx graph))
    (when *spy* (spyx (all-nodes graph)))
    graph))

(s/defn shortest-path :- array/Array
  "Calculates the shortest-path betwen each pair of Nodes"
  [graph :- Graph]
  (let [
    N           (count (all-nodes graph))
    dist        (atom (array/create N N 1e99))
  ]
    (doseq [ ii (keys graph) ]
      (swap! dist array/set-elem ii ii 0))
    (doseq [ ii (keys graph)
             jj (neighbors graph ii) ]
      (swap! dist array/set-elem ii jj 1))

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
    (assert (array/symmetric? @dist))
    @dist
  ))

(s/defn closeness :- [s/Num]
  "Calculates the closeness for each node given the shortest-path array"
  [spath :- array/Array]
  (let [farness     (forv [ii  (range (array/num-rows spath)) ]
                      (apply + (spath ii)))
        closeness   (mapv #(/ 1 %) farness)
  ]
    closeness ))

(def edges-filename "edges.txt")

(defn -main []
  (binding [*spy* true]
    (let [
      text      (slurp edges-filename)
      graph     (load-graph text)
      spath     (shortest-path graph)
      -- (do (println "spath:") (array/disp spath))
      cness     (closeness spath)
      -- (newline)
      -- (spyx cness)
      cness     (sort-by :closeness > 
                  (mapv #(hash-map :closeness %1  :node %2)  
                        cness 
                        (range (count cness))))
    ]
      (newline)
      (spyx cness)
    )))
    
