(ns demo.core
  (:require
    [clojure.string     :as str]
    [cooljure.parse     :as coolp]
    [demo.array         :as array]
    [demo.darr          :as darr]
    [hiphip.array       :as harr]
    [hiphip.double      :as hd]
    [schema.core        :as s] )
  (:use [cooljure.core] )
  (:gen-class))

(s/set-fn-validation! true)

(def ^:dynamic *spy* false)
(def ^:dynamic *show-status* false)

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

(s/defn num-edges :- s/Int
  "Returns the number of (symmetric) edges in the graph"
  [graph :- Graph]
  (let [edges-dest  (vals graph)
        dest-sum    (reduce + 0 (mapv count edges-dest))
        result      (/ dest-sum 2)
  ]
    result ))

(s/defn accum-edges :- Graph
  "Update an Graph with a new edge symmetrically n1->n2 and n2->n1"
  [ graph   :- Graph 
    edge    :- Edge ]
  (let [ [n1 n2]    edge 
        result      (as-> graph result
                      (update-in result [n1]  (fnil conj (sorted-set))  n2)
                      (update-in result [n2]  (fnil conj (sorted-set))  n1))
  ]
    result
  ))

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
    edges ))

(s/defn load-graph  :- Graph
  [text :- s/Str]
  (let [edges       (load-edges text)
        graph       (reduce accum-edges (sorted-map) edges)
        edge-sets   (mapv #(into (sorted-set) %) edges)
        edge-freqs  (frequencies edge-sets)
        edge-dups   (filter #(< 1 (val %)) edge-freqs)
  ]
    (println "Duplicate Edges:  Count =" (count edge-dups))
    (println "   Values =" edge-dups)
    graph))

(s/defn shortest-path-0 :- array/Array
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
    (println "shortest-path-0: init done")

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
    (when *show-status* (newline))
    (assert (array/symmetric? @dist))
    @dist
  ))

(s/defn shortest-path :- darr/Darr
  "Calculates the shortest-path betwen each pair of Nodes"
  [graph :- Graph]
  (let [N           (count (all-nodes graph))
        -- (assert (= N (count graph)))
        dist        (darr/create N N 1e99)
  ]
    (doseq [ ii (keys graph) ]
      (darr/set-elem dist ii ii 0))
    (doseq [ ii (keys graph)
             jj (neighbors graph ii) ]
      (darr/set-elem dist ii jj 1))

    (dotimes [kk N]
      (let [^doubles darr-kk  (aget ^objects dist kk) ]
        (dotimes [ii N]
          (let [^doubles darr-ii  (aget ^objects dist ii) ]
            (dotimes [jj N]
              (let [dist-sum    (+ (aget darr-ii kk)
                                   (aget darr-kk jj))
                    dist-ij     (aget darr-ii jj)
              ]
              (when (< dist-sum dist-ij)
                (aset darr-ii jj dist-sum))))))))
    (let [result 
            (into [] (for [ii (range N)]
              (into [] (for [jj (range N)]
                (int (darr/get-elem dist ii jj))))))
    ]
      result )))

(s/defn closeness :- [s/Num]
  "Calculates the closeness for each node given the shortest-path array"
  [spath :- array/Array]
  (let [farness     (forv [ii  (range (array/num-rows spath)) ]
                      (apply + (spath ii)))
        closeness   (mapv #(/ 1 %) farness)
  ]
    closeness ))

(def edges-filename "edges.txt")
(def edges-filename "edges-full.txt")

(defn -main []
  (binding [*spy* false
            *show-status* true ]
    (let [
      text      (slurp edges-filename)
      -- (println \newline "lines read:" (count (str/split-lines text)))
      graph     (load-graph text)
      -- (println \newline "graph nodes:" (count graph)
                  "   edges:" (/ (reduce + (mapv count (vals graph)))
                                 2 ))

      spath     (shortest-path graph)
      cness     (closeness spath)
      -- (newline)
      -- (spyx (take 44 cness))
      cness     (sort-by :closeness > 
                  (mapv #(hash-map :closeness %1  :node %2)  
                        cness 
                        (range (count cness))))
    ]
      (newline)
      (spyx (take 44 cness))
    )))
    
