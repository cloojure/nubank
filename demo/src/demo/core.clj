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

(def graph 
  "The current graph"
  (atom (sorted-map)))

(defn reset 
  "Reset graph to empty"
  []
  (reset! graph (sorted-map)))

(s/defn parse-edge :- Edge
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(s/defn num-edges :- s/Int
  "Returns the number of (symmetric) edges in the graph"
  []
  (let [edges-dest  (vals @graph)
        dest-sum    (reduce + 0 (mapv count edges-dest))
        result      (/ dest-sum 2)
  ]
    result ))

(s/defn all-nodes :- #{Node}
  "Returns a set of all nodes in the graph"
  []
  (into (sorted-set)
    (flatten
      [ (keys @graph)  (map seq (vals @graph)) ] )))

(s/defn neighbors :- #{Node}
  "Returns the set of the neighbors for a node"
  [ node :- Node ]
  {:pre [ (contains? @graph node) ] }
  (@graph node))

(s/defn connected? :- s/Bool
  "Returns true if two nodes are connected by an edge."
  [ n1 :- Node
    n2 :- Node ]
  {:pre [ (contains? @graph n1) 
          (contains? @graph n2) ] }
  (let [connected1   (contains? (neighbors n1) n2)
        connected2   (contains? (neighbors n2) n1) 
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

; #todo rename -> add-edge
(s/defn accum-edges :- nil
  "Update an Graph with a new edge symmetrically n1->n2 and n2->n1"
  [ [n1 n2] :- Edge ]
  (swap! graph update-in [n1]  (fnil conj (sorted-set))  n2)
  (swap! graph update-in [n2]  (fnil conj (sorted-set))  n1))

(s/defn load-graph :- nil
  [text :- s/Str]
  (let [edges       (load-edges text)
        --          (doseq [edge edges] (accum-edges edge))
        edge-sets   (mapv #(into (sorted-set) %) edges)
        edge-freqs  (frequencies edge-sets)
        edge-dups   (filter #(< 1 (val %)) edge-freqs)
  ]
    (println "Duplicate Edges:  Count =" (count edge-dups))
    (println "   Values =" edge-dups)
  ))

(s/defn shortest-path :- darr/Darr
  "Calculates the shortest-path betwen each pair of Nodes"
  []
  (let [N           (count (all-nodes))
        -- (assert (= N (count @graph)))
        dist        (darr/create N N 1e99)
  ]
    (doseq [ ii (keys @graph) ]
      (darr/set-elem dist ii ii 0))
    (doseq [ ii (keys @graph)
             jj (neighbors ii) ]
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
      --        (load-graph text)
      -- (println \newline "graph nodes:" (count @graph) "   edges:"   
                  (as-> (vals @graph) it
                        (mapv count it)
                        (reduce + it)
                        (/ it 2 )))

      spath     (shortest-path)
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
    
