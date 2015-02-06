(ns demo.graph
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

(def edges-filename "edges.txt")
(def edges-filename "edges-full.txt")

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

(def fraud-nodes 
  "A set of nodes flagged as fraudulent"
  (atom (sorted-set)))

(defn reset 
  "Reset graph to empty"
  []
  (reset! graph         (sorted-map))
  (reset! fraud-nodes   (sorted-set)))


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
      (keys @graph)))

(s/defn neighbors :- #{Node}
  "Returns the set of the neighbors for a node"
  [ node :- Node ]
  {:pre [ (contains? @graph node) ] }
  (@graph node))

(s/defn connected? :- s/Bool
  "Returns true if two nodes exist in the graph and are connected by an edge."
  [ n1 :- Node
    n2 :- Node ]
  (if (and  (contains? @graph n1) 
            (contains? @graph n2))
    (let [connected1   (contains? (neighbors n1) n2)
          connected2   (contains? (neighbors n2) n1) 
    ]
      (assert (= connected1 connected2)) ; must be symmetric
      connected1 )))

(s/defn parse-edge :- Edge
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(s/defn load-edges :- [Edge]
  [text :- s/Str]
  (mapv parse-edge (str/split-lines text)))

(s/defn add-edge :- nil
  "Update an Graph with a new edge symmetrically n1->n2 and n2->n1"
  [ [n1 n2] :- Edge ]
  (swap! graph update-in [n1]  (fnil conj (sorted-set))  n2)
  (swap! graph update-in [n2]  (fnil conj (sorted-set))  n1))

(s/defn load-graph :- nil
  [text :- s/Str]
  (let [edges       (load-edges text)
        --          (doseq [edge edges] (add-edge edge))
        edge-sets   (mapv #(into (sorted-set) %) edges)
        edge-freqs  (frequencies edge-sets)
        edge-dups   (filter #(< 1 (val %)) edge-freqs)
  ]
    (newline)
    (println (str "load-graph:  duplicate edges (" (count edge-dups) "):")
                edge-dups)
  ))

(s/defn shortest-path :- array/Array
  "Calculates the shortest-path betwen each pair of Nodes"
  []
  (let [N       (count (all-nodes))
        dist    (darr/create N N 1e99)
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
    (let [result-array
            (into [] (for [ii (range N)]
              (into [] (for [jj (range N)]
                (int (darr/get-elem dist ii jj))))))
    ] result-array )))

(s/defn calc-penalty :- s/Num
  "Calculate the closeness score penalty for 2 nodes."
  [ node-dist   :- array/Array
    n1          :- Node
    n2          :- Node ]
    (let [dist      (array/get-elem node-dist n1 n2)
          penalty   (- 1 (Math/pow 0.5 dist))
    ] penalty ))

(s/defn fraud-adjust :- [s/Num]
  "Use node distance to adjust closeness for fraud"
  [ closeness   :- [s/Num]
    node-dist   :- array/Array ]
  (into []
    (for [node-idx (range (count closeness))]
      (let [penalties         
                (mapv   #(calc-penalty node-dist node-idx %)
                                      @fraud-nodes)
            total-penalty     (apply * penalties)
      ] 
        (* (closeness node-idx) total-penalty)))))

(defn calc-closeness []
  (let [
    node-dist       (shortest-path)
    farness         (forv [ii  (range (array/num-rows node-dist)) ]
                      (apply + (node-dist ii)))
    closeness-raw   (mapv #(/ 1 %) farness)
    result          (fraud-adjust closeness-raw node-dist)
  ] result ))

(defn sorted-closeness []
  (let [
    closeness   (calc-closeness)
    result      (sort-by :closeness > 
                  (mapv #(hash-map :closeness %1  :node %2)  
                        closeness 
                        (range (count closeness))))
  ] result ))

(defn -main []
  (binding [*spy* false
            *show-status* true ]
    (let [
      text      (slurp edges-filename)
        -- (println \newline "lines read:" (count (str/split-lines text)))
        -- (load-graph text)
        -- (println \newline "graph nodes:" (count @graph) "   edges:"   
                    (as-> (vals @graph) it
                          (mapv count it)
                          (reduce + it)
                          (/ it 2 )))
    ]
      (newline)
      (println "top nodes by closeness:" (take 20 (sorted-closeness)))
    )))
    
