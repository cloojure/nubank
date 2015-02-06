(ns demo.graph
  (:require
    [clojure.string     :as str]
    [clojure.set        :as set]
    [cooljure.parse     :as coolp]
    [demo.array         :as array]
    [demo.darr          :as darr]
    [hiphip.array       :as harr]
    [hiphip.double      :as hd]
    [schema.core        :as s] )
  (:use [cooljure.core] )
  (:gen-class))

(s/set-fn-validation! true)

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


(def state
  "The current system state"
  (atom { 
    :node->idx      nil ; map from node id to consecutive array index
    :graph          nil ; the current graph
    :fraud-nodes    nil ; A set of nodes flagged as fraudulent
  } ))

(defn reset 
  "Reset graph to empty"
  []
  (swap! state assoc-in [:node->idx]    (hash-map))
  (swap! state assoc-in [:graph]        (sorted-map))
  (swap! state assoc-in [:fraud-nodes]  (sorted-set)))
(do (println "Initializing system state...")
    (reset))

(s/defn node-idx :- Node
  "Returns the array idx for a node-id, assigning a new index if necessary"
  [node-id :- s/Any]
  (let [curr-map    (@state :node->idx)
        idx         (get curr-map node-id :fail) ]
    (if (not= :fail idx)
      idx ; found it
      ;else assign a new idx
      (let [new-idx (count curr-map) ] ; zero-based idx
        (swap! state update-in [:node->idx] conj {node-id new-idx})
        new-idx))))

(s/defn get-graph :- Graph
  "Returns the current graph"
  []
  (@state :graph))

(s/defn get-fraud-nodes :- #{Node}
  "Returns a list of fraud nodes."
  []
  (@state :fraud-nodes))

(s/defn add-fraud :- nil
  [node :- Node]
  (swap! state update-in [:fraud-nodes] conj node))

(s/defn num-edges :- s/Int
  "Returns the number of (symmetric) edges in the graph"
  []
  (let [edges-dest  (vals (@state :graph))
        dest-sum    (reduce + 0 (mapv count edges-dest))
        result      (/ dest-sum 2)
  ]
    result ))

(s/defn all-nodes :- #{Node}
  "Returns a set of all nodes in the graph"
  []
  (into (sorted-set)
      (keys (@state :graph))))

(s/defn neighbors :- #{Node}
  "Returns the set of the neighbors for a node"
  [ node :- Node ]
  (let [result (get-in @state [:graph node]) ]
    (if (truthy? result)
      result
      (throw (Exception. (str "neighbors: missing node: " node))))))

(s/defn connected? :- s/Bool
  "Returns true if two nodes exist in the graph and are connected by an edge."
  [ n1 :- Node
    n2 :- Node ]
  (let [connected1   (contains? (neighbors n1) n2)
        connected2   (contains? (neighbors n2) n1) 
  ]
    (assert (= connected1 connected2)) ; must be symmetric
    connected1 ))

(s/defn add-edge
  [id-1 :- s/Str
   id-2 :- s/Str ]
  (let [n1  (node-idx id-1)
        n2  (node-idx id-2) ] 
    ; Update graph with a new edge symmetrically n1->n2 and n2->n1
    (swap! state update-in [:graph n1]  (fnil conj (sorted-set))  n2)
    (swap! state update-in [:graph n2]  (fnil conj (sorted-set))  n1)))

(s/defn load-graph :- nil
  "For each line in a file-string, parse a space-delimited string of the form 
  ' id-1 id-2 '.  Add the edge defined by [id-1 id-2] to the graph using zero-based
  indexing."
  [file-str :- s/Str]
  (let [file-lines      (str/split-lines file-str) ]
    (doseq [line-str    file-lines]
      (let [edge-ids    (re-seq #"\S+" line-str) ]
        (assert (= 2 (count edge-ids)))
        (add-edge   (first  edge-ids)
                    (second edge-ids) )))
    (println (format "load-graph:  lines read: %d   edges saved: %d"
                (count file-lines) (num-edges)))
  ))

(s/defn shortest-path :- array/Array
  "Calculates the shortest-path betwen each pair of Nodes"
  []
  (let [N       (count (all-nodes))
        dist    (darr/create N N 1e99)
  ]
    (doseq [ ii (range N) ]
      (darr/set-elem dist ii ii 0))
    (doseq [ ii (range N)
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
                                      (@state :fraud-nodes))
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
    idx->node   (set/map-invert (@state :node->idx))
    result      (sort-by :closeness > 
                  (mapv #(hash-map :closeness %1  :node-id (idx->node %2))
                        closeness 
                        (range (count closeness))))
  ] result ))

(defn -main []
  (let [text  (slurp edges-filename) ]
    (load-graph text)
    (println \newline "graph nodes:" (count (all-nodes)))
    (newline)
    (println "top nodes by closeness:" (take 20 (sorted-closeness)))
  ))
    
