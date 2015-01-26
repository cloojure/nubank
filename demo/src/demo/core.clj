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

(s/defn parse-edge :- Edge
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(def EdgesMap 
  "A map composed of entries like [n0 #{n1 n2 n3...}], such that an edge exists that 
  connects n0 to each of n1, n2, n3 ..."
  { Node #{Node} } )

(s/defn accum-edges :- EdgesMap
  "Update an EdgesMap with a new edge."
  [ edges-map       :- EdgesMap 
    edge            :- Edge ]
  (let [ [n1 n2]  edge ]
    (as-> edges-map accum
      (update-in accum [n1]  (fnil conj #{})  n2)
      (update-in accum [n2]  (fnil conj #{})  n1))))

(defn -main []
  (let [
    edge-lines      (str/split-lines (slurp edges-filename))
    edges           (mapv parse-edge edge-lines)
    -- (s/validate [Edge] edges)
    -- (spyx edges)
    edges-map       (reduce accum-edges {} edges)
  ]
    (spyx edges-map)
  ))
    
