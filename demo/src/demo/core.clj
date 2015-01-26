(ns demo.core
  (:require
    [clojure.string     :as str]
    [cooljure.parse     :as coolp]
    [schema.core        :as s]
    [schema.test        :as s-tst] )
  (:use [cooljure.core] )
  (:gen-class))

; (s/set-fn-validation! true)

(def edges-filename "edges.txt")

(def IntPair [ (s/one s/Int "n1")
               (s/one s/Int "n2") ] )

(s/defn parse-edge :- IntPair
  "Parse a string of the form 'n1 n2', returning a vector like [n1 n2]" 
  [line-str :- s/Str]
  (mapv coolp/parse-int (re-seq #"\d+" line-str)))

(defn -main []
  (let [
    edge-lines      (str/split-lines (slurp edges-filename))
    edges           (mapv parse-edge edge-lines)
  ]
    (s/validate [IntPair] edges)
    (spyx edges)
  ))
    
