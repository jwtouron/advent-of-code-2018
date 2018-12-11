(ns advent-of-code-2018.day8
  (require [clojure.java.io :as io]
           [advent-of-code-2018.utils :refer [->long]]))

(def input
  (-> "day8.txt"
      io/resource
      io/file
      slurp
      (clojure.string/split #"\s+")
      (as-> xs (map ->long xs))))

(def example
  [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(declare parse-node)

(defn parse-children [num-children data]
  {:pre [(vector? data)]}
  (loop [data data
         children []]
    (if (= (count children) num-children)
      [children data]
      (let [[node data] (parse-node data)]
        (recur data (conj children node))))))

(defn parse-metadata [num-metadata data]
  {:pre [(vector? data)]}
  (let [metadata (subvec data 0 num-metadata)
        data (subvec data num-metadata)]
    [metadata data]))

(defn parse-node [data]
  {:pre [(vector? data)]}
  (let [num-children (nth data 0)
        num-metadata (nth data 1)
        data (subvec data 2)
        [children data] (parse-children num-children data)
        [metadata data] (parse-metadata num-metadata data)]
    [{:children children :metadata metadata} data]))

(defn solve-part1 [data]
  (apply + (apply concat (map :metadata (tree-seq map? :children (first (parse-node data)))))))

(assert (= (solve-part1 (into [] input)) 40036))

(defn node-value [node]
  (if-not (seq (node :children))
    (apply + (node :metadata))
    (let [children (node :children)
          metadata (node :metadata)]
      (apply + (map (fn [x]
                      (if (> x (count children))
                        0
                        (node-value (nth children (dec x)))))
                    metadata)))))

(defn solve-part2 [data]
  (let [tree (first (parse-node data))]
    (node-value tree)))

(assert (= (solve-part2 (into [] input)) 21677))
