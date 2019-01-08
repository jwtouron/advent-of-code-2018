(ns advent-of-code-2018.utils
  (:require [clojure.java.io :as io]))

(defn a* [g-score h-score neighbors dist-between start goal]
  (loop [closed-set #{}
         open-set (sorted-set start)
         came-from {}
         g-scores {start 0}
         f-scores {start (h-score start goal)}]
    (when (not (empty open-set))
      (let [current (peek open-set)]
        (if (= current goal)
          :made-it
          (let [open-set (disj open-set current)
                closed-set (conj closed-set current)
                neighbors (filter (comp not closed-set)
                                  (neighbors current))
                [] (loop [neighbors neighbors]
                     (if-not (seq neighbors)
                       []
                       (let [neighbor (first neighbors)
                             neighbors (next neighbors)
                             tentative-g-score (+ (g-scores current)
                                                  (dist-between current neighbor))]
                         )))]))))))
            

                   
          

(defn get-input-lines [file]
  (-> file
      io/resource
      io/file
      slurp
      clojure.string/split-lines))

(defn ->long [x]
  (Long/parseLong x))
