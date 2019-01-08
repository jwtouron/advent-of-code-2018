(ns advent-of-code-2018.day15
  (:require [advent-of-code-2018.utils :refer [get-input-lines]]))

(defn parse-line [row line]
  (reduce (fn [m [col chr]]
            (let [key (case chr
                        \# :wall
                        \. :open
                        \G :goblin
                        \E :elf)]
              (update m key (fnil conj #{}) [row col])))
         #_   (assoc m [row col] chr)
          {}
          (map vector (range) line)))

(def input
  (->> "day16.txt"
       get-input-lines
       (map parse-line (range))
       (apply merge-with clojure.set/union)))


