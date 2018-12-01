(ns advent-of-code-2018.day1
  (require [clojure.java.io :as io])
  (import [java.lang.math]))

(def day1-input
  (->> "day1.txt"
       io/resource
       io/file
       slurp
       clojure.string/split-lines
       (map #(Long/parseLong %))))

(def part1-solution
  (apply + day1-input))
(assert (= part1-solution 582))

(def part2-solution
  (let [input (cycle day1-input)]
    (reduce (fn [[current seen] change]
              (let [new-current (+ current change)]
                (if (seen new-current)
                  (reduced new-current)
                  [new-current (conj seen new-current)])))
            [0 #{}]
            input)))
(assert (= part2-solution 488))
