(ns advent-of-code-2018.day2
  (require [advent-of-code-2018.utils :refer [get-input-lines]]))


(def day2-input
  (get-input-lines "day2.txt"))

(defn count-letters [letters]
  (reduce (fn [m l]
            (update-in m [l] (fnil inc 0)))
          {}
          letters))

(defn has-letters-twice+thrice [letters]
  (reduce (fn [[twice thrice] [l count]]
            (let [new-state [(or (= count 2) twice)
                             (or (= count 3) thrice)]]
              (if (= [true true] new-state)
                (reduced new-state)
                new-state)))
          [false false]
          (count-letters letters)))

(def part1-solution
  (let [[twice thrice]
        (reduce (fn [[twice-total thrice-total] letters]
                  (let [[twice thrice] (has-letters-twice+thrice letters)]
                    [(if twice
                       (inc twice-total)
                       twice-total)
                     (if thrice
                       (inc thrice-total)
                       thrice-total)]))
                [0 0]
                day2-input)]
    (* twice thrice)))
(assert (= part1-solution 8715))

(defn differ-by-one? [word1 word2]
  (let [differ-count
        (reduce (fn [count [c1 c2]]
                  (if (not= c1 c2)
                    (inc count)
                    count))
                0
                (mapv vector word1 word2))]
    (= differ-count 1)))

(defn common-letters [word1 word2]
  (->> (map vector word1 word2)
       (filter #(apply = %))
       (map first)))

(def part2-solution
  (->> (for [word1 day2-input
             word2 day2-input
             :when (differ-by-one? word1 word2)]
         (common-letters word1 word2))
       first
       (apply str)))
(assert (= part2-solution "fvstwblgqkhpuixdrnevmaycd"))
