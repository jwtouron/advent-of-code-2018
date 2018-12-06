(ns advent-of-code-2018.day3
  (require [advent-of-code-2018.utils :refer [get-input-lines]]
           [clojure.set])
  (import [java.lang.math]))

(defn ->long [x] (Long/parseLong x))

(defn parse-input-line [line]
  (let [[_ id from-left from-top width height] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)]
    {:id (->long id)
     :from-left (->long from-left)
     :from-top (->long from-top)
     :width (->long width)
     :height (->long height)}))

(def day3-input
  (->> "day3.txt"
       get-input-lines
       (map parse-input-line)))

(defn calc-claim-cells [{:keys [from-left from-top width height]}]
  (for [r (range from-top (+ from-top height))
        c (range from-left (+ from-left width))]
    [r c]))

(defn calc-cell-counts [claims]
  (reduce (fn [counts claim]
            (reduce #(update %1 %2 (fnil inc 0))
                    counts
                    (calc-claim-cells claim)))
          {}
          claims))

(def cell-counts (calc-cell-counts day3-input))

(def part1-solution
  (count (filter (fn [[_ x]] (> x 1)) cell-counts)))
(assert (= part1-solution 114946))

(defn claim-overlaps? [claim cell-counts]
  (let [cells (calc-claim-cells claim)]
    (reduce (fn [b cell]
              (if (> (cell-counts cell) 1)
                (reduced true)
                false))
            false
            cells)))

(def part2-solution
  (loop [claims day3-input]
    (let [claim (first claims)
          claims (next claims)]
      (if (claim-overlaps? claim cell-counts)
        (recur claims)
        (:id claim)))))
(assert (= 877 part2-solution))
