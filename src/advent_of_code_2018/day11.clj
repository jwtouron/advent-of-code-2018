(ns advent-of-code-2018.day11
  (require [advent-of-code-2018.utils :refer [->long]]
           [clojure.core.match :refer [match]]))

(set! *warn-on-reflection* true)

(def input 6042)

(defn cell-power-level [cell serial-num]
  (let [[x y] cell
        rack-id (+ x 10)
        power-level (* rack-id y)
        power-level (+ power-level serial-num)
        power-level (* power-level rack-id)
        power-level (first (drop 2 (reverse (str power-level))))
        power-level (- (->long (str power-level)) 5)]
    power-level))

(defn square-power-level [c+p [x y] size]
  (let [d (c+p [(+ x (dec size)) (+ y (dec size))])
        [a b c] (match [x y]
                       [0 0]   [0 0 0]
                       [x1 0]  [0 0 (get c+p [(dec x) (+ y (dec size))] 0)]
                       [0 y1]  [0 (c+p [(+ x (dec size)) (dec y)]) 0]
                       [x1 y1] [(get c+p [(dec x) (dec y)] 0)
                                (get c+p [(+ x (dec size)) (dec y)] 0)
                                (get c+p [(dec x) (+ y (dec size))] 0)])
        _ nil #_(println a b c d)]
    (- (+ a d)
       b
       c)))

(defn make-summed-area-table [cells]
  (reduce (fn [cells [x y]]
              (assoc cells [x y] (- (+ (get cells [x y] 0)
                                       (get cells [(dec x) y] 0)
                                       (get cells [x (dec y)] 0))
                                    (get cells [(dec x) (dec y)] 0))))
          cells
          (map first (sort-by first cells))))

(defn make-cells+power [serial-num]
  (let [cells (apply merge (for [x (range 1 301)
                                 y (range 1 301)]
                             {[x y] (cell-power-level [x y] serial-num)}))]
    (make-summed-area-table cells)))

(defn find-most-powerful-square [c+p size]
  (let [top-lefts (for [x (range 1 (- 302 size))
                        y (range 1 (- 302 size))]
                    [x y])]
    (apply max-key #(nth % 1) (map #(vector % (square-power-level c+p % size)) top-lefts))))

(defn solve-part1 [serial-num]
  (first (find-most-powerful-square (make-cells+power serial-num) 3)))
(assert (= (solve-part1 input) [21 61]))

(defn solve-part2 [serial-num]
  (let [c+p (make-cells+power serial-num)
        mps (for [size (range 1 301)]
              [(find-most-powerful-square c+p size) size])
        [[[x y] p] s] (apply max-key #(nth (nth % 0) 1) mps)]
    [x y s]))
;; (assert (= (solve-part2 18) [90 269 16]))
;; (assert (= (solve-part2 42) [232 251 12]))

(assert (= (solve-part2 input) [232 251 12]))

