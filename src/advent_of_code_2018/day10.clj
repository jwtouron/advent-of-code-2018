(ns advent-of-code-2018.day10
  (require [advent-of-code-2018.utils :refer [get-input-lines ->long]]))

; position=<-30175,  10254> velocity=< 3, -1>
(defn parse-line [line]
  (let [[_ X Y x y] (re-matches #"position=<\s*(-?\d+),\s+(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>" line)]
    [(->long X) (->long Y) (->long x) (->long y)]))

(def input
  (-> "day10.txt"
      get-input-lines
      (as-> xs (map parse-line xs))))

(def example
  (-> "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>"
      clojure.string/split-lines
      (as-> xs (map parse-line xs))))

(defn print-points [points]
  (let [points (set (map #(into [] (take 2 %)) points))
        xmin (nth (apply min-key #(nth % 0) points) 0)
        xmax (nth (apply max-key #(nth % 0) points) 0)
        ymin (nth (apply min-key #(nth % 1) points) 1)
        ymax (nth (apply max-key #(nth % 1) points) 1)
        line (for [r (range ymin (inc ymax))
                   c (range xmin (inc xmax))]
               (if (points [c r]) \# \.))
        lines (->> line
                   (partition (inc (- xmax xmin)))
                   (map (partial apply str)))]
    (doseq [line lines]
      (println line))))

(defn advance-points [points]
  (map (fn [[X Y x y]]
         [(+ X x) (+ Y y) x y])
       points))

(defn calc-area [points]
  (let [points (map #(take 2 %) points)
        xmin (nth (apply min-key #(nth % 0) points) 0)
        xmax (nth (apply max-key #(nth % 0) points) 0)
        ymin (nth (apply min-key #(nth % 1) points) 1)
        ymax (nth (apply max-key #(nth % 1) points) 1)]
    (* (- xmax xmin) (- ymax ymin))))

(loop [i 0
       points example]
  (when (< i 4)
    (print-points points)
    (println "area:" (calc-area points))
    (recur (inc i) (advance-points points))))

(defn solve-step [points]
  (println n)
  (print-points points xshift grid-xmax yshift grid-ymax)
  (Thread/sleep 2000)
  (recur (step-points points) xshift grid-xmax yshift grid-ymax (inc n)))

(defn solve [points]
  (loop [i -1
         last-area (/ 1.0 0)
         last-points nil
         points points]
    (let [area (calc-area points)]
      (if (> area last-area)
        (do
          (println "i:" i)
          (print-points last-points))
        (let [new-points (advance-points points)]
          (recur (inc i) area points new-points))))))
(solve input) ;; XECXBPZB
