(ns advent-of-code-2018.day6
  (require [advent-of-code-2018.utils :refer [get-input-lines ->long]]))

(defn parse-coords [x]
  (map ->long (clojure.string/split x #", ")))

(def input
  (->> "day6.txt"
       get-input-lines
       (map parse-coords)))

(def example
  [[1 1]
   [6 1]
   [3 8]
   [4 3]
   [5 5]
   [9 8]])

(defn abs [n]
  (if (neg? n) (- n) n))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn find-closest2 [coord coords]
  (loop [closest (->> coords (take 2) (map #(vector % (distance coord %))))
         coords (drop 2 coords)]
    (if-not (seq coords)
      (->> closest (sort-by #(nth % 1)) (into []))
      (let [coord2 (first coords)
            d (distance coord coord2)
            coords (next coords)
            closest (sort-by #(nth % 1) (conj closest [coord2 d]))]
        (recur (take 2 closest) coords)))))

(defn find-infinites [coords]
  (reduce (fn [infs coord]
            (let [[[c1 d1] [c2 d2]] (find-closest2 coord coords)]
              (if (not= d1 d2)
                (conj infs c1)
                infs)))
          #{}
          (lazy-cat (map #(vector % 4000) (range 400))
                    (map #(vector % -4000) (range 400))
                    (map #(vector 4000 %) (range 400))
                    (map #(vector -4000 %) (range 400)))))

(defn solve-part1 [coords]
  (let [coords2 (for [x (range 400)
                      y (range 400)]
                  [x y])
        infs (find-infinites coords)
        areas (reduce (fn [areas coord]
                        (let [[[c1 d1] [c2 d2]] (find-closest2 coord coords)]
                          (if (or (infs c1) (= d1 d2))
                            areas
                            (update areas c1 (fnil inc 0)))))
                      {}
                      coords2)]
    (apply max-key val areas)))
#_(assert (= 4829 (second (solve-part1 input))))

(defn solve-part2 [coords max-distance]
  (let [coords2 (for [x (range 400)
                      y (range 400)]
                  [x y])]
    (reduce (fn [sum c2]
              (let [total-distance (reduce (fn [sum c1]
                                             (+ sum (distance c1 c2)))
                                           0
                                           coords)]
                (if (< total-distance max-distance)
                  (inc sum)
                  sum)))
            0
            coords2)))

#_(assert (= 46966 (solve-part2 input 10000)))
