(ns advent-of-code-2018.day12)

(def initial-state
  (map #(vector %1 %2)
       (range)
       "##...#......##......#.####.##.#..#..####.#.######.##..#.####...##....#.#.####.####.#..#.######.##..."))

(def transitions
  {[\# \. \. \. \.] \.
   [\# \. \. \# \#] \#
   [\. \. \. \. \#] \.
   [\. \. \. \# \.] \.
   [\. \. \. \# \#] \#
   [\# \. \# \. \#] \.
   [\. \# \. \. \.] \#
   [\# \# \. \# \.] \.
   [\. \. \# \. \#] \.
   [\. \# \# \. \#] \#
   [\# \# \# \. \#] \#
   [\. \# \. \# \#] \.
   [\. \. \. \. \.] \.
   [\# \# \# \# \#] \#
   [\# \# \# \. \.] \.
   [\# \# \. \. \#] \#
   [\# \. \# \# \#] \#
   [\# \. \# \. \.] \.
   [\. \. \# \# \#] \.
   [\. \. \# \. \.] \.
   [\. \# \. \. \#] \#
   [\. \# \# \. \.] \#
   [\# \# \. \. \.] \#
   [\. \# \. \# \.] \#
   [\. \# \# \# \.] \#
   [\# \. \. \# \.] \.
   [\# \# \# \# \.] \.
   [\. \# \# \# \#] \#
   [\# \. \# \# \.] \#
   [\# \# \. \# \#] \.
   [\. \. \# \# \.] \.
   [\# \. \. \. \#] \#})

(def example-initial-state
  (map #(vector %1 %2) (range) "#..#.#..##......###...###"))

(def example-transitions
  {[\. \. \. \# \#] \#
   [\. \. \# \. \.] \#
   [\. \# \. \. \.] \#
   [\. \# \. \# \.] \#
   [\. \# \. \# \#] \#
   [\. \# \# \. \.] \#
   [\. \# \# \# \#] \#
   [\# \. \# \. \#] \#
   [\# \. \# \# \#] \#
   [\# \# \. \# \.] \#
   [\# \# \. \# \#] \#
   [\# \# \# \. \.] \#
   [\# \# \# \. \#] \#
   [\# \# \# \# \.] \#})

(defn state->zipper [state]
  (let [needed-dots (- 4 (count (take-while #(= (nth % 1) \.) state)))
        first-index (ffirst state)
        state (into [] (concat (map #(vector % \.) (range (- first-index needed-dots) first-index)) state))]
    [(subvec state 0 2) (nth state 2) (subvec state 3)]))

(defn zipper-move-right [[before current after]]
  [(conj before current) (or (first after) [(inc (first current)) \.]) (into [] (next after))])

(defn zipper->llcrr [[before current after]]
  (let [i (nth current 0)
        before (subvec before (- (count before) 2))
        after (case (count after)
                0 [[(+ i 1) \.] [(+ i 2) \.]]
                1 [(first after) [(+ i 2) \.]]
                (subvec after 0 2))]
    (mapv second (concat before [current] after))))

(defn zipper-at-end? [zipper]
  (let [[_ _ after] zipper
        llcrr (zipper->llcrr zipper)]
    (and (= after [])
         (= llcrr [\. \. \. \. \.]))))

(defn step-state [transitions state]
  (let [zipper (state->zipper state)]
    (loop [zipper zipper
           next-state []]
      (if (zipper-at-end? zipper)
        (conj next-state [(get-in zipper [1 0]) (get transitions (zipper->llcrr zipper) \.)])
        (let [transition (zipper->llcrr zipper)]
          (recur (zipper-move-right zipper)
                 (conj next-state [(get-in zipper [1 0]) (get transitions transition \.)])))))))

(defn step-state-many [transitions state n]
  (loop [i 0
         state state]
    (if (= i n)
      state
      (recur (inc i) (step-state transitions state)))))

(defn sum-state [state]
  (->> state
       (filter #(= (nth % 1) \#))
       (map first)
       (apply +)))

(defn solve-part1 [transitions state generations]
  (let [state (step-state-many transitions state generations)]
    (sum-state state)))

;; part 1
(assert (= (solve-part1 example-transitions example-initial-state 20) 325))
(assert (= (solve-part1 transitions initial-state 20) 3421))

;; Part 2
;; NOTE: This answer is derived through observing the behavior of the states
(defn solve-part2 []
  (let [score (loop [i 0
                     state initial-state
                     previous-score 0]
                (if (= i 100)
                  previous-score
                  (let [next-state (step-state transitions state)
                        next-score (sum-state state)]
                    (recur (inc i) next-state next-score))))]
    (+ (* (- 50000000000 99)
          51)
       score)))
(assert (= (solve-part2) 2550000001195))
