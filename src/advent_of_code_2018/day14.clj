(ns advent-of-code-2018.day14
  (:require [advent-of-code-2018.utils :refer [->long]]))

(def input 760221)

(def new-scoreboard [3 7])

(def new-state {:elf1 0 :elf2 1 :scoreboard new-scoreboard})

(defn create-new-recipes [{:keys [elf1 elf2 scoreboard] :as state}]
  (let [elf1-score (nth scoreboard elf1)
        elf2-score (nth scoreboard elf2)
        new-recipes (->> (+ elf1-score elf2-score) str (map (comp ->long str)))]
    (update state :scoreboard into new-recipes)))

(defn update-elves [{:keys [elf1 elf2 scoreboard] :as state}]
  (let [elf1-score (nth scoreboard elf1)
        elf1-steps (inc elf1-score)
        elf1-score (rem (+ elf1 elf1-steps) (count scoreboard))
        elf2-score (nth scoreboard elf2)
        elf2-steps (inc elf2-score)
        elf2-score (rem (+ elf2 elf2-steps) (count scoreboard))]
    (-> state (assoc :elf1 elf1-score) (assoc :elf2 elf2-score))))

(defn solve-part1 [recipes]
  (loop [state new-state]
    (let [scoreboard (state :scoreboard)]
      (if (>= (count scoreboard) (+ recipes 10))
        (->> scoreboard (drop recipes) (take 10))
        (recur (-> state create-new-recipes update-elves))))))

(assert (= (into [] (solve-part1 9)) [5 1 5 8 9 1 6 7 7 9]))
(assert (= (into [] (solve-part1 5)) [0 1 2 4 5 1 5 8 9 1]))
(assert (= (into [] (solve-part1 18)) [9 2 5 1 0 7 1 0 8 5]))
(assert (= (into [] (solve-part1 2018)) [5 9 4 1 4 2 9 8 8 2]))
(assert (= (into [] (solve-part1 input)) [1 4 1 1 3 8 3 6 2 1]))

(defn solve-part2 [scores]
  (let [num-scores (count scores)]
    (loop [state new-state]
      (let [scoreboard (state :scoreboard)
            sb-length (count scoreboard)]
        (cond
          (and (>= sb-length num-scores) (= (subvec scoreboard (- sb-length num-scores)) scores))
          (- sb-length num-scores)

          (and (>= sb-length (inc num-scores))
               (= (subvec scoreboard (- sb-length num-scores 1) (- sb-length num-scores 1 (- num-scores))) scores))
          (- sb-length num-scores 1)

          :else (recur (-> state create-new-recipes update-elves)))))))

(assert (= (solve-part2 [5 1 5 8 9])) 9)
(assert (= (solve-part2 [0 1 2 4 5])) 5)
(assert (= (solve-part2 [9 2 5 1 0]) 18))
(assert (= (solve-part2 [5 9 4 1 4]) 2018))

(assert (= (solve-part2 [7 6 0 2 2 1]) 20177474))
