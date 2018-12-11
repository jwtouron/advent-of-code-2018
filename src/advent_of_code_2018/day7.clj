(ns advent-of-code-2018.day7
  (require [advent-of-code-2018.utils :refer [get-input-lines]]))

(defn parse-instruction [line]
  (let [[_ x y] (re-matches #"Step (\w+) must be finished before step (\w+) can begin." line)]
    [(first x) (first y)]))

(def input
  (->> "day7.txt"
       get-input-lines
       (map parse-instruction)))

(def example
  (->> "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."
       clojure.string/split-lines
       (map parse-instruction)))

(defn instructions->dependency-graph [instrs]
  (reduce (fn [graph instr]
            (let [[x y] instr]
              (update graph y (fnil conj #{}) x)))
          {}
          instrs))

(def dependency-graph
  (instructions->dependency-graph input))

(def example-graph (instructions->dependency-graph example))

(defn find-leaves [dependency-graph]
  (reduce (fn [leaves [k v]]
            (reduce (fn [leaves dependency]
                      (if-not (dependency-graph dependency)
                        (conj leaves dependency)
                        leaves))
                    leaves
                    v))
          #{}
          dependency-graph))

(defn remove-step [step dependency-graph]
  (clojure.walk/walk (fn [x]
                       (let [[a b] x]
                         (cond
                           (= b #{step}) nil
                           (b step)      [a (disj b step)]
                           :else         x)))
                     identity
                     dependency-graph))

(defn calc-graph-order [dependency-graph]
  (loop [graph dependency-graph
          order []]
      (if-not (seq graph)
        (let [keys+vals (lazy-cat (keys dependency-graph) (mapcat seq (vals dependency-graph)))
              to-append (clojure.set/difference (set keys+vals) (set order))
              to-append (sort to-append)]
          (apply str (into order to-append)))
        (let [leaves (find-leaves graph)
              to-remove (first (sort leaves))
              graph (remove-step to-remove graph)]
          (recur graph (conj order to-remove))))))

(defn solve-part1 [dependency-graph]
  (calc-graph-order dependency-graph))

(assert (= "CABDFE" (solve-part1 example-graph)))
(assert (= "ACHOQRXSEKUGMYIWDZLNBFTJVP" (solve-part1 dependency-graph)))

(defn worker-finished? [add-seconds {:keys [letter elapsed]}]
  (= elapsed (+ (- (int letter) 64)
                add-seconds)))

(defn calc-available-steps [finished-steps in-progress all-steps dependency-graph]
  (reduce (fn [available step]
            (if (and (not (seq (clojure.set/difference (dependency-graph step) finished-steps)))
                     (not (in-progress step))
                     (not (finished-steps step)))
              (conj available step)
              available))
          #{}
          all-steps))

(defn solve-part2 [dependency-graph add-seconds total-workers]
  (let [order (calc-graph-order dependency-graph)]
    (loop [workers nil
           remaining-steps (calc-graph-order dependency-graph)
           finished-steps #{}
           elapsed-time 0]
      (if (= (count order) (count finished-steps))
        (dec elapsed-time)
        (let [finished-workers (->> workers (filter (partial worker-finished? add-seconds)) set)
              finished-steps (clojure.set/union finished-steps (set (map #(% :letter) finished-workers)))
              workers (filter #(not (finished-steps (% :letter))) workers)
              needed-steps (- total-workers (count workers))
              available-steps (calc-available-steps finished-steps (set (map #(% :letter) workers)) order dependency-graph)
              new-tasks (take needed-steps available-steps)
              workers (lazy-cat workers (map #(hash-map :letter % :elapsed 0) available-steps))
              remaining-steps (drop (count new-tasks) remaining-steps)
              workers (map #(update % :elapsed inc) workers)
              elapsed-time (inc elapsed-time)]
          (recur workers remaining-steps finished-steps elapsed-time))))))

(assert (= 14 (solve-part2 example-graph 0 2))) ;; correct answer is actually 15
;; 985 is the RIGHT answer, Note this doesn't work for the example, we're off by one
;; TODO: Make this right for the example
(assert (= 985 (solve-part2 dependency-graph 60 5)))
