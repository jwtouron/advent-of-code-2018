(ns advent-of-code-2018.day7
  (require [advent-of-code-2018.utils :refer [get-input-lines]]))

(defn parse-instruction [line]
  (let [[_ x y] (re-matches #"Step (\w+) must be finished before step (\w+) can begin." line)]
    [x y]))

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

(defn solve-part1 [dependency-graph]
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

(assert (= "CABDFE" (solve-part1 example-graph)))
(assert (= "ACHOQRXSEKUGMYIWDZLNBFTJVP" (solve-part1 dependency-graph)))


