(ns advent-of-code-2018.day5
  (require [advent-of-code-2018.utils :refer [get-input-lines]]
           [clojure.string :refer [lower-case]]
           [clojure.core.match :refer [match]])
  (import [java.lang.math]))

(def input
  (-> "day5.txt"
      get-input-lines
      first))

(defn run-reactions
  ([polymer]
   (run-reactions polymer nil))
  ([polymer removals]
   (let [removals (set removals)]
     (loop [before '()
            after (seq polymer)]
       (match [(first before) (first after)]
              [_ (x :guard removals)] (recur before (next after))
              [nil x]                 (recur (cons x before) (next after))
              [x nil]                 before
              [b a]                   (if (and (not= b a)
                                               (= (lower-case b) (lower-case a)))
                                        (recur (next before) (next after))
                                        (recur (cons a before) (next after))))))))

(defn solve-part1 [polymer]
  (count (run-reactions polymer)))
(assert (= (solve-part1 input) 11668))

(defn solve-part2 [polymer]
  (as-> polymer $$$
    (for [removals (partition 2 "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ")]
      (run-reactions $$$ removals))
    (apply min-key count $$$)
    (count $$$)))
(assert (= 4652 (solve-part2 input)))
