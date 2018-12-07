(ns advent-of-code-2018.day4
  (require [advent-of-code-2018.utils :refer [get-input-lines]]
           [clj-time.core :as time])
  (import [java.lang.math]))

(defn ->long [x]
  (Long/parseLong x))

(defn parse-begins-shift [line]
  (when-let [[_ year month day hour minute guard-id]
             (re-matches #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] Guard #(\d+) begins shift" line)]
    [:begins-shift (->long year) (->long month) (->long day) (->long hour) (->long minute) (->long guard-id)]))

(defn parse-wakes-up [line]
  (when-let [[_ year month day hour minute]
             (re-matches #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] wakes up" line)]
    [:wakes-up (->long year) (->long month) (->long day) (->long hour) (->long minute)]))

(defn parse-falls-asleep [line]
  (when-let [[_ year month day hour minute]
             (re-matches #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] falls asleep" line)]
    [:falls-asleep (->long year) (->long month) (->long day) (->long hour) (->long minute)]))

(defn parse-log-entry [line]
  (if-let [x (parse-begins-shift line)]
    x
    (if-let [x (parse-wakes-up line)]
      x
      (if-let [x (parse-falls-asleep line)]
        x))))

(defn sort-log-entries [entries]
  (sort-by #(vec (take 5 (next %))) entries))

(def input
  (->> "day4.txt"
       get-input-lines
       (map parse-log-entry)
       sort-log-entries))

(defn partition-entries-by-shift [entries]
  (loop [entries entries
         output []]
    (if (seq entries)
      (let [guard (vector (first entries))
            entries (next entries)
            [rest entries] (split-with #(not= (first %) :begins-shift) entries)
            guard (into guard rest)]
        (recur entries (conj output guard)))
      output)))

(defn entries->shift [entries]
  "Entries refer to log entries for one shift"
  (let [shift {:guard-id (last (first entries))
               :sleep-minutes #{}
               :last-fell-asleep nil}
        shift (reduce (fn [shift entry]
                        (let [[event year month day hour minute] entry]
                          (condp = event
                            :falls-asleep
                            (-> shift (assoc :last-fell-asleep minute))
                            :wakes-up
                            (update shift
                                    :sleep-minutes
                                    clojure.set/union
                                    (shift :sleep-minutes)
                                    (set (range (shift :last-fell-asleep) minute))))))
                      shift
                      (next entries))]
    (dissoc shift :last-fell-asleep)))

(defn get-total-shift-sleep-time [shift]
  (-> shift :sleep-minutes count))

(defn calc-longest-sleeping-guard [shifts]
  (->> shifts
       (reduce (fn [times shift]
                 (update times (shift :guard-id) (fnil + 0) (get-total-shift-sleep-time shift)))
               {})
       (apply max-key val)
       first))

(defn calc-most-slept-minute [shifts]
  (->> shifts
       (reduce (fn [counts shift]
                 (reduce (fn [counts minute]
                           (update counts minute (fnil inc 0)))
                         counts
                         (shift :sleep-minutes)))
               {})
       (apply max-key val)
       first))

(def part1-solution
  (let [shifts (->> input
                    partition-entries-by-shift
                    (map entries->shift))
        guard (calc-longest-sleeping-guard shifts)
        minute (calc-most-slept-minute (filter #(= (% :guard-id) guard) shifts))]
    (* guard minute)))
(assert (= 8421 part1-solution))

(defn count-minutes-asleep-per-guard [shifts]
  (->> shifts
       (reduce (fn [counts shift]
                 (reduce (fn [counts minute]
                           (update-in counts [(shift :guard-id) minute] (fnil inc 0)))
                         counts
                         (shift :sleep-minutes)))
               {})))

(def part2-solution
  (let [shifts (->> input
                    partition-entries-by-shift
                    (map entries->shift))
        minute-counts (count-minutes-asleep-per-guard shifts)
        [guard [minute _]] (apply max-key
                                  (comp second second)
                                  (map #(vector (first %) (apply max-key val (second %))) minute-counts))]
    (* guard minute)))
(assert (= 83359 part2-solution))
