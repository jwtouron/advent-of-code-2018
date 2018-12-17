(ns advent-of-code-2018.day13
  (:require [advent-of-code-2018.utils :refer [get-input-lines]]))

(defn parse-line [y line]
  (loop [x 0
         line line
         track {}
         carts []]
    (if-not (seq line)
      {:track track :carts carts}
      (let [loc [x y]
            x (inc x)
            v (first line)
            v' (case (first line)
                 \v \|
                 \^ \|
                 \> \-
                 \< \-
                 (first line))
            line (next line)
            track (assoc track loc v')
            carts (if (#{\v \^ \> \<} v)
                    (conj carts {:dir v :loc loc :next-turn :left})
                    carts)]
        (recur x line track carts)))))

(defn make-carts+track [lines]
  (->> lines
       (map parse-line (range))
       (apply merge-with (fn [x y]
                           (if (map? x)
                             (merge x y)
                             (into x y))))))

(def input
  (->> "day13.txt"
       get-input-lines
       make-carts+track))

(def example
  (->> "/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   "
       clojure.string/split-lines
       make-carts+track))

(defn collisions [carts]
  (let [cs (reduce (fn [collisions {:keys [loc] :as cart}]
                     (update collisions loc (fnil conj []) cart))
                   {}
                   carts)
        cs (->> cs
                (filter #(> (count (nth % 1)) 1))
                (into {}))]
    (when (seq cs)
      cs)))

(defn cart-at [loc carts]
  (first (filter #(= (% :loc) loc) carts)))

(defn print-carts+track [c+t]
  (let [track (c+t :track)
        carts (c+t :carts)
        xmax (apply max (map #(get-in % [0 0]) track))
        ymax (apply max (map #(get-in % [0 1]) track))
        line (for [y (range (inc ymax))
                   x (range (inc xmax))
                   :let [loc [x y]
                         cs (collisions carts)
                         cart (cart-at loc carts)
                         sym (cond
                               (get cs loc) \X
                               cart (cart :dir)
                               :else (track loc))]]
               sym)]
    (doseq [x (partition (inc xmax) line)]
      (println (apply str x)))))

(defn next-loc [[x y] dir]
  (case dir
    \< [(dec x) y]
    \> [(inc x) y]

    \^ [x (dec y)]
    \v [x (inc y)]))

(defn next-dir [next-track-piece current-dir next-turn]
  (case next-track-piece
    \\ (case current-dir \^ \< \> \v \v \> \< \^)
    \/ (case current-dir \< \v \^ \> \v \< \> \^)
    \+ (case [current-dir next-turn]
         [\v :left]     \>
         [\v :straight] \v
         [\v :right]    \<
         [\< :left]     \v
         [\< :straight] \<
         [\< :right]    \^
         [\> :left]     \^
         [\> :straight] \>
         [\> :right]    \v
         [\^ :left]     \<
         [\^ :straight] \^
         [\^ :right]    \>)
    current-dir))

(def next-turns {:left :straight :straight :right :right :left})

(defn update-cart [track {:keys [loc dir next-turn]}]
  (let [nloc (next-loc loc dir)
        npiece (track nloc)
        ndir (next-dir npiece dir next-turn)
        nnext-turn (if (= npiece \+) (next-turns next-turn) next-turn)
        cart {:loc nloc :dir ndir :next-turn nnext-turn}]
    cart))

(defn remove-carts-at [locs carts]
  (let [locs (set locs)]
    (reduce (fn [carts cart]
              (if-not (locs (cart :loc))
                (conj carts cart)
                carts))
            []
            carts)))

(defn tick [c+t on-crash] ;; on-crash is :stop or :remove
  (let [old-carts (sort-by #(into [] (reverse (% :loc))) (c+t :carts))
        track (c+t :track)
        new-carts []]
    (loop [old-carts old-carts
           new-carts new-carts]
      (if-not (seq old-carts)
        (assoc c+t :carts new-carts)
        (let [cart (->> old-carts first (update-cart track))
              old-carts (next old-carts)
              new-carts (conj new-carts cart)
              all-carts (concat old-carts new-carts)
              cs (collisions all-carts)
              remove-carts (partial remove-carts-at (keys cs))]
          (cond
            (and (seq cs) (= on-crash :stop))   (assoc c+t :carts all-carts)
            (and (seq cs) (= on-crash :remove)) (recur (remove-carts old-carts) (remove-carts new-carts))
            :else                               (recur old-carts new-carts)))))))

(defn tick-til-collision [c+t]
  (loop [c+t c+t]
    (if (collisions (c+t :carts))
      c+t
      (recur (tick c+t :stop)))))

(defn solve-part1 [c+t]
  (->> c+t tick-til-collision :carts collisions ffirst))
(assert (= (solve-part1 input) [5 102]))

(def example2
  (->> "/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/"
       clojure.string/split-lines
       make-carts+track))

(defn solve-part2 [c+t]
  (loop [c+t c+t]
    (if (< (count (c+t :carts)) 2)
      (-> c+t :carts first :loc)
      (recur (tick c+t :remove)))))
(assert (= (solve-part2 example2) [6 4]))
(assert (= (solve-part2 input) [46 45]))
