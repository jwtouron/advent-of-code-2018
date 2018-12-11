(ns advent-of-code-2018.day9)

(def input [486 70833])

(def example1 [9 25]) ; 32
(def example2 [10 1618]) ; 8317
(def example3 [13 7999]) ; 146373
(def example4 [17 1104]) ; 2764
(def example5 [21 6111]) ; 54718

(def new-ring {:current 0 0 {:cw 0 :ccw 0}})
(def new-ring+1 {:current 1 0 {:cw 1 :ccw 1} 1 {:cw 0 :ccw 0}})
(def new-ring+2 {:current 2 0 {:cw 2 :ccw 1} 1 {:cw 0 :ccw 2} 2 {:cw 1 :ccw 0}})

(defn print-ring [ring]
  (loop [current 0
         nums [0]]
    (if (= (get-in ring [current :cw]) 0)
      (apply str (interpose " " (map #(if (= % (ring :current)) (str "(" % ")") (str %)) nums)))
      (let [current (get-in ring [current :cw])
            nums (conj nums current)]
        (recur current nums)))))

(defn move-cw [ring]
  (assoc ring :current (get-in ring [(ring :current) :cw])))

(defn move-ccw [ring]
  (assoc ring :current (get-in ring [(ring :current) :ccw])))

(defn place-cw [marble ring]
  (let [current (ring :current)
        cw1 (get-in ring [(ring :current) :cw])]
    (-> ring
        (assoc-in [current :cw] marble)
        (assoc-in [cw1 :ccw] marble)
        (assoc-in [marble :cw] cw1)
        (assoc-in [marble :ccw] current))))

(defn delete-current [ring]
  (let [current (ring :current)
        ccw (get-in ring [current :ccw])
        cw  (get-in ring [current :cw])]
    (-> ring
        (assoc-in [ccw :cw] cw)
        (assoc-in [cw :ccw] ccw)
        (assoc :current cw)
        (dissoc current))))

(defn new-game [num-players]
  {:next-player 1
   :num-players num-players
   :scores (zipmap (range 1 (inc num-players)) (repeat 0))
   :ring new-ring})

#_
(let [marbles (range 1 26)]
  (->> (new-game 9) (step-game-many marbles) println)
  (->> (new-game 9) (step-game-many marbles) :ring print-ring))

(defn step-game [marble {:keys [next-player num-players scores ring] :as game}]
  (let [next-next-player (rem (inc next-player) num-players)]
    (if (zero? (rem marble 23))
      (let [points marble
            ring   (first (drop 7 (iterate move-ccw ring)))
            points (+ points (ring :current))
            ring   (delete-current ring)]
        (-> game
            (assoc :next-player next-next-player)
            (assoc :ring ring)
            (update-in [:scores next-player] (fnil + 0) points)))
      (let [ring (cond
                   (= ring new-ring)   new-ring+1
                   (= ring new-ring+1) new-ring+2
                   :else               (move-cw (place-cw marble (move-cw ring))))]
        (-> game
            (assoc :next-player next-next-player)
            (assoc :ring ring))))))

(defn step-game-many [marbles game]
  (loop [marbles marbles
         game game]
    (if (seq marbles)
      (recur (next marbles) (step-game (first marbles) game))
      game)))

(defn solve [num-players last-marble]
  (let [game (step-game-many (range 1 (inc last-marble)) (new-game num-players))]
    (-> game
        :scores
        (as-> xs (map #(nth % 1) xs))
        (as-> xs (apply max xs)))))

(assert (= (apply solve example1) 32))
(assert (= (apply solve example2) 8317))
(assert (= (apply solve example3) 146373))
(assert (= (apply solve example4) 2764))
(assert (= (apply solve example5) 54718))

(assert (= (apply solve input) 373597))

;; part 2
;; (println (solve (nth input 0) (* (nth input 1) 100))) ; => 2954067253
