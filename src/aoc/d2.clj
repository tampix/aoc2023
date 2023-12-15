(ns aoc.d2
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def population {"red"   12
                 "green" 13
                 "blue"  14})

(defn hand->bag
  [hand]
  (->> (str/split hand #",? ")
       (partition 2)
       (map (juxt second
                  (comp parse-long first)))
       (into {"red"   0
              "green" 0
              "blue"  0})))

(defn line->game
  [line]
  (let [hands (-> line
                  (str/replace #"Game \d+: " "")
                  (str/split #"; "))]
    (map hand->bag hands)))

(defn possible-hand?
  [hand]
  (every? (fn [[color num]]
            (<= num (get population color)))
          hand))

(defn possible-game?
  [game]
  (every? possible-hand? game))

(defn game->power
  [game]
  (letfn [(min-cubes [acc el]
            (-> acc
                (update "red" max (get el "red"))
                (update "green" max (get el "green"))
                (update "blue" max (get el "blue"))))]
    (->> game
         (reduce min-cubes)
         vals
         (reduce *))))

(defn day2
  []
  (let [games (->> (util/read-input-lines 2)
                   (mapv line->game))]
    ;; part1
    (->> games
         (reduce-kv (fn [sum id game]
                      (if (possible-game? game)
                        (+ sum id 1)
                        sum))
                    0)
         println)
    ;; part2
    (->> games
         (transduce (map game->power)
                    +)
         println)))
