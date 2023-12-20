(ns aoc.d4
  (:require [clojure.math :as math]
            [aoc.util :as util]))

(defn day4
  []
  (let [cards (into []
                    (comp (map parse-long)
                          (partition-by nil?)
                          (partition-all 4)
                          (map (fn [[_ winning _ numbers]]
                                 (count (filter (set winning)
                                                numbers)))))
                    (re-seq #"\S+" (util/read-input 4)))]
    ;; part 1
    (->> cards
         (transduce (map #(long (math/pow 2 (dec %))))
                    +)
         println)
    ;; part 2
    (->> cards
         (reduce-kv (fn [bag k v]
                      (reduce #(update %1 %2 + (bag k))
                              bag
                              (range (inc k) (+ k v 1))))
                    (vec (repeat (count cards) 1)))
         (reduce +)
         println)))
