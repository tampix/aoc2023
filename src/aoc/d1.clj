(ns aoc.d1
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(defn solve
  [in]
  (transduce (comp (map (juxt first last))
                   (map (partial apply str))
                   (map parse-long))
             +
             (-> in
                 (str/replace #"[^\d\n]" "")
                 str/split-lines)))

(defn translate
  [s]
  (reduce-kv str/replace
             s
             {"one"   "o1e"
              "two"   "t2o"
              "three" "t3e"
              "four"  "f4r"
              "five"  "f5e"
              "six"   "s6x"
              "seven" "s7n"
              "eight" "e8t"
              "nine"  "n9e"}))

(defn day1
  []
  (let [in (util/read-input 1)]
    ;; part1
    (->> in solve println)
    ;; part2
    (->> in translate solve println)))
