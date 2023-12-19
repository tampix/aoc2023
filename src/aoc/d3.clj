(ns aoc.d3
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(defn re-seq-match
  "Like `re-seq`, but groups are represented as matches with keys `:start`, `:end`
  and `:group`."
  [re s]
  (let [m          (re-matcher re s)
        group-full (fn [i]
                     {:start (.start m i)
                      :end   (.end m i)
                      :group (.group m i)})]
    (loop [res ()]
      (if (.find m)
        (let [gc     (.groupCount m)
              groups (if (zero? gc)
                           (group-full 0)
                           (mapv group-full (-> gc inc range)))]
          (recur (cons groups (lazy-seq res))))
        res))))

(defn parse-grid
  [grid re value-fn]
  (into {}
        (for [row-idx                   (range (count grid))
              {:keys [start end group]} (re-seq-match re (grid row-idx))
              col-idx                   (range start end)
              :let [point [row-idx col-idx]]]
          [point (value-fn [point group])])))

(defn point->neighbors
  [[y x]]
  (for [off-y [-1 0 1]
        off-x [-1 0 1]
        :when (not (and (zero? off-y)
                        (zero? off-x)))]
    [(+ off-y y) (+ off-x x)]))

(defn day3
  []
  (let [grid  (util/read-input-lines 3)
        nums  (parse-grid grid
                          #"\d+"
                          (comp parse-long second))
        syms  (parse-grid grid
                          #"[^\d.]"
                          (fn [[point sym]]
                            {:sym sym
                             :adjacent-parts (into #{}
                                                   (keep nums)
                                                   (point->neighbors point))}))]
    ;; part 1
    (->> (vals syms)
         (transduce (mapcat :adjacent-parts)
                    +)
         println)
    ;; part 2
    (->> (vals syms)
         (transduce (comp (filter (comp #{"*"} :sym))
                          (map :adjacent-parts)
                          (filter (comp #{2} count))
                          (map (partial reduce *)))
                    +)
         println)))
