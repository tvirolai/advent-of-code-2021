(ns advent-of-code-2021.day21
  (:require [clojure.math.combinatorics :as combo]))

(defn next-position [pos dice-scores]
  (let [score (+ pos (reduce + dice-scores))
        last-digit (Character/digit (last (str score)) 10)]
    (cond
      (>= 10 score) score
      (zero? last-digit) 10
      :default last-digit)))

(defn scores [a b]
  (loop [dice-scores (partition 3 (cycle (range 1 (inc 10))))
         space-a a
         score-a 0
         space-b b
         score-b 0
         rolls 0]
    (if (>= (max score-a score-b) 1000)
      {:a score-a
       :b score-b
       :rolls rolls}
      (if (zero? (mod rolls 2))
        (let [pos (next-position space-a (first dice-scores))]
          (recur (rest dice-scores)
                 pos
                 (+ score-a pos)
                 space-b
                 score-b
                 (+ rolls 3)))
        (let [pos (next-position space-b (first dice-scores))]
          (recur (rest dice-scores)
                 space-a
                 score-a
                 pos
                 (+ score-b pos)
                 (+ rolls 3)))))))

(defn part-01 []
  (let [{:keys [a b rolls]} (scores 1 3)]
    (* (min a b) rolls)))

;; Part 2

(def possible-outcomes
  (combo/selections '(1 2 3) 3))

(def combo-sums
  (->> possible-outcomes
       (map (partial reduce +))
       sort
       (partition-by identity)
       (reduce (fn [acc val]
                 (conj acc (list (first val) (count val))))
               [])))

(def play-dirac
  (memoize (fn [p1-pos p2-pos p1-score p2-score]
             (cond
               (>= p1-pos 21) [1 0]
               (>= p2-pos 21) [0 1]
               :else (let [{:keys [p1-wins p2-wins]} (reduce (fn [acc [sum amount]]
                                                               (let [p1-new-pos (mod (+ sum p1-pos) 10)
                                                                     p1-new-score (inc (+ p1-score p1-new-pos))
                                                                     [other-wins player-wins] (play-dirac p2-pos p1-new-pos p2-score p1-new-score)]
                                                                 (-> acc
                                                                     (update :p1-wins + (* player-wins amount))
                                                                     (update :p2-wins + (* other-wins amount)))))
                                                             {:p1-wins 0
                                                              :p2-wins 0}
                                                             combo-sums)]
                       [p1-wins p2-wins])))))
