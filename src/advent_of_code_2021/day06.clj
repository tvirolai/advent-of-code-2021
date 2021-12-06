(ns advent-of-code-2021.day06
  (:require [clojure.string :as s]))

(def data
  (mapv read-string
        (s/split (slurp "./inputs/day06.txt") #",")))

(def days 80)

(defn next-state [state]
  (loop [i 0
         acc state
         new-borns 0]
    (if (= i (count state))
      (into acc (vec (repeat new-borns 8)))
      (let [curr (nth acc i)]
        (recur (inc i)
               (if (zero? curr)
                 (assoc acc i 6)
                 (update acc i dec))
               (if (zero? curr)
                 (inc new-borns)
                 new-borns))))))

(defn part-01 []
  (->> data
       (iterate next-state)
       (take (inc days))
       last
       count))

(def data-p2
  (reduce (fn [acc val]
            (update acc val inc))
          (vec (repeat 9 0))
          data))

;; Modeling the state as a vector
(defn next-state* [state]
  (let [zeros (first state)
        iteration (vec (rest (take 9 (cycle state))))]
    (update (conj iteration zeros) 6 + zeros)))

(def days-p2 256)

(defn part-02 []
  (->> data-p2
       (iterate next-state*)
       (take (inc days-p2))
       last
       (reduce +)))
