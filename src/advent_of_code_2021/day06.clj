(ns advent-of-code-2021.day06
  (:require [clojure.string :as s]))

(def data
  (reduce (fn [acc val]
            (update acc val inc))
          (vec (repeat 9 0))
          (->> #","
               (s/split (slurp "./inputs/day06.txt"))
               (mapv read-string))))

;; Modeling the state as a vector
(defn next-state [state]
  (let [zeros (first state)
        iteration (vec (rest (take 9 (cycle state))))]
    (update (conj iteration zeros) 6 + zeros)))

(defn solve [days]
  (->> data
       (iterate next-state)
       (take (inc days))
       last
       (reduce +)))

(defn part-01 []
  (solve 80))

(defn part-02 []
  (solve 256))
