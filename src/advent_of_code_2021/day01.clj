(ns advent-of-code-2021.day01
  (:require [clojure.string :as s]))

(defn read-input []
  (map read-string
       (s/split (slurp "./inputs/day01.txt") #"\n")))

(defn part-01 []
  (let [data (partition 2 1 (read-input))]
    (count (filter (partial apply <) data))))

(defn part-02 []
  (let [data (partition 3 1 (read-input))]
    (->> data
         (map (partial reduce +))
         (partition 2 1)
         (filter (partial apply <))
         count)))
