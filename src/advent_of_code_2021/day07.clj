(ns advent-of-code-2021.day07
  (:require [clojure.string :as s]
            [kixi.stats.core :as stats]))

(def file "./inputs/day07.txt")

(def data
  (map read-string (s/split (slurp file) #",")))

(defn part-01 []
  (let [median (int (transduce identity stats/median data))]
    (reduce (fn [acc val]
              (+ acc (Math/abs (- val median))))
            0
            data)))

(defn part-02 []
  (let [mean-val (int (transduce identity stats/mean data))]
    (reduce (fn [acc val]
              (let [distance (Math/abs (- val mean-val))]
                (+ acc (reduce + (range 1 (inc distance))))))
            0
            data)))
