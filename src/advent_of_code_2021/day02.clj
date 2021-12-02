(ns advent-of-code-2021.day02
  (:require [advent-of-code-2021.core :refer [read-input]]
            [clojure.string :as s]))

(defrecord Coords [x y])

(defn part-01 []
  (let [data (read-input "day02.txt")
        start-point (->Coords 0 0)
        end-coords (reduce (fn [acc v]
                             (let [[dir n] (s/split v #" ")
                                   amount (Integer/parseInt n)]
                               (case dir
                                 "forward" (update acc :x + amount)
                                 "up" (update acc :y - amount)
                                 "down" (update acc :y + amount))))
                           start-point data)]
    (->> end-coords
         vals
         (apply *))))

(defrecord Coords-2 [x y aim])

(defn part-02 []
  (let [data (read-input "day02.txt")
        start-point (->Coords-2 0 0 0)
        end-coords (reduce (fn [{:keys [aim] :as acc} v]
                             (let [[dir n] (s/split v #" ")
                                   amount (Integer/parseInt n)]
                               (case dir
                                 "forward" (-> acc
                                               (update :x + amount)
                                               (update :y + (* amount aim)))
                                 "up" (update acc :aim - amount)
                                 "down" (update acc :aim + amount))))
                           start-point data)]
    (->> (dissoc end-coords :aim)
         vals
         (apply *))))
