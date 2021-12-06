(ns advent-of-code-2021.day05
  (:require [advent-of-code-2021.core :refer [read-input]]
            [clojure.string :as s]))

(def filename "day05.txt")

(defrecord Coord [x y])

(defrecord Instruction [coord1 coord2])

(defn line->instruction [line]
  (let [[c1 c2] (s/split line #" -> ")
        [x1 y1] (s/split c1 #",")
        [x2 y2] (s/split c2 #",")]
    (->Instruction
     (->Coord (read-string x1)
              (read-string y1))
     (->Coord (read-string x2)
              (read-string y2)))))

(defn parse-input []
  (let [data (read-input filename)]
    (map line->instruction data)))

(defn horizontal-or-vertical? [instruction]
  (let [{:keys [coord1 coord2]} instruction]
    (or (= (:x coord1) (:x coord2))
        (= (:y coord1) (:y coord2)))))

(defn instr->points [{:keys [coord1 coord2] :as instr}]
  (let [x1 (:x coord1)
        y1 (:y coord1)
        x2 (:x coord2)
        y2 (:y coord2)]
    (let [diagonal? (not (horizontal-or-vertical? instr))]
      (if-not diagonal?
        (for [x (range (min x1 x2) (inc (max x1 x2)))
              y (range (min y1 y2) (inc (max y1 y2)))]
          (->Coord x y))
        (let [xrange (range (min x1 x2) (inc (max x1 x2)))
              yrange (range (min y1 y2) (inc (max y1 y2)))
              dir-xrange (if (> x1 x2)
                           xrange
                           (reverse xrange))
              dir-yrange (if (> y1 y2)
                           yrange
                           (reverse yrange))]
          (map (fn [x y]
                 (->Coord x y))
               dir-xrange dir-yrange))))))

(defn part-01 []
  (let [instr (filter horizontal-or-vertical? (parse-input))]
    (->> instr
         (mapcat instr->points)
         frequencies
         vals
         (filter (partial < 1))
         count)))

(defn part-02 []
  (let [instr (parse-input)]
    (->> instr
         (mapcat instr->points)
         frequencies
         vals
         (filter (partial < 1))
         count)))
