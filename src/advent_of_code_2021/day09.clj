(ns advent-of-code-2021.day09
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(def filename "./inputs/day09.txt")

(def data
  (let [lines (s/split (slurp filename) #"\n")]
    (vec (for [l lines]
           (mapv #(Integer/parseInt %) (s/split l #""))))))

(defrecord Coord [x y])

(defn get-neighbor-coords [coord]
  (let [{:keys [x y]} coord]
    (for [xi (range (dec x) (+ 2 x))
          yi (range (dec y) (+ 2 y))
          :when (and (>= xi 0)
                     (>= yi 0)
                     (or (= xi x)
                         (= yi y))
                     (not (and (= x xi)
                               (= y yi))))]
      (->Coord xi yi))))

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(defn point-in-grid? [grid {:keys [x y]}]
  (let [y-dim (count grid)
        x-dim (count (first grid))]
    (and (>= (dec x-dim) x 0)
         (>= (dec y-dim) y 0))))

(defn low-point? [grid point]
  (let [neighbors (->> point
                       get-neighbor-coords
                       (filter (partial point-in-grid? data))
                       (map (partial get-cell data)))
        curr-point (get-cell data point)]
    (< curr-point (apply min neighbors))))

(defn get-low-point-coords [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (first grid)))
        :let [c (->Coord x y)]
        :when (low-point? grid c)]
    (->Coord x y)))

(defn part-01 []
  (->> (get-low-point-coords data)
       (map (partial get-cell data))
       (map inc)
       (reduce +)))

(defn get-non-nine-neighbors [grid point]
  (when (point-in-grid? grid point)
    (let [neighbors-coords (->> point
                                get-neighbor-coords
                                (filter (partial point-in-grid? grid)))]
      (filter (fn [c]
                (< (get-cell grid c) 9)) neighbors-coords))))

(defn get-basin [grid low-point]
  (loop [prev-points nil
         curr-points (set (get-non-nine-neighbors grid low-point))]
    (if (and (not (nil? prev-points))
             (= curr-points prev-points))
      curr-points
      (let [new-neighbors (->> curr-points
                               (map (partial get-non-nine-neighbors data))
                               (filter (complement nil?))
                               flatten
                               set)]
        (recur curr-points
               (cset/union new-neighbors curr-points))))))

(defn part-02 []
  (let [low-points (get-low-point-coords data)
        basins (map (partial get-basin data) low-points)]
    (->> basins
         (map count)
         sort
         (take-last 3)
         (apply *))))
