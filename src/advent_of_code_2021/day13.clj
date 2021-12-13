(ns advent-of-code-2021.day13
  (:require [clojure.string :as s]))

(def filename "./inputs/day13.txt")

(defrecord Coord [x y])

(defrecord Fold [axis number])

(def data
  (let [[coords _ folds] (partition-by empty?
                                       (s/split (slurp filename) #"\n"))
        points (for [c coords
                     :let [[x y] (map read-string (s/split c #","))]]
                 (->Coord x y))
        instr (for [f folds
                    :let [fold (last (s/split f #" "))
                          [axis number] (s/split fold #"=")]]
                (->Fold (keyword axis) (read-string number)))]
    {:points points
     :folds instr}))

(defn convert-point [{:keys [axis number] :as fold} point]
  (let [fold-axis-value (axis point)]
    (if (> fold-axis-value number)
      (update point axis - (* 2 (- fold-axis-value number)))
      point)))

(defn apply-fold [fold coords]
  (map (partial convert-point fold) coords))

(defn part-01 []
  (let [{:keys [points folds]} data]
    (count (set (apply-fold (first folds) points)))))

(defn solve-coords []
  (let [{:keys [points folds]} data]
    (loop [p points
           f folds]
      (if (empty? f)
        p
        (recur (apply-fold (first f) p)
               (rest f))))))

(defn mark-spot [grid {:keys [x y]}]
  (assoc-in grid [y x] "#"))

(defn grid->string [grid]
  (s/join "\n" (for [line grid]
                 (s/join line))))

(defn part-02 []
  (let [coords (solve-coords)
        x-size (apply max (map :x coords))
        y-size (apply max (map :y coords))
        grid (vec (for [row (range 0 (inc x-size))]
                    (vec (repeat (inc x-size) "."))))]
    (loop [remaining-coords coords
           g grid]
      (if (empty? remaining-coords)
        (grid->string g)
        (recur (rest remaining-coords)
               (mark-spot g (first remaining-coords)))))))
