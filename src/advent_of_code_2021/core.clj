(ns advent-of-code-2021.core
  (:require [clojure.string :as s]))

(defn read-input [filename]
  (s/split (slurp (str "./inputs/" filename)) #"\n"))

(defn read-input-grid [filename]
  (let [rows (s/split (slurp filename) #"\n")]
    (vec (for [row rows]
           (->> (seq row)
                (map str)
                (mapv #(Integer/parseInt % 10)))))))

(defrecord Coord [x y])

(defn- get-neighbor-coords [coord]
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

(defn- point-in-grid? [grid {:keys [x y]}]
  (let [y-dim (count grid)
        x-dim (count (first grid))]
    (and (>= (dec x-dim) x 0)
         (>= (dec y-dim) y 0))))

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(defn get-neighbor-points [grid coord]
  (filter (partial point-in-grid? grid)
          (get-neighbor-coords coord)))
