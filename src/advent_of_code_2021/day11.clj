(ns advent-of-code-2021.day11
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(defrecord Coord [x y])

(def filename "./inputs/day11.txt")

(def data
  (let [lines (s/split (slurp filename) #"\n")]
    (vec (for [l lines]
           (mapv #(Integer/parseInt %) (s/split l #""))))))

(defn point-in-grid? [grid {:keys [x y]}]
  (let [y-dim (count grid)
        x-dim (count (first grid))]
    (and (>= (dec x-dim) x 0)
         (>= (dec y-dim) y 0))))

(defn get-neighbor-coords [coord]
  (let [{:keys [x y]} coord]
    (for [xi (range (dec x) (+ 2 x))
          yi (range (dec y) (+ 2 y))
          :when (and (point-in-grid? data (->Coord xi yi))
                     (not (and (= xi x)
                               (= yi y))))]
      (->Coord xi yi))))

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(defn increment-all [grid]
  (vec
   (for [row grid]
     (mapv inc row))))

(defn increment-on [grid {:keys [x y]}]
  (update-in grid [y x] inc))

(defn increment-on-coords [grid coords]
  (if (empty? coords)
    grid
    (recur (increment-on grid (first coords))
           (rest coords))))

(defn get-flashing [grid]
  (flatten
   (for [y (range 0 (count grid))]
     (for [x (range 0 (count (first grid)))
           :let [value (get-cell grid (->Coord x y))]
           :when (> value 9)]
       (->Coord x y)))))

(defn increment-neighbors [grid flashing]
  (let [neighbors (mapcat get-neighbor-coords flashing)]
    (increment-on-coords grid neighbors)))

(defn zero-flashed [grid]
  (vec (for [row grid]
         (mapv (fn [val]
                 (if (> val 9)
                   0
                   val)) row))))

(defn zero-count [grid]
  (count (filter zero? (flatten grid))))

(defn- zeros-in-string [s]
  (count (filter #(= \0 %) (seq s))))

;; This takes and returns a map that looks like:
;; {:grid [[0 1 2...]]
;;  :flashes 4}
(defn next-state [{:keys [grid flashes]}]
  (let [incremented-grid (increment-all grid)]
    (loop [curr-grid incremented-grid
           all-flashes #{}]
      (let [curr-flashing (cset/difference
                           (set (get-flashing curr-grid))
                           all-flashes)]
        (if (zero? (count curr-flashing))
          (let [this-round-grid (zero-flashed curr-grid)
                this-round-flashes (zero-count this-round-grid)]
            {:grid this-round-grid
             :curr-flashes this-round-flashes
             :flashes (+ flashes this-round-flashes)})
          (recur
           (increment-neighbors curr-grid curr-flashing)
           (cset/union curr-flashing all-flashes)))))))

(defn part-01 []
  (->> {:grid data
        :flashes 0}
       (iterate next-state)
       (take 101)
       last
       :flashes))

(defn all-flash? [{:keys [grid]}]
  (every? zero? (flatten grid)))

(defn part-02 []
  (count
   (take-while
    (complement all-flash?)
    (iterate next-state {:grid data
                         :flashes 0}))))
