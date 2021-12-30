(ns advent-of-code-2021.day20
  (:require [clojure.string :as s]))

(def filename "./inputs/day20.txt")

(def data
  (let [lines (s/split (slurp filename) #"\n")
        algorithm (s/join (take-while (complement empty?) lines))
        image (vec (for [line (last (partition-by empty? lines))]
                     (vec (s/split line #""))))]
    {:image image
     :algorithm algorithm}))

(defrecord Coord [x y])

(defn get-pixel [image {:keys [x y]}]
  (nth (nth image y) x))

(defn get-pixel-form [image {:keys [x y]}]
  (for [y (range (dec y) (+ 2 y))
        x (range (dec x) (+ 2 x))]
    (get-pixel image (->Coord x y))))

(defn pixel-form->int [pixel-form]
  (let [binary (s/join (map #(if (= "." %) 0 1) pixel-form))]
    (Integer/parseInt binary 2)))

(defn add-large-padding [image]
  (let [padding-amount 100
        padding-symbol "."
        line-len (count (first image))
        empty-line (vec (repeat (+ (* 2 padding-amount) line-len) padding-symbol))
        widened-image (vec (for [line image
                                 :let [padding (vec (repeat padding-amount padding-symbol))]]
                             (into (into padding line) padding)))
        up-and-down-padding (vec (repeat padding-amount empty-line))]
    (into (into up-and-down-padding widened-image)
          up-and-down-padding)))

(defn in-operation-range? [image {:keys [x y]}]
  (let [y-dim (count image)
        x-dim (count (first image))]
    (and (< 0 x (dec x-dim))
         (< 0 y (dec y-dim)))))

(defn invert-pixel [pixel]
  (if (= "." pixel)
    "#"
    "."))

(defn enhance [algorithm image]
  (let [y-dim (count image)
        x-dim (count (first image))]
    (vec
     (for [y (range 0 y-dim)]
       (vec
        (for [x (range 0 x-dim)
              :let [coord (->Coord x y)]]
          (if-not (in-operation-range? image coord)
            (invert-pixel (get-pixel image coord))
            (let [pixel-form (get-pixel-form image coord)]
              (str (nth algorithm (pixel-form->int pixel-form)))))))))))

(defn solve [rounds]
  (let [{:keys [image algorithm]} data]
    (loop [img (add-large-padding image)
           i 0]
      (if (= i rounds)
        (get (frequencies (flatten img)) "#")
        (recur (enhance algorithm img)
               (inc i))))))

(defn part-01 []
  (solve 2))

(defn part-02 []
  (solve 50))
