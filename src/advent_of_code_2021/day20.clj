(ns advent-of-code-2021.day20
  (:require [clojure.string :as s]))

(def filename "./inputs/day20_example.txt")

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

(defn get-pixel-form [image {:keys [x y] :as coord}]
  (for [y (range (dec y) (+ 2 y))
        x (range (dec x) (+ 2 x))]
    (get-pixel image (->Coord x y))))

(defn pixel-form->int [pixel-form]
  (let [binary (s/join (map #(if (= "." %) 0 1) pixel-form))]
    (Integer/parseInt binary 2)))

(defrecord Padding-info [top right bottom left])

(defn needs-padding? [image]
  (let [image-width (count (first image))
        top (take 2 image)
        bottom (take-last 2 image)
        left-margin (vec (for [y (range 0 (count image))]
                           (for [x '(0 1)]
                             (get-pixel image (->Coord x y)))))
        right-margin (vec (for [y (range 0 (count image))]
                            (for [x (list (- image-width 2) (dec image-width))]
                              (get-pixel image (->Coord x y)))))
        empty-predicate (fn [input]
                          (pos? (get (frequencies (flatten input)) "#" 0)))]
    (->Padding-info (empty-predicate top)
                    (empty-predicate right-margin)
                    (empty-predicate bottom)
                    (empty-predicate left-margin))))

#_(defn add-padding* [image]
  (let [line-len (count (first image))
        empty-line (vec (repeat (+ (* 2 3) line-len) "."))
        widened-image (vec (for [line image
                                 :let [padding (vec (repeat 3 "."))]]
                             (into (into padding line) padding)))
        up-and-down-padding (vec (repeat 3 empty-line))]
    (into (into up-and-down-padding widened-image)
          up-and-down-padding)))

(defn add-padding* [image]
  (let [line-len (count (first image))
        empty-line (vec (repeat (+ 2 line-len) "."))
        widened-image (vec (for [line image
                                 :let [padding (vec (repeat 1 "."))]]
                             (into (into padding line) padding)))
        up-and-down-padding (vec (repeat 1 empty-line))]
    (into (into up-and-down-padding widened-image)
          up-and-down-padding)))

(defn add-padding [image]
  (let [{:keys [top right bottom left]} (needs-padding? image)
        line-len (count (first image))
        new-line-len (cond
                       (and left right) (+ 4 line-len)
                       (or left right) (+ 2 line-len)
                       :else line-len)
        empty-line (vec (repeat new-line-len "."))
        empty-lines (vec (repeat 2 (vec (repeat new-line-len "."))))
        widened-image (vec (for [line image
                                 :let [padding ["." "."]]]
                             (cond
                               (and left right) (into (into padding line) padding)
                               left (into padding line)
                               right (into line padding))))]
    (cond
      (and top bottom) (into (into [empty-line] widened-image)
                             [empty-line])
      top (into [empty-line] widened-image)
      bottom (into image empty-line)
      :else image)))

(defn in-operation-range? [image {:keys [x y]}]
  (let [y-dim (count image)
        x-dim (count (first image))]
    (and (< 0 x (dec x-dim))
         (< 0 y (dec y-dim)))))

(defn- print-image [image]
  (doseq [line (map s/join image)]
    (println line)))

(defn enhance [algorithm image]
  (let [padded-image (add-padding image)
        y-dim (count padded-image)
        x-dim (count (first padded-image))]
    (vec
     (for [y (range 0 y-dim)]
       (vec
        (for [x (range 0 x-dim)
              :let [coord (->Coord x y)]]
          (if-not (in-operation-range? padded-image coord)
            (get-pixel padded-image coord)
            (let [pixel-form (get-pixel-form padded-image coord)]
              (str (nth algorithm (pixel-form->int pixel-form)))))))))))

(defn get-pixel-val [algorithm image coord]
  (let [pixel-form (get-pixel-form image coord)
        numb (pixel-form->int pixel-form)]
    (println numb)
    (str (nth algorithm numb))))

(defn part-01* []
  (let [{:keys [image algorithm]} data
        images (iterate (partial enhance algorithm) image)
        counts (frequencies (flatten (last (take 3 images))))
        ]
    ;; (enhance algorithm image)
    {:images (take 3 images)
     :count-2 (frequencies (flatten (last (take 2 images))))
     :counts counts}))

(defn part-01 []
  (let [{:keys [image algorithm]} data]
    (loop [img image
           i 0]
      (println "")
      (print-image (if (zero? i)
                     (add-padding img)
                     img))
      (if (= i 2)
        (frequencies (flatten img))
        (recur (enhance algorithm img)
               (inc i))))))
