(ns advent-of-code-2021.day08
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(def filename "./inputs/day08.txt")

(def data
  (for [line (s/split (slurp filename) #"\n")
        :let [[input output] (s/split line #" \| ")]]
    {:input (vec (s/split input #" "))
     :output (vec (s/split output #" "))}))

(def unique-counts #{2 4 3 7})

(defn part-01 []
  (let [outputs (mapcat :output data)]
    (->> outputs
         (filter #(unique-counts (count %)))
         count)))

(defn- count-is? [no string]
  (= no (count string)))

(defn is-zero? [four seven input]
  (let [as-set (set input)]
    (and (= 6 (count input))
             (not (cset/subset? (set four) as-set))
             (cset/subset? (set seven) as-set))))

(defn is-nine? [four seven input]
  (let [as-set (set input)]
    (and (= 6 (count input))
         (cset/subset? (set four) as-set)
         (cset/subset? (set seven) as-set))))

(defn is-six? [zero nine input]
  (let [as-set (set input)]
    (and (= 6 (count input))
         (not (cset/subset? (set zero) as-set))
         (not (cset/subset? (set nine) as-set)))))

(defn is-three? [seven input]
  (and (= 5 (count input))
       (cset/subset? (set seven) (set input))))

(defn is-two? [one six three input]
  (let [as-set (set input)
        different-part (cset/difference (set one) (set six))]
    (and (= 5 (count input))
         (not= three input)
         (cset/subset? different-part as-set))))

(defn is-five? [two three input]
  (and (= 5 (count input))
       (not= two input)
       (not= three input)))

(defn decode-chars [input]
  (let [one (->> input (filter (partial count-is? 2)) first)
        four (->> input (filter (partial count-is? 4)) first)
        seven (->> input (filter (partial count-is? 3)) first)
        eight (->> input (filter (partial count-is? 7)) first)
        zero (->> input (filter (partial is-zero? four seven)) first)
        nine (->> input (filter (partial is-nine? four seven)) first)
        six (->> input (filter (partial is-six? zero nine)) first)
        three (->> input (filter (partial is-three? seven)) first)
        two (->> input (filter (partial is-two? one six three)) first)
        five (->> input (filter (partial is-five? two three)) first)]
    [zero one two three four five six seven eight nine]))

(defn decode-number [decode-vector input]
  (let [input-set (set input)
        res (mapv #(= input-set (set %)) decode-vector)]
    (assert (= 1 (count (filter true? res))))
    (.indexOf res true)))

(defn part-02 []
  (let [decoded-outputs (for [line data
                              :let [{:keys [input output]} line
                                    decode-vector (decode-chars input)]]
                          (->> output
                               (map (partial decode-number decode-vector))
                               s/join))]
    (->> decoded-outputs
         (map #(Integer/parseInt %))
         (reduce +))))
