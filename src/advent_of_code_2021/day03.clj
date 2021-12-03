(ns advent-of-code-2021.day03
  (:require [advent-of-code-2021.core :refer [read-input]]
            [clojure.set :as se]
            [clojure.string :as s]))

(def inputfile "day03.txt")

(defn gamma-rate
  ([input] (gamma-rate input ""))
  ([input res]
   (if (empty? (first input))
     res
     (let [f (map first input)
           bit (->> f
                    frequencies
                    (apply max-key val)
                    first)]
       (gamma-rate (map rest input)
                   (str res bit))))))

(defn epsilon-rate
  "Input as a binary string"
  [input]
  (let [complement (Integer/toBinaryString (bit-not (Integer/parseInt input 2)))]
    (s/join (take-last (count input) complement))))

(defn part-01 []
  (let [data (map #(s/split % #"")
                  (read-input inputfile))
        gamma (gamma-rate data)
        epsilon (epsilon-rate gamma)]
    (* (Integer/parseInt gamma 2)
       (Integer/parseInt epsilon 2))))

(defn get-counts [input index]
  (->> input
       (map #(nth % index))
       frequencies))


(defn get-favored-bit-oxygen [counts]
  (let [zero-count (get counts \0 0)
        one-count (get counts \1 0)]
    (if (> zero-count one-count)
      0
      1)))

(defn get-favored-bit-co2 [counts]
  (let [zero-count (get counts \0 0)
        one-count (get counts \1 0)]
    (if (< one-count zero-count)
      1
      0)))

(defn oxygen-generator
  ([input] (oxygen-generator input 0))
  ([input index]
   (if (or (= index (count (first input)))
           (= 1 (count input)))
     (first input)
     (let [counts (get-counts input index)
           bit (get-favored-bit-oxygen counts)]
       (oxygen-generator (filter (fn [item]
                                   (= bit (Character/digit (nth item index) 10)))
                                 input)
                         (inc index))))))

(defn co2-generator
  ([input] (co2-generator input 0))
  ([input index]
   (if (or (= index (count (first input)))
           (= 1 (count input)))
     (first input)
     (let [counts (get-counts input index)
           bit (get-favored-bit-co2 counts)]
       (co2-generator (filter (fn [item]
                                   (= bit (Character/digit (nth item index) 10)))
                                 input)
                         (inc index))))))

(defn part-02 []
  (let [data (read-input inputfile)]
    (* (Integer/parseInt (oxygen-generator data) 2)
       (Integer/parseInt (co2-generator data) 2))))
