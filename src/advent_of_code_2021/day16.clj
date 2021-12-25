(ns advent-of-code-2021.day16
  (:require [clojure.string :as s]))

(defn hex->binary [hex-char]
  (condp = hex-char
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \A "1010"
    \B "1011"
    \C "1100"
    \D "1101"
    \E "1110"
    \F "1111"))

(def ex1 "D2FE28")

(defrecord Packet [version id literal-value subpackets])

(defn get-version [packet]
  (let [v (s/join (take 3 packet))]
    (Integer/parseInt v 2)))

(defn get-id [packet]
  (let [t (s/join (take 3 (drop 3 packet)))]
    (Integer/parseInt t 2)))

(defn get-literal-value [packet]
  (let [sections (->> packet
                      (drop 6)
                      s/join
                      (partition 5)
                      (partition-by (fn [[head & _]]
                                      (= \0 head)))
                      vec)
        bits-to-read (conj (vec (first sections)) (first (last sections)))
        res-bits (s/join (reduce (fn [acc val]
                                   (let [[head & tail] val]
                                     (into acc tail)))
                                 []
                                 bits-to-read))]
    (Integer/parseInt res-bits 2)))
