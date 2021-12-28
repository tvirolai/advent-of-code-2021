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

;; Examples

(def ex1 "D2FE28")

(def ex1-bin (s/join (map hex->binary ex1)))

(def ex2 "38006F45291200")

(def ex2-bin (s/join (map hex->binary ex2)))

(def ex3 "EE00D40C823060")

(def ex3-bin (s/join (map hex->binary ex3)))

(def ex4 "8A004A801A8002F478")

(def ex4-bin (s/join (map hex->binary ex4)))

(def ex5 "620080001611562C8802118E34")

(def ex5-bin (s/join (map hex->binary ex5)))

(def ex6 "C0015000016115A2E0802F182340")

(def ex6-bin (s/join (map hex->binary ex6)))

(def ex7 "A0016C880162017C3686B18A3D4780")

(def ex7-bin (s/join (map hex->binary ex7)))

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
    #_res-bits
    sections))

(defn get-type [input]
  (if (= 4 (get-id input))
    :literal
    :operator))

(defn get-length-type-id [input]
  (let [id (first (drop 6 input))]
    (Character/digit id 10)))

(defn get-sub-packets [input]
  (let [type-id (get-length-type-id input)]
    (if (zero? type-id)
      (let [subpacket-length (subs input 7 22)
            sublength (Integer/parseInt subpacket-length 2)]
        (subs input 22 (+ 22 sublength)))
      (let [number-of-subpackets (Integer/parseInt (subs input 7 18) 2)
            subpacket-data (subs input 18)]
        subpacket-data
        #_{:n number-of-subpackets
         :data subpacket-data}))))

(defrecord Literal-packet [version id type literal-value-bits literal-value size])
(defrecord Operator-packet [version id type length-type-id subpackets size])

(defn parse [input]
  (let [type (get-type input)
        version (get-version input)
        id (get-id input)
        literal-val (when (= 4 id) (get-literal-value input))]
    (condp = type
      :literal (->Literal-packet
                version
                id
                type
                literal-val
                (Integer/parseInt literal-val 2)
                (+ 6 (/ (count literal-val) 4) (count literal-val)))
      :operator (->Operator-packet
                 version
                 id
                 type
                 (get-length-type-id input)
                 (get-sub-packets input)))))
