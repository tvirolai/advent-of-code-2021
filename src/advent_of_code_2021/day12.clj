(ns advent-of-code-2021.day12
  (:require [clojure.string :as s]))

(defn big? [cave]
  (= cave (s/upper-case cave)))

(defn small-cave? [cave]
  (and (not (contains? #{"start" "end"} cave))
       (= cave (s/lower-case cave))))

(defn contains-one-small-twice? [seen]
  (let [only-small (remove (fn [cave]
                             (or (big? cave)
                                 (#{"start" "end"} cave))) seen)]
    (if (empty? only-small)
      false
      (= 2 (apply max (vals (frequencies only-small)))))))

(defrecord Instr [from to])

(defn parse-input [filename]
  (let [paths (flatten (for [line (s/split (slurp filename) #"\n")
                             :let [[from to] (s/split line #"-")]]
                         [(->Instr from to)
                          (->Instr to from)]))]
    (->> paths
         (remove (fn [{:keys [from]}]
                   (= from "end")))
         (reduce (fn [acc {:keys [from to]}]
                   (if-not (contains? acc from)
                     (assoc acc from [to])
                     (update acc from conj to)))
                 {}))))

(defn dfs [G a seen can-revisit-one-small?]
  (let [revisiting-small-possible? (and can-revisit-one-small?
                                        (false? (contains-one-small-twice? seen)))]
    (cond
      (= "end" a) 1
      (and (and (small-cave? a)
                (not revisiting-small-possible?))
           (contains? (set seen) a)) 0
      (and (not (empty? seen))
           (= "start" a)) 0
      :else (reduce + (map #(dfs G % (conj seen a) can-revisit-one-small?)
                           (get G a))))))

(defn part-01 []
  (let [data (parse-input "./inputs/day12.txt")]
    (dfs data "start" '() false)))

(defn part-02 []
  (let [data (parse-input "./inputs/day12.txt")]
    (dfs data "start" '() true)))
