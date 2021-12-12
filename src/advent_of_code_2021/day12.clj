(ns advent-of-code-2021.day12
  (:require [clojure.string :as s]))

(def filename
  "./inputs/day12.txt")

(defn big? [cave]
  (= cave (s/upper-case cave)))

(defn small-cave? [cave]
  (and (not (contains? #{"start" "end"} cave))
       (= cave (s/lower-case cave))))

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

(defn dfs [G a seen]
  (cond
    (= "end" a) 1
    (and (not (big? a))
         (contains? seen a)) 0
    :else (reduce + (map #(dfs G % (conj seen a))
                         (get G a)))))

(defn path-count [filename]
  (let [data (parse-input filename)]
    (dfs data "start" #{})))

(defn part-01 []
  (path-count filename))

;; PART 2

(defn contains-one-small-twice? [seen]
  (let [only-small (remove (fn [cave]
                             (or (big? cave)
                                 (#{"start" "end"} cave))) seen)]
    (if (empty? only-small)
      false
      (= 2 (apply max (vals (frequencies only-small)))))))

(defn dfs* [G a seen]
  (let [revisiting-small-possible? (false? (contains-one-small-twice? seen))]
    (cond
      (= "end" a) 1
      (and (and (small-cave? a)
                (not revisiting-small-possible?))
           (contains? (set seen) a)) 0
      (and (not (empty? seen))
           (= "start" a)) 0
      :else (reduce + (map #(dfs* G % (conj seen a))
                           (get G a))))))

(defn part-02 []
  (let [data (parse-input filename)]
    (dfs* data "start" '())))
