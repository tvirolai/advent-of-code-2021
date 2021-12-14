(ns advent-of-code-2021.day14
  (:require [clojure.string :as s]))

(def data
  (let [[polymer _ & instrs] (s/split (slurp "./inputs/day14.txt") #"\n")]
    {:polymer polymer
     :instrs (into {} (for [line instrs
                            :let [[from to] (s/split line #" -> ")
                                  target (str (first from) to (last from))]]
                        [from target]))}))

(defn next-state [instrs polymer]
  (let [matching-instrs (filter (fn [{:keys [from]}]
                                  (s/includes? polymer from))
                                instrs)
        chunks (partition 2 1 polymer)]
    (->> chunks
         (map (fn [chunk]
                (let [s-chunk (apply str chunk)]
                  (get instrs s-chunk chunk))))
         (map-indexed (fn [idx itm]
                        (if (= idx (dec (count chunks)))
                          itm
                          (subs itm 0 2))))
         s/join)))


(defn part-01 []
  (let [{:keys [polymer instrs]} data
        res (->> polymer
                 (iterate (partial next-state instrs))
                 (take 11)
                 last)
        counts (vals (frequencies res))]
    (- (apply max counts) (apply min counts))))

;; Part 2

(defn next-state* [{:keys [instrs counts]}]
  (loop [operations (keys instrs)
         res counts]
    (if (empty? operations)
      {:instrs instrs
       :counts (reduce-kv (fn [m k v]
                            (let [orig-count (get counts k)]
                              (assoc m k (- v orig-count))
                              ))
                          {}
                          res)}
      (let [instruction (first operations)
            instr-count (get counts instruction)
            [v1 v2] (get instrs instruction)]
        (recur (rest operations)
               (-> res
                   #_(assoc instruction 0)
                   (update v1 + instr-count)
                   (update v2 + instr-count)))))))

(def data2
  (let [{:keys [polymer instrs]} data
        new-instrs (reduce-kv (fn [m k v]
                                (let [[p1 p2] (map s/join (partition 2 1 v))]
                                  (assoc m k [p1 p2])))
                              {}
                              instrs)
        polymer-pairs (map s/join (partition 2 1 polymer))
        counts (reduce-kv (fn [m k _]
                            (let [pair-count-in-polymer (count (filter #(= k %)
                                                                       polymer-pairs))]
                              (assoc m k pair-count-in-polymer)))
                          {}
                          instrs)]
    {:polymer polymer
     :instrs new-instrs
     :counts counts}))

(defn character-counts [counts]
  (let [pairs (keys counts)
        alphabet (set (map str (s/join pairs)))]
    (reduce (fn [acc ch]
              (let [pairs-starting-with-ch (filter #(s/starts-with? % ch) pairs)
                    pairs-ending-with-ch (filter #(s/ends-with? % ch) pairs)
                    sum-starting (reduce (fn [acc pair]
                                           (+ acc (get counts pair 0)))
                                         0
                                         pairs-starting-with-ch)
                    sum-ending (reduce (fn [acc pair]
                                         (+ acc (get counts pair 0)))
                                       0
                                       pairs-ending-with-ch)]
                (assoc acc ch (max sum-starting sum-ending))))
            {}
            alphabet)))

(defn part-02 []
  (let [counts (->> data2
                    (iterate next-state*)
                    (take 41)
                    last
                    :counts
                    character-counts
                    vals)]
    (- (apply max counts)
       (apply min counts))))
