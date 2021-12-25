(ns advent-of-code-2021.day25
  (:require [clojure.string :as s]))

(def filename "./inputs/day25.txt")

(defrecord Coord [x y])

(def initial-state
  {:> #{}
   :v #{}
   :. #{}
   :dim-x nil
   :dim-y nil})

(defn next-coord [{:keys [dim-x dim-y] :as state}
                  symbol
                  {:keys [x y] :as coord}]
  (condp = symbol
    :> (->Coord (if (= x (dec dim-x))
                  0
                  (inc x)) y)
    :v (->Coord x (if (= y (dec dim-y))
                    0
                    (inc y)))
    :. coord))

(def data
  (let [grid (vec (for [row (s/split (slurp filename) #"\n")]
                    (vec (s/split row #""))))
        matrix (for [y (range 0 (count grid))]
                 (for [x (range 0 (count (first grid)))
                       :let [value (keyword (nth (nth grid y) x))]]
                   [value (->Coord x y)]))]
    (reduce (fn [acc [symbol coord]]
              (update acc symbol conj coord))
               (-> state
                   (assoc :dim-x (count (first grid)))
                   (assoc :dim-y (count grid)))
               (reduce into '() matrix))))

(defn next-state [state]
  (let [intermediate-state (reduce (fn [acc v]
                                     (let [next-c (next-coord state :> v)]
                                       (if ((:. state) next-c)
                                         (-> acc
                                             (update :> conj next-c)
                                             (update :. conj v)
                                             (update :. disj next-c))
                                         (update acc :> conj v))))
                                   (assoc state :> #{})
                                   (:> state))]

    (reduce (fn [acc v]
              (let [next-c (next-coord intermediate-state :v v)]
                (if ((:. intermediate-state) next-c)
                  (-> acc
                      (update :v conj next-c)
                      (update :. conj v)
                      (update :. disj next-c))
                  (update acc :v conj v))))
            (assoc intermediate-state :v #{})
            (:v intermediate-state))))

(defn part-01 []
  (let [states (iterate next-state data)]
    (loop [curr (first states)
           prev nil
           next-states (rest states)
           i 0]
      (reset! debug {:i i
                     :curr curr
                     :prev prev})
      (if (= curr prev)
        i
        (recur (first next-states)
               curr
               (rest next-states)
               (inc i))))))
