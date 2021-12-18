(ns advent-of-code-2021.day17)

(defrecord Position [x-coord y-coord x-vel y-vel])

(defrecord Target [x1 x2 y1 y2])

(def target
  (->Target 117 164 -140 -89))

(defn in-target? [{:keys [x1 x2 y1 y2] :as t}
                  {:keys [x-coord y-coord] :as pos}]
  (and (<= x1 x-coord x2)
       (<= y1 y-coord y2)))

(defn past-target? [target position]
  (or (< (:y-coord position)
         (:y1 target))
      (> (:x-coord position)
         (:x2 target))))

(defn next-position [{:keys [x-coord y-coord x-vel y-vel]}]
  (->Position (+ x-coord x-vel)
              (+ y-coord y-vel)
              (max (dec x-vel) 0)
              (dec y-vel)))

(defn max-height [position]
  (let [positions (take-while (fn [pos]
                                (>= (:y-coord pos)
                                   (:y-coord position)))
                              (iterate next-position position))]
    (->> positions
         (map :y-coord)
         (apply max))))

(defn hits-target? [target position]
  (let [positions (take-while #(not (past-target? target %))
                              (iterate next-position position))]
    (->> positions
         (filter (partial in-target? target))
         empty?
         not)))

;; Uninspired brute-forcing this time
(defn part-01 []
  (let [vals (for [x-vel (range 10 2000)
                   y-vel (range 1 2000)
                   :let [pos (->Position 0 0 x-vel y-vel)]
                   :when (hits-target? target pos)]
               (max-height (->Position 0 0 x-vel y-vel)))]
    (apply max vals)))

(defn part-02 []
  (let [vals (for [x-vel (range 10 2000)
                   y-vel (range -2000 2000 1)
                   :let [pos (->Position 0 0 x-vel y-vel)]
                   :when (hits-target? target pos)]
               pos)]
    (count vals)))
