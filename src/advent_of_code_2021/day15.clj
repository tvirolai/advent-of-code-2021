(ns advent-of-code-2021.day15
  (:require [clojure.string :as s]))

(def filename "./inputs/day15_example.txt")

(defrecord Coord [x y])

(defrecord Coord* [x y visited?])

(def data
  (let [rows (s/split (slurp filename) #"\n")]
    (vec (for [row rows]
           (->> (seq row)
                (map str)
                (mapv #(Integer/parseInt % 10)))))))

(defn- get-neighbor-coords [coord]
  (let [{:keys [x y]} coord]
    (for [xi (range (dec x) (+ 2 x))
          yi (range (dec y) (+ 2 y))
          :when (and (>= xi 0)
                     (>= yi 0)
                     (or (= xi x)
                         (= yi y))
                     (not (and (= x xi)
                               (= y yi))))]
      (->Coord xi yi))))

(defn- point-in-grid? [grid {:keys [x y]}]
  (let [y-dim (count grid)
        x-dim (count (first grid))]
    (and (>= (dec x-dim) x 0)
         (>= (dec y-dim) y 0))))

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(defn get-neighbor-points [grid coord]
  (filter (partial point-in-grid? grid)
          (get-neighbor-coords coord)))

(defn distance-to [{:keys [x y] :as coord} target]
  (+ (- (:x target) x)
     (- (:y target) y)))

(defn lower-right-corner [grid]
  (->Coord (dec (count (first data)))
           (dec (count data))))

(defn get-cost-and-distance [grid coord target]
  (-> coord
      (assoc :cost (get-cell grid coord))
      (assoc :distance (distance-to coord target))))

(defn next-step [grid coord target]
  (if (= 1 (distance-to coord target))
    (get-cost-and-distance grid target target)
    (let [neighbors (get-neighbor-points grid coord)
          next-2-steps (mapcat (fn [neighbor]
                                 (let [next-neighbors (get-neighbor-points grid neighbor)]
                                   (for [alternative next-neighbors
                                         :let [enriched-neighbor (get-cost-and-distance grid neighbor target)
                                               enriched-alternative (get-cost-and-distance grid alternative target)
                                               ]]
                                     [enriched-neighbor (assoc enriched-alternative
                                                               :cumulative-cost (+ (:cost enriched-neighbor)
                                                                                   (:cost enriched-alternative)))])))
                               neighbors)
          min-distance (apply min (map :distance (flatten next-2-steps)))
          _ (println min-distance)
          alternatives-by-distance (filter (fn [[neighbor next-position]]
                                             (= min-distance (:distance next-position)))
                                           next-2-steps)
          min-cost (apply min (remove nil? (map :cumulative-cost (flatten alternatives-by-distance))))
          cheapest (->> alternatives-by-distance
                        (filter (fn [[neighbor next-position]]
                                  (= min-cost (:cumulative-cost next-position))))
                        first)]
      (first cheapest))))

(defn all-routes
  ([grid coord steps] (all-routes grid coord steps (list [coord])))
  ([grid coord steps results]
   (if (= steps (count (first results)))
     results
     (recur grid coord steps
            (mapcat (fn [route]
                    (for [coord (get-neighbor-points grid (last route))]
                      (conj route coord)))
                  results)))))

(defn next-step* [grid coord target degree]
  (let [steps (min (distance-to coord target) degree)]
    (if (= 1 steps)
      (get-cost-and-distance grid target target)
      (let [next-n-steps (for [route (all-routes grid coord steps)
                               :let [enriched-route (mapv #(get-cost-and-distance grid % target) route)
                                     cumulative-cost (->> (map :cost enriched-route)
                                                          (reduce +))]]
                           (assoc-in enriched-route [(dec steps) :cumulative-cost] cumulative-cost))
            min-distance (apply min (map :distance (flatten next-n-steps)))
            alternatives-by-distance (filter (fn [route]
                                               (= min-distance (:distance (last route))))
                                             next-n-steps)
            min-cost (apply min (remove nil? (map :cumulative-cost (flatten alternatives-by-distance))))
            cheapest (->> alternatives-by-distance
                          (filter (fn [route]
                                    (= min-cost (:cumulative-cost (last route)))))
                          first)]
        ;; First is the starting point here
        (second cheapest)))))

;; Return all coordinates of the path
(defn solve [grid start-point target]
  (loop [res []
         curr start-point]
    (println curr)
    (if (zero? (distance-to curr target))
      (conj res curr)
      (let [next (next-step* grid curr target 3)]
        (recur (conj res curr)
               next)))))

(defn testi []
  (let [path (solve data (->Coord 0 0) (lower-right-corner data))
        path-cost (reduce + (remove nil? (map :cost path)))]
    path-cost))

(defn testaa []
  (next-step* data (->Coord 0 0) (lower-right-corner data) 4))

(defn dijkstra [grid]
  (let [start  (->Coord 0 0)
        target (lower-right-corner grid)
        unvisited-nodes ()]
    target))
