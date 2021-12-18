(ns advent-of-code-2021.day15
  (:require [clojure.string :as s]))

(def filename "./inputs/day15_example.txt")

(defrecord Coord [x y])

(defrecord Coord* [x y value distance prev visited?])

(defn get-cell [grid {:keys [x y]}]
  (nth (nth grid y) x))

(def data
  (let [rows (s/split (slurp filename) #"\n")]
    (vec (for [row rows]
           (->> (seq row)
                (map str)
                (mapv #(Integer/parseInt % 10)))))))

(defn parse-data [grid]
  (let [dim (count grid)]
    (vec (for [y (range 0 dim)]
           (vec (for [x (range 0 dim)]
                  (->Coord* x y
                            (get-cell grid {:x x :y x})
                            (if (and (zero? x)
                                     (zero? y))
                              0
                              ##Inf)
                            nil
                            false)))))))


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

(defn get-neighbor-points [grid coord]
  (filter (partial point-in-grid? grid)
          (get-neighbor-coords coord)))

(defn distance-to [{:keys [x y] :as coord} target]
  (+ (- (:x target) x)
     (- (:y target) y)))

(defn lower-right-corner [grid]
  (->Coord (dec (count (first grid)))
           (dec (count grid))))

(defn get-cost-and-distance [grid coord target]
  (-> coord
      (assoc :cost (get-cell grid coord))
      (assoc :distance (distance-to coord target))))

(defn get-all-nodes [grid]
  (vec
   (for [x (range 0 (count (first grid)))
         y (range 0 (count grid))]
     (->Coord* y x
               (if (and (zero? x)
                        (zero? y))
                 0
                 (get-cell grid (->Coord x y)))
               (if (and (zero? x)
                        (zero? y))
                 0
                 ##Inf)
               nil
               false))))

(defn neighbor? [p1 p2]
  (let [x1 (:x p1)
        y1 (:y p1)
        x2 (:x p2)
        y2 (:y p2)]
    (or (and (= x1 x2)
             (= 1 (Math/abs (- y1 y2))))
        (and (= y1 y2)
             (= 1 (Math/abs (- x1 x2)))))))

(defn same? [p1 p2]
  (and (= (:x p1)
          (:x p2))
       (= (:y p1)
          (:y p2))))

(defn not-curr-or-its-neighbor? [curr node]
  (and (false? (same? curr node))
       (false? (neighbor? curr node))))

(defn dijkstra* [grid]
  (let [target (lower-right-corner grid)]
    (loop [nodes (get-all-nodes grid)]
      (let [unvisited-nodes (filter (complement :visited?) (flatten nodes))
            visited-nodes (filter :visited? nodes)
            visited-count (count visited-nodes)]
        (when (zero? (mod visited-count 1000))
          (println (str (count visited-nodes) " / " (count nodes) " nodes visited...")))
        (if (empty? unvisited-nodes)
          nodes
          (let [curr-node (->> unvisited-nodes
                               (sort-by :distance <)
                               first)
                neighbors (filter (partial neighbor? curr-node) unvisited-nodes)
                rest-nodes (filterv (partial not-curr-or-its-neighbor? curr-node) unvisited-nodes)
                updated-neighbors (mapv (fn [{:keys [value distance] :as node}]
                                          (let [dist-through-curr (+ (:distance curr-node) value)]
                                            (if (< dist-through-curr distance)
                                              (-> node
                                                  (assoc :distance dist-through-curr)
                                                  (assoc :prev (->Coord (:x curr-node)
                                                                        (:y curr-node))))
                                              node)))
                                        neighbors)]
            (recur (-> rest-nodes
                       (into updated-neighbors)
                       (into visited-nodes)
                       (conj (assoc curr-node :visited? true))))))))))

(defn update-cell [grid {:keys [x y] :as cell}]
  (assoc-in grid [x y] cell))

#_(defn dijkstra [grid]
  (let [target (lower-right-corner grid)]
    (loop [graph grid
           prev nil
           unvisited-count (count (flatten grid))]
      (let [unvisited-nodes (filter (complement :visited?) (flatten graph))]
        (if (empty? unvisited-nodes)
          graph
          (let [curr-node (->> unvisited-nodes
                               (sort-by :distance <)
                               first)
                unvisited-neighbors (->> curr-node
                                         (get-neighbor-points graph)
                                         (map (partial get-cell graph))
                                         (filter (complement :visited?)))
                updated-neighbors (mapv (fn [{:keys [value distance] :as node}]
                                          (let [dist-through-curr (+ (:distance curr-node) value)]
                                            (if (< dist-through-curr distance)
                                              (-> node
                                                  (assoc :distance dist-through-curr)
                                                  (assoc :prev (->Coord (:x curr-node)
                                                                        (:y curr-node))))
                                              node)))
                                        unvisited-neighbors)
                updated-grid (reduce (fn [acc v]
                                       (update-cell acc v))
                                     graph updated-neighbors)]
            (if (= curr-node prev)
              {:curr curr-node :graph graph}
              (recur (update-cell updated-grid (assoc curr-node :visited? true))
                     curr-node
                     (count unvisited-nodes)))))))))

(defn decode-path [dijkstra-res]
  (let [starting-point (->> dijkstra-res
                            (filter (fn [{:keys [x y]}]
                                      (let [target-point (lower-right-corner data)]
                                        (= x (:x target-point)
                                           y (:y target-point))))))]
    (loop [res starting-point]
      (let [curr (first res)]
        (if (and (zero? (:x curr))
                 (zero? (:y curr)))
          res
          (let [prev (->> dijkstra-res
                          (filter (fn [{:keys [x y] :as node}]
                                    (and (= x (get-in curr [:prev :x]))
                                         (= y (get-in curr [:prev :y])))))
                          first)]

            (recur (conj res prev))))))))

(defn part-01 []
  (let [dijkstra-res (dijkstra data)
        {:keys [x y]} (lower-right-corner data)]
    (->> dijkstra-res
         (filter (fn [node]
                   (and (= x (:x node))
                        (= y (:y node)))))
         first
         :distance)))

(defn get-indexes
  "If the index is e.g. 23, then the value is found in index
  3 of the original grid, incremented 3 times."
  [int]
  (let [numbers (map read-string (s/split (str int) #""))]
    (if (= 1 (count numbers))
      {:increment 0
       :index (first numbers)}
      {:increment (first numbers)
       :index (last numbers)})))

(defn rolling-increment [number to-add1 to-add2]
  (let [result (+ number to-add1 to-add2)]
    (if (< result 10)
      result
      (inc (:index (get-indexes result))))))

(defn make-larger-grid [grid]
  (let [orig-dim (count (first grid))]
    (vec (for [y (range 0 (* 5 orig-dim))
               :let [indexes-y (get-indexes y)
                     increment-y (:increment indexes-y)
                     index-y (:index indexes-y)]]
           (vec (for [x (range 0 (* 5 orig-dim))
                      :let [indexes-x (get-indexes x)
                            increment-x (:increment indexes-x)
                            index-x (:index indexes-x)
                            original-value (get-cell grid (->Coord index-x index-y))]]
                  (rolling-increment original-value increment-x increment-y)))))))

;; Insanely slow with the larger input, but couldn't bother to refactor.
(defn part-02 []
  (let [grid (make-larger-grid data)
        dijkstra-res (dijkstra* grid)
        {:keys [x y] :as p} (lower-right-corner grid)]
    (->> dijkstra-res
         (filter (fn [node]
                   (and (= x (:x node))
                        (= y (:y node)))))
         first
         :distance)))
