(ns advent-of-code-2021.day04
  (:require [clojure.string :as s]))

(def filename
  "./inputs/day04.txt")

(defn square->rows [square]
  (let [horiz-rows (for [line square]
                     (->> (s/split line #" ")
                          (filter (complement empty?))
                          (mapv read-string)))]
    horiz-rows))

(defn square->all-rows [square]
  (let [transpose (for [i (range 5)]
                    (mapv #(nth % i) square))]

    (->> [square transpose]
         flatten
         (partition 5)
         (map vec))))

(defn read-input []
  (let [data (s/split (slurp filename) #"\n")
        squares (->> data
                     rest
                     (partition-by empty?)
                     (filter #(< 1 (count %)))
                     (map square->rows))]
    {:nos (map read-string (s/split (first data) #","))
     :squares squares
     :rows (->> squares
                (map (fn [horiz-rows]
                       (for [i (range 5)]
                         (mapv #(nth % i) horiz-rows))))
                (conj squares)
                flatten
                (partition 5)
                (map vec))}))

(defrecord Row [seen vals row square all-seen])

(defn square-contains-row? [square row]
  (let [all-rows ()]))

(defn solve []
  (let [{:keys [nos squares rows]} (read-input)]
    (loop [numbers nos
           rs (map (fn [ro]
                     (->Row []
                            (set ro)
                            ro
                            (->> squares
                                 (filter (fn [sq]
                                           (let [r (filter (partial = ro) (square->all-rows sq))]
                                             (first r))))) '()))
                   rows)]
      (let [[full-row] (filter #(= 5 (count (:seen %))) rs)]
        (if (seq full-row)
          full-row
          (recur (rest numbers)
                 (map (fn [{:keys [seen vals all-seen] :as row}]
                        (let [n (first numbers)
                              res (assoc row :seen (conj seen n))
                              val (if (vals n)
                                    (assoc row :seen (conj seen n))
                                    row)]
                          (assoc val :all-seen (conj all-seen n))))
                      rs)))))))

(defn part-01 []
  (let [{:keys [seen vals row square all-seen]} (solve)
        last-number (last seen)
        unmarked (->> square
                      flatten
                      (filter #(nil? ((set all-seen) %))))]
    (* last-number (reduce + unmarked))))

(defn board-wins? [board]
  (= 5 (count (:seen board))))

(defn square-wins? [square winning-row]
  (let [rows (square->all-rows (:square square))]
    (boolean (filter #(= winning-row %) rows))))

(defrecord Square [square rows seen all-seen])

#_(defn solve-last []
  (let [{:keys [nos squares rows]} (read-input)]
    (loop [numbers nos
           squares (map (fn [square]
                          (->Square
                           (square->all-rows square)
                           []
                           []))
                        squares)]
      (let [winning-squares (filter square-wins? squares)
            non-winning (remove square-wins? squares)
            curr-number (first numbers)]
        (if (empty? non-winning)
          winning-squares
          (recur (rest numbers)
                 (->> squares
                      (remove square-wins?)
                      (map (fn [{:keys [rows seen all-seen]}]
                             )))))))))

(defrecord Row2 [seen vals row square all-seen checksum])

(def tila (atom ""))

(defn solve-last []
  (let [{:keys [nos squares rows]} (read-input)]
    (loop [numbers nos
           rs (map (fn [ro]
                     (let [square (->> squares
                                       (filter (fn [sq]
                                                 (let [r (filter (partial = ro) (square->all-rows sq))]
                                                   (first r)))))
                           checksum (reduce + (flatten square))]
                       (->Row2 []
                               (set ro)
                               ro
                               (->> squares
                                    (filter (fn [sq]
                                              (let [r (filter (partial = ro) (square->all-rows sq))]
                                                (first r)))))
                               []
                               checksum)))
                   rows)
           all-seen []
           i 0]
      (let [curr-number (first numbers)
            winning-rows (filter #(= 5 (count (:seen %))) rs)
            winning-checksum (:checksum (first winning-rows))
            rest-rows (remove #(= winning-checksum (:checksum %)) rs)]
        (if (seq winning-rows)
          winning-rows
          (recur (rest numbers)
                 (map (fn [{:keys [seen vals row] :as board}]
                        (if (vals curr-number)
                          (assoc board :seen (conj seen curr-number))
                          board))
                      rest-rows)
                 (conj all-seen curr-number)
                 (inc i)))))))

(def tila (atom []))

(defn solve* []
  (let [{:keys [nos squares rows]} (read-input)]
    (loop [numbers nos
           rs (map (fn [ro]
                     (->Row []
                            (set ro)
                            ro
                            (->> squares
                                 (filter (fn [sq]
                                           (let [r (filter (partial = ro) (square->all-rows sq))]
                                             (first r)))))
                            []))
                   rows)]
      (let [full-row (filter #(= 5 (count (:seen %))) rs)
            full-boards (->> full-row
                             (map :square)
                             set)
            rest-boards (remove #(full-boards (:square %)) rs)]
        (swap! tila conj {:full (count full-row)
                          :rest-boards (count rest-boards)})
        (if (empty? rest-boards)
          full-row
          (recur (rest numbers)
                 (map (fn [{:keys [seen vals all-seen] :as row}]
                        (let [n (first numbers)
                              res (assoc row :seen (conj seen n))
                              val (if (vals n)
                                    (assoc row :seen (conj seen n))
                                    row)]
                          (assoc val :all-seen (conj all-seen n))))
                      rest-boards)))))))

(defn part-02 []
  (let [res (first (solve*))
        board-values (->> res :square flatten)
        seen (set (:all-seen res))
        unseen (remove #(seen %) board-values)
        last-val (last (:all-seen res))]
    (* last-val (reduce + unseen))))
