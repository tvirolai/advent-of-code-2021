(ns advent-of-code-2021.day10
  (:require [clojure.string :as s]))

(def inputfile "./inputs/day10.txt")

(def data
  (for [line (s/split (slurp inputfile) #"\n")]
    (mapv str (seq line))))

(def pairs
  [["(" ")"] ["{" "}"] ["[" "]"] ["<" ">"]])

(defn opening? [ch]
  (boolean ((set (map first pairs)) ch)))

(def closing? (complement opening?))

(defn matches? [opening closing]
  (<= 0 (.indexOf pairs [opening closing])))

(defn first-illegal-char
  ([line] (first-illegal-char line []))
  ([line queue]
   (if (empty? line)
     nil
     (let [char (first line)
           last-in-queue (peek queue)]
       (cond
         (opening? char) (recur (rest line)
                                (conj queue char))
         (and (closing? char)
              (matches? last-in-queue char)) (recur (rest line)
                                                    (pop queue))
         :else char)))))

(def points
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn part-01 []
  (let [illegals (->> data
                      (map first-illegal-char)
                      (filter string?))]
    (reduce + (for [illegal illegals]
                (get points illegal)))))

;;;; PART 2

(defn get-closing [char]
  (let [pair (filter (fn [pair]
                       (= (str char) (first pair)))
                     pairs)]
    (->> pair first last)))

(defn corrupted? [line]
  (not (nil? (first-illegal-char line))))

(def incomplete-lines
  (remove corrupted? data))

(defn remove-matching
  ([line] (remove-matching (apply str line) nil))
  ([line prev-line]
   (if (and (string? prev-line)
            (= line prev-line))
     line
     (recur (-> line
                (s/replace #"\(\)" "")
                (s/replace #"\[\]" "")
                (s/replace #"\{\}" "")
                (s/replace #"\<\>" ""))
            line))))

(defn get-completion [line]
  (let [chars (remove-matching line)]
    chars
    (reverse (map get-closing chars))))

(defn get-completion-score [completion]
  (loop [score 0
         chars completion]
    (if (empty? chars)
      score
      (let [char-value (condp = (first chars)
                         ")" 1
                         "]" 2
                         "}" 3
                         ">" 4)]
        (recur (+ (* 5 score) char-value)
               (rest chars))))))

(defn part-02 []
  (let [completions (map get-completion incomplete-lines)
        scores (sort (map get-completion-score completions))]
    (nth scores (Math/floor (/ (count scores ) 2)))))
