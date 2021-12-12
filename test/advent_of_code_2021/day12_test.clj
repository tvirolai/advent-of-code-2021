(ns advent-of-code-2021.day12-test
  (:require [advent-of-code-2021.day12 :as day12]
            [clojure.test :refer :all]))

(deftest examples-part-1
  (let [path-count (fn [filename]
                     (let [data (day12/parse-input filename)]
                       (day12/dfs data "start" '() false)))]
  (is (= 10 (path-count "./inputs/day12_example1.txt")))
  (is (= 19 (path-count "./inputs/day12_example2.txt")))
  (is (= 226 (path-count "./inputs/day12_example3.txt")))))

(deftest examples-part-2
  (let [path-count (fn [filename]
                     (let [data (day12/parse-input filename)]
                       (day12/dfs data "start" '() true)))]
    (is (= 36 (path-count "./inputs/day12_example1.txt")))
    (is (= 103 (path-count "./inputs/day12_example2.txt")))
    (is (= 3509 (path-count "./inputs/day12_example3.txt")))))
