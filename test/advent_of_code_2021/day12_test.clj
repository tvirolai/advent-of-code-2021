(ns advent-of-code-2021.day12-test
  (:require [advent-of-code-2021.day12 :as sut]
            [clojure.test :refer :all]))

(deftest examples-part-1
  (is (= 10 (sut/path-count "./inputs/day12_example1.txt")))
  (is (= 19 (sut/path-count "./inputs/day12_example2.txt")))
  (is (= 226 (sut/path-count "./inputs/day12_example3.txt"))))

(deftest examples-part-2
  (let [path-count (fn [filename]
                     (let [data (sut/parse-input filename)]
                       (sut/dfs* data "start" '())))]
    (is (= 36 (path-count "./inputs/day12_example1.txt")))
    (is (= 103 (path-count "./inputs/day12_example2.txt")))
    (is (= 3509 (path-count "./inputs/day12_example3.txt")))))
