(ns advent-of-code-2021.core
  (:require [clojure.string :as s]))

(defn read-input [filename]
  (s/split (slurp (str "./inputs/" filename)) #"\n"))
