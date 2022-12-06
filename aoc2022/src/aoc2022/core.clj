(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [aoc2022.utils :as u]
   [aoc2022.old :as old]
  ))


;; Pour Aurore.
(defn day6_1 [file]
  (loop [[a & [b c d & _ :as rest]] (seq (first (u/lines file))) counter 0]
    (if (= (count (set [a b c d])) 4)
      (+ counter 4)
      (recur rest (inc counter)))
    ))

;; Pour Aurore.
(defn day6_2 [file nbchar]
  (loop [[_ & rest :as line ] (seq (first (u/lines file))) counter 0]
    (let [elts (take nbchar line)]
      (if (= (count (set elts)) nbchar)
        (+ counter nbchar)
        (recur rest (inc counter)))
      )))

(day6_2 (resource "day6.input") 14)
