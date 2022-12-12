(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as mat]
   [aoc2022.utils :as u]
   [aoc2022.old :as old]
  ))

(defn- parse-line [line]
  (let [line (str/split line #" ")]
    (if (= (first line) "noop")
      [:noop]
      [:add (read-string (second line))]
      )))

(defn- find-cycle [cycle next-cycle vs]
  (u/find-first #(and (<= cycle %) (> next-cycle %)) vs))

(defn build-cycle-values [lines]
  (loop [[hd & tl] lines, x 1, r '()]
    (if hd
      (let [r (case (first hd)
                :add (cons (+ x (second hd)) (cons x r))
                :noop (cons x r))]
        (recur tl (first r) r))
      (reverse r)
      )))

(defn day10-1 [file]
  (let [lines (map parse-line (u/lines file))
        cycle-values (into [] (build-cycle-values lines))]
    (reduce (fn [sum v]
              (+ sum (* v (get cycle-values (- v 1)))))
            0 [20 60 100 140 180 220])))


(defn- get-xy [cycle] [(dec (mod cycle 40)) (quot cycle 40)])

(defn- init-mat []
  (mat/fill! (mat/mutable (mat/new-matrix 6 40)) "."))

(defn day10-2 [file]
  (let [cycle-values (build-cycle-values (map parse-line (u/lines file)))
        m (init-mat)]
    (doseq [[i v] (map-indexed vector cycle-values)]
      (let [[x y] (get-xy (inc i))]
        (if (or (= v x) (= v (inc x)) (= v (+ x 2)))
          (mat/mset! m y x "#")
          )))
    (mat/pm m)))

(println "\nSTART")

(day10-2 (resource "day10.input"))
