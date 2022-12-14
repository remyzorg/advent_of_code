(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as mat]
   [aoc2022.utils :as u]
   [aoc2022.old :as old]
  ))

(defn- parse-file [file]
  (->>
   (str/split (slurp file) #"\n\n")
   (map #(str/split % #"\n"))
   (map (fn [cpl] (map read-string cpl)))))

(defn- right-order [n [hd1 & tl1] [hd2 & tl2]]
  (print (apply concat (repeat n "  ")) "- Compare" hd1 "VS" hd2 " ")
  (cond
    (not hd1) (do
                (println "true")
                true)
    (not hd2) (do
                (println false)
                false)

    (and (not (int? hd1)) (not (int? hd2)))
    (do
      (println "seq / seq -> rec")
      (and (right-order (inc n) hd1 hd2) (right-order n tl1 tl2))
      )

    (and (int? hd1) (int? hd2))
    (do
      (print "int / int ")
      (cond
        (> hd1 hd2) (do
                      (println "false")
                      false)
        (< hd1 hd2) (do
                      (println "true")
                      true)
        :else
        (do
          (println "-> rec")
          (right-order n tl1 tl2))
        ))

    :else (do
            (println "else rec")
            (and (if (int? hd1) (right-order (inc n) [hd1] hd2)
                     (right-order (inc n) hd1 [hd2]))
               (right-order n tl1 tl2)))
    ))


(defn- test-right-order []
  (assert (right-order 0 [[1 2 3]] [9]))
  (assert (not (right-order 0 [9] [[1 2 3]])))
  (assert (right-order 0 [1] [[2 3 4]]))
  (assert (right-order 0 [] [[]]))
  (assert (not (right-order 0 [[]] [])))
  (assert (right-order 0 [1] [2 3 4]))
  (assert (right-order 0 [] [2 3 4]))
  (assert (right-order 0 [] [2 3 4]))
  (assert (not (right-order 0 [1 1 1] [1 1])))
)

(test-right-order)

(defn day12-1 [file]
  (let [pairs (parse-file file)]

    ;; (doseq [p pairs] (println p))

    (println (first (reduce (fn [[sum id] [p1 p2]]
                              ;; (println "")
                              (if (right-order 0 p1 p2)
                                (do (println id)
                                  [(+ sum id) (inc id)])
                                [sum (inc id)])
                              ) [0 1] pairs)))
    ))

(println "\nSTART")

(day12-1 (resource "day12.input"))

