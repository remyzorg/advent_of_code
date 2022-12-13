(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as mat]
   [aoc2022.utils :as u]
   [aoc2022.old :as old]
  ))



(println "\nSTART")

(day11-1 (resource "day11.input"))

(println (* 171832 172863))
