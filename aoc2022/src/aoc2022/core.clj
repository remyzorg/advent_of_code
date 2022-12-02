(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [aoc2022.core :as utils]
   [aoc2022.old :as old]
  ))


(old/day2_2 (resource "day2.input"))

