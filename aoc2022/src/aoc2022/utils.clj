(ns aoc2022.utils
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   ))

(defn reduce-file [file init f]
  (with-open [rdr (io/reader file)]
    (reduce f init (line-seq rdr))))
