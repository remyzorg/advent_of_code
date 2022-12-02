(ns aoc2022.utils
  (:require
   [clojure.java.io :refer [resource]]
   [clojure.string :as str]
   ))

(defn reduce-file [file init f]
  (with-open [rdr (reader file)]
    (reduce f init (line-seq rdr))))
