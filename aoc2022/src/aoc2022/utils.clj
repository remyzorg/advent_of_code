(ns aoc2022.utils
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   ))

(defn reduce-file [f file init]
  (with-open [rdr (io/reader file)]
    (reduce f init (line-seq rdr))))

(defn lines [file]
  (str/split-lines (slurp file)))

(defn exist? [f s]
  (loop [[hd & tl] s]
    (if hd
      (if (f hd) true
          (recur tl))
      false)))

(defn find-first [f s]
  (loop [[hd & tl] s]
    (if hd
      (if (f hd) hd (recur tl))
      nil)))
