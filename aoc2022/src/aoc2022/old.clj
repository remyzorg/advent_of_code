(ns aoc2022.old
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [aoc2022.core :as utils]
   ))

(defn day1_1 [file]
  (with-open [rdr (io/reader file)]
    (loop [[hd & tl] (line-seq rdr) cur 0 mx 0]
      (if (not hd) mx
          (if (= hd "")
            (recur tl 0 (max cur mx))
            (recur tl (+ cur (read-string hd)) mx))))))

(defn day1_2 [file]
  (with-open [rdr (io/reader file)]
    (loop [[hd & tl] (line-seq rdr) cur 0 mxs '()]
      (if (not hd) (reduce + mxs)
          (if (= hd "")
            (recur tl 0 (take 3 (sort > (cons cur mxs))))
            (recur tl (+ cur (read-string hd)) mxs))))))

(defn mod3 [n] (mod n 3))

(defn- score-mod [a p]
  (cond (= a p) 3
        (= (mod3 (inc a)) p) 6
        :else 0))

(defn day2_1 [file]
  (let [f (fn [total line]
            (let [a (mod3 (inc (int (get line 0))))
                  p  (mod3 (+ 2 (int (get line 2))))]
              (+ total (inc p) (score-mod a p))))]
    (utils/reduce-file file 0 f)))

(defn day2_2 [file]
  (let [f
        (fn [total line]
          (let [a (mod3 (inc (int (get line 0))))
                p  (mod3 (+ 2 (int (get line 2))))
                a-mod (case p 0 (mod3 (- a 1)) 1 a (mod3 (+ a 1)))
                p-mult (* p 3)]
            (+ total p-mult (inc a-mod))))]
    (utils/reduce-file file 0 f)))
