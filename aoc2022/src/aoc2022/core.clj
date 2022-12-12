(ns aoc2022.core
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [clojure.core.matrix :as mat]
   [aoc2022.utils :as u]
   [aoc2022.old :as old]
  ))


(defn parse-line [s]
  (let [[pos nb] (str/split s #" ")]
    [pos (read-string nb)]))

(defn- moveh [[x y] move]
  (case move
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn- near? [[x1 y1] [x2 y2]]
  (and
   (<= (abs (- x1 x2)) 1)
   (<= (abs (- y1 y2)) 1)))

;; (defn day9-1 [file]
;;   (loop [h [0 0], t [0 0], r #{},
;;          [[dir nb] & lines] (map parse-line (u/lines file))]
;;     (if lines
;;       (let [lines (if (= nb 1) lines (cons [dir (dec nb)] lines))
;;             newh (moveh h dir)
;;             newt (if (near? t newh) t h)
;;             r (conj r newt)]
;;         (recur newh newt r lines))
;;       (println (count r)))))

(defn- mv-coord [c1 c2]
  (let [c2cmp (if (> (abs c2) 1) 0 1)]
    (if (> (abs c1) c2cmp) (/ c1 (abs c1)) 0)))

(defn- move [[x1 y1 :as pt1] [x2 y2 :as pt2]]
  (let [distx (- x1 x2)
        disty (- y1 y2)
        mvx (mv-coord distx disty)
        mvy (mv-coord disty distx)]
    [(+ x2 mvx) (+ y2 mvy)]))

(defn move-all [newh tls]
  (loop [[hd & tl] tls, prev newh, newtls '()]
    (if hd
      (let [hd (move prev hd)]
        (recur tl hd (cons hd newtls)))
      newtls)))

(defn day9-2 [n file]
  (loop [h [0 0], tls (repeat n [0 0]), r #{},
         [[dir nb :as line] & lines] (map parse-line (u/lines file))]
    (if line
      (let [lines (if (= nb 1) lines (cons [dir (dec nb)] lines))
            newh (moveh h dir)
            newtls (move-all newh tls)
            last-tl (first newtls)
            r (conj r last-tl)
            newtls (reverse newtls)]
        (recur newh newtls r lines))
      (println (count r)))))

(println "START\n")
(day9-2 9 (resource "day9.input"))
