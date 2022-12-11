(ns aoc2022.old
  (:gen-class)
  (:require
   [clojure.java.io :refer [resource] :as io]
   [clojure.string :as str]
   [aoc2022.utils :as u]
   ))

;; DAY 1

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

;; DAY 2

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
    (u/reduce-file f file 0)))


(defn day2_2 [file]
  (let [f
        (fn [total line]
          (let [a (mod3 (inc (int (get line 0))))
                p  (mod3 (+ 2 (int (get line 2))))
                a-mod (case p 0 (mod3 (- a 1)) 1 a (mod3 (+ a 1)))
                p-mult (* p 3)]
            (+ total p-mult (inc a-mod))))]
    (u/reduce-file f file 0 )))

;; DAY 3

(defn priority [letter]
  (if (>= (int letter) 97)
    (inc (- (int letter) (int \a)))
    (+ 27 (- (int letter) (int \A)))))

(defn- str-first-half [s]
  (subs s 0 (/ (count s) 2)))

(defn- str-second-half [s]
  (subs s (/ (count s) 2)))

(defn- common-letter [s]
  (first (clojure.set/intersection
          (set (str-first-half s))
          (set (str-second-half s)))))

(defn day3_1 [file]
  (u/reduce-file
   (fn [total line]
     (+ (priority (common-letter line)) total))
   file 0 ))

(defn- common-letter [s1 s2 s3]
  (first (clojure.set/intersection (set s1) (set s2) (set s3))))

(defn day3_2 [file]
  (let [lines (u/reduce-file conj file '())]
    ;; (prn lines)
    (loop [[hd1 hd2 hd3 & tl] lines, total 0]
      (if hd1
        (recur tl (+ total (priority (common-letter hd1 hd2 hd3))))
        total))
    ))

;; DAY 4

(defn day4_1 [file]
  (let [f (fn [total line]
            (let [[s1 e1 s2 e2]
                  (map (comp read-string first) (re-seq #"(\d+)" line))]
              (cond
                (and (<= s1 s2) (>= e1 e2)) (+ total 1)
                (and (<= s2 s1) (>= e2 e1)) (+ total 1)
                :else total
                )))]
    (u/reduce-file f file 0)))

(defn day4_2 [file]
  (reduce
   #(let [[s1 e1 s2 e2] (map read-string (str/split %2 #",|-"))]
      (+ %1 (if (<= (max s1 s2) (min e1 e2)) 1 0)))
   0 (u/lines file)))

;; DAY 5

(defn- parse-crates [l stacks]
  ;; either 3 spaces or [word], followed by a space
  (let [new_crates (map #(get % 2) (re-seq #"(\s{3}|\[(\w)\])\s?" l))
        stacks (if (empty? stacks) (map (fn [_] '()) new_crates) stacks)]
    (map cons new_crates stacks)))

(defn- parse-move [l]
  (map #(-> % first read-string) (re-seq #"(\d+)" l)))

(defn- parse-all [file]
  (reduce (fn [[stacks moves] line]
            (cond
              ;; if it starts with m, we consider it to be a move
              (= (get line 0) \m) (list stacks (cons (parse-move line) moves))
              ;; if it contains a [, it is a crates line
              (str/includes? line "[") (list (parse-crates line stacks) moves)
              :else (list stacks moves)))
          (list '() '()) (u/lines file)))

(defn day5_1_2 [file]
  (let [[stacks moves] (parse-all file)]
    (->> stacks
         (map (partial filter (fn [v] v))) ;; remove nil put by parse-crates
         (map reverse) ;; recursion put stacks in the wrong order
         (into [])
         (#(reduce
            (fn [stacks [amount from to]]
              (let [[to from] (list (dec to) (dec from))
                    elts (take amount (get stacks from)) ;; read stack `from`
                    from_l (drop amount (get stacks from)) ;; delete elements from `from`
                    to_l (concat elts (get stacks to))] ;; reverse elts to get 1
                (-> stacks (assoc to to_l) (assoc from from_l))))
            % (reverse moves)))
         (map first))))

;; DAY 6

;; Pour Aurore.
(defn day6_1 [file]
  (loop [[a & [b c d & _ :as rest]] (seq (first (u/lines file)))
         counter 4]
    (if (= (count (set [a b c d])) 4)
      counter
      (recur rest (inc counter)))
    ))

;; Pour Aurore.
(defn day6_2 [file nbchar]
  (loop [[_ & rest :as line ] (seq (first (u/lines file)))
         counter nbchar]
    (if (= (count (set (take nbchar line))) nbchar)
      counter
      (recur rest (inc counter)))
    ))


;; DAY 7

(defn- dir? [l] (not (seq? l)))

(defn- size [l] (second l))

(defn- seq-or-val [v] (if (seq? v) (first v) v))

(defn- concat-path [path]
  (str/join "/" (reverse path)))

(defn- get-depth [s]
  (- (count (str/split s #"/")) 2))

(defn- traversal [depth sum res [line & tl :as lines]]
  (do
    (cond
      (not line)
      (list res sum tl)

      (< (get-depth (seq-or-val line)) depth)
      (do
        (list (if (< sum 100000) (+ sum res) res) sum lines)
        )

      (dir? line)
      (let [[res dir-sum tl] (traversal (inc depth) 0 res tl)]
        (traversal depth (+ dir-sum sum) res tl))
      :else (traversal depth (+ (size line) sum) res tl))
    )
  )

(defn- read-commands [file]
  (loop [[line & lines] (u/lines file)
         pwd '()
         files '()]
    (if line
      (let [[hd hd2 arg rest] (str/split line #" ")]
        (cond
          (= hd "$") (case hd2
                       "cd" (if (= arg "..")
                              (recur lines (drop 1 pwd) files)
                              (recur lines (cons arg pwd) files))
                       "ls" (recur lines pwd files))
          (= hd "dir") (recur lines pwd (cons (concat-path (cons hd2 pwd)) files))
          :else (recur lines pwd
                       (cons
                        (list (concat-path (cons hd2 pwd)) (read-string hd) )
                        files))
          ))
      files)))

(defn- sort-files [files]
  (sort (fn [f1 f2] (compare (seq-or-val f1) (seq-or-val f2))) files))

(defn day7-1 [file]
  (let [files (sort-files (read-commands file))]
    (println (traversal 1 0 0 files))
    ))

(defn- traversal [pwd depth sum res [line & tl :as lines]]
  (do
    (cond
      (or (not line) (< (get-depth (seq-or-val line)) depth))
      (list (cons (list pwd sum) res) sum lines)

      (dir? line)
      (let [[res dir-sum tl] (traversal line (inc depth) 0 res tl)]
        (traversal pwd depth (+ dir-sum sum) res tl))
      :else (traversal pwd depth (+ (size line) sum) res tl))
    )
  )

(defn- sort-dirs-by-size [dirs]
  (sort (fn [f1 f2] (compare (second f1) (second f2))) dirs))

(defn day7-2 [file]
  (let [files (sort-files (read-commands file))
        [dirs _ _] (traversal "/" 1 0 '() files)
        [root total-size] (first dirs)
        ]
    (loop [[[d size] & rest] (sort-dirs-by-size dirs)]
      (if (and (not (= d root)) (> (- 70000000 (- total-size size)) 30000000))
        (list d size)
        (recur rest)))
    ))
