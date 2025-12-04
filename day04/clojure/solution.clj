(ns solution
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input []
  (-> (io/file (io/resource "../input.txt"))
      (io/file "../input.txt")
      slurp
      str/trim
      (str/split #"\n")))

(def directions
  "8 directions for neighbor checking (including diagonals)"
  [[-1 -1] [-1 0] [-1 1]
   [0 -1]         [0 1]
   [1 -1]  [1 0]  [1 1]])

(defn count-adjacent-rolls [grid rows cols r c]
  "Count how many adjacent cells contain rolls (@)"
  (reduce (fn [count [dr dc]]
            (let [nr (+ r dr)
                  nc (+ c dc)]
              (if (and (>= nr 0) (< nr rows)
                       (>= nc 0) (< nc cols)
                       (= \@ (get-in grid [nr nc])))
                (inc count)
                count)))
          0
          directions))

(defn part1 []
  "Count rolls of paper that can be accessed by a forklift.
   A roll can be accessed if it has fewer than 4 adjacent rolls."
  (let [lines (read-input)
        grid (vec (map vec lines))
        rows (count grid)
        cols (if (pos? rows) (count (first grid)) 0)]
    (reduce (fn [accessible-count r]
              (reduce (fn [count c]
                        (if (= \@ (get-in grid [r c]))
                          (let [adjacent-rolls (count-adjacent-rolls grid rows cols r c)]
                            (if (< adjacent-rolls 4)
                              (inc count)
                              count))
                          count))
                      accessible-count
                      (range cols)))
            0
            (range rows))))

(defn find-removable [grid rows cols]
  "Find all rolls that can be removed (fewer than 4 adjacent rolls)"
  (for [r (range rows)
        c (range cols)
        :when (= \@ (get-in grid [r c]))
        :let [adjacent-rolls (count-adjacent-rolls grid rows cols r c)]
        :when (< adjacent-rolls 4)]
    [r c]))

(defn part2 []
  "Count total rolls removed by iteratively removing accessible rolls.
   Repeat until no more rolls can be removed."
  (let [lines (read-input)
        initial-grid (vec (map vec lines))
        rows (count initial-grid)
        cols (if (pos? rows) (count (first initial-grid)) 0)]
    (loop [grid initial-grid
           total-removed 0]
      (let [removable (find-removable grid rows cols)]
        (if (empty? removable)
          total-removed
          (let [new-grid (reduce (fn [g [r c]]
                                   (assoc-in g [r c] \.))
                                 grid
                                 removable)]
            (recur new-grid (+ total-removed (count removable)))))))))

(defn -main []
  (println (str "Part 1: " (part1)))
  (println (str "Part 2: " (part2))))

(-main)
