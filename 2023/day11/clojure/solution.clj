#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-grid
  "Parse the grid and return galaxy positions as [row col] pairs."
  [lines]
  (for [r (range (count lines))
        c (range (count (nth lines r)))
        :when (= \# (nth (nth lines r) c))]
    [r c]))

(defn find-empty-rows
  "Find row indices that contain no galaxies."
  [lines]
  (set (for [r (range (count lines))
             :when (not (str/includes? (nth lines r) "#"))]
         r)))

(defn find-empty-cols
  "Find column indices that contain no galaxies."
  [lines]
  (let [rows (count lines)
        cols (if (pos? rows) (count (first lines)) 0)]
    (set (for [c (range cols)
               :when (every? #(not= \# (nth (nth lines %) c)) (range rows))]
           c))))

(defn calculate-distance
  "Calculate Manhattan distance between two galaxies with expansion."
  [[r1 c1] [r2 c2] empty-rows empty-cols expansion-factor]
  (let [min-r (min r1 r2)
        max-r (max r1 r2)
        min-c (min c1 c2)
        max-c (max c1 c2)
        row-expansion (count (filter #(contains? empty-rows %) (range min-r max-r)))
        col-expansion (count (filter #(contains? empty-cols %) (range min-c max-c)))
        row-dist (+ (- max-r min-r) (* row-expansion (dec expansion-factor)))
        col-dist (+ (- max-c min-c) (* col-expansion (dec expansion-factor)))]
    (+ row-dist col-dist)))

(defn all-pairs
  "Generate all unique pairs from a collection."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(nth v i) (nth v j)])))

(defn calculate-distances
  "Calculate sum of Manhattan distances between all pairs of galaxies."
  [galaxies empty-rows empty-cols expansion-factor]
  (reduce + (map (fn [[g1 g2]]
                   (calculate-distance g1 g2 empty-rows empty-cols expansion-factor))
                 (all-pairs galaxies))))

(defn part1
  "Solve Part 1 - expansion factor of 2."
  [lines]
  (let [galaxies (parse-grid lines)
        empty-rows (find-empty-rows lines)
        empty-cols (find-empty-cols lines)]
    (calculate-distances galaxies empty-rows empty-cols 2)))

(defn part2
  "Solve Part 2 - expansion factor of 1,000,000."
  [lines]
  (let [galaxies (parse-grid lines)
        empty-rows (find-empty-rows lines)
        empty-cols (find-empty-cols lines)]
    (calculate-distances galaxies empty-rows empty-cols 1000000)))

(defn -main []
  (let [input-file (or (first *command-line-args*) "../input.txt")
        content (slurp input-file)
        lines (vec (remove empty? (str/split-lines content)))]
    (println "Part 1:" (part1 lines))
    (println "Part 2:" (part2 lines))))

(-main)
