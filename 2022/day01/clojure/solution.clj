#!/usr/bin/env bb
(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  "Parse input into list of calorie totals per elf."
  (->> (slurp filename)
       str/trim
       (#(str/split % #"\n\n"))
       (map (fn [group]
              (->> (str/split-lines group)
                   (filter seq)
                   (map parse-long)
                   (reduce +))))))

(defn part1 [elves]
  "Find the Elf carrying the most Calories."
  (apply max elves))

(defn part2 [elves]
  "Find total calories carried by top three Elves."
  (->> elves
       (sort >)
       (take 3)
       (reduce +)))

(defn -main []
  (let [script-dir (-> *file*
                       (java.io.File.)
                       (.getParentFile)
                       (.getAbsolutePath))
        input-file (str script-dir "/../input.txt")
        elves (parse-input input-file)]
    (println "Part 1:" (part1 elves))
    (println "Part 2:" (part2 elves))))

(-main)
