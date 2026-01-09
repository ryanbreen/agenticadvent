#!/usr/bin/env clojure

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [filename]
  (->> (slurp filename)
       str/split-lines
       (filter (complement str/blank?))
       (map #(str/split % #" "))))

(defn part1
  "X=Rock, Y=Paper, Z=Scissors"
  [rounds]
  (let [shape-score {"X" 1 "Y" 2 "Z" 3}
        outcomes {["A" "X"] 3   ; Rock vs Rock = draw
                  ["A" "Y"] 6   ; Rock vs Paper = win
                  ["A" "Z"] 0   ; Rock vs Scissors = loss
                  ["B" "X"] 0   ; Paper vs Rock = loss
                  ["B" "Y"] 3   ; Paper vs Paper = draw
                  ["B" "Z"] 6   ; Paper vs Scissors = win
                  ["C" "X"] 6   ; Scissors vs Rock = win
                  ["C" "Y"] 0   ; Scissors vs Paper = loss
                  ["C" "Z"] 3}] ; Scissors vs Scissors = draw
    (reduce (fn [total [opp me]]
              (+ total (shape-score me) (outcomes [opp me])))
            0
            rounds)))

(defn part2
  "X=lose, Y=draw, Z=win"
  [rounds]
  (let [choices {["A" "X"] 3   ; Rock, need to lose -> Scissors
                 ["A" "Y"] 1   ; Rock, need to draw -> Rock
                 ["A" "Z"] 2   ; Rock, need to win -> Paper
                 ["B" "X"] 1   ; Paper, need to lose -> Rock
                 ["B" "Y"] 2   ; Paper, need to draw -> Paper
                 ["B" "Z"] 3   ; Paper, need to win -> Scissors
                 ["C" "X"] 2   ; Scissors, need to lose -> Paper
                 ["C" "Y"] 3   ; Scissors, need to draw -> Scissors
                 ["C" "Z"] 1}  ; Scissors, need to win -> Rock
        outcome-score {"X" 0 "Y" 3 "Z" 6}]
    (reduce (fn [total [opp outcome]]
              (+ total (choices [opp outcome]) (outcome-score outcome)))
            0
            rounds)))

(defn -main []
  (let [script-dir (System/getProperty "user.dir")
        input-file (str script-dir "/../input.txt")
        rounds (parse-input input-file)]
    (println "Part 1:" (part1 rounds))
    (println "Part 2:" (part2 rounds))))

(-main)
