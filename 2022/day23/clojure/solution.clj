#!/usr/bin/env bb

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [text]
  "Parse elf positions from grid into a set of [row col] coordinates."
  (into #{}
        (for [[r line] (map-indexed vector (str/split-lines (str/trim text)))
              [c ch] (map-indexed vector line)
              :when (= ch \#)]
          [r c])))

(def dir-checks
  "Direction checks: direction -> [positions-to-check, move-delta]"
  {:N {:checks [[-1 -1] [-1 0] [-1 1]] :delta [-1 0]}
   :S {:checks [[1 -1] [1 0] [1 1]] :delta [1 0]}
   :W {:checks [[-1 -1] [0 -1] [1 -1]] :delta [0 -1]}
   :E {:checks [[-1 1] [0 1] [1 1]] :delta [0 1]}})

(def all-neighbors
  "All 8 neighbor offsets."
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn has-neighbor? [elves [r c]]
  "Check if elf at [r c] has any neighboring elves."
  (some (fn [[dr dc]]
          (contains? elves [(+ r dr) (+ c dc)]))
        all-neighbors))

(defn can-move-dir? [elves [r c] dir]
  "Check if elf can move in given direction (no elves in check positions)."
  (let [{:keys [checks]} (dir-checks dir)]
    (not-any? (fn [[dr dc]]
                (contains? elves [(+ r dr) (+ c dc)]))
              checks)))

(defn propose-move [elves elf directions]
  "Propose a move for an elf. Returns new position or nil if no move."
  (when (has-neighbor? elves elf)
    (let [[r c] elf]
      (some (fn [dir]
              (when (can-move-dir? elves elf dir)
                (let [{:keys [delta]} (dir-checks dir)
                      [dr dc] delta]
                  [(+ r dr) (+ c dc)])))
            directions))))

(defn simulate-round [elves directions]
  "Run one round of simulation. Returns [new-elves moved?]."
  ;; Phase 1: Each elf proposes a move
  (let [proposals (into {}
                        (for [elf elves
                              :let [proposed (propose-move elves elf directions)]
                              :when proposed]
                          [elf proposed]))
        proposal-counts (frequencies (vals proposals))]

    ;; Phase 2: Execute moves (only if unique proposal)
    (let [new-elves (into #{}
                          (for [elf elves]
                            (if-let [proposed (proposals elf)]
                              (if (= 1 (proposal-counts proposed))
                                proposed
                                elf)
                              elf)))
          moved? (not= elves new-elves)]
      [new-elves moved?])))

(defn rotate-directions [directions]
  "Rotate directions: first goes to end."
  (conj (subvec directions 1) (first directions)))

(defn bounding-rect-empty [elves]
  "Count empty tiles in bounding rectangle."
  (let [rows (map first elves)
        cols (map second elves)
        min-r (apply min rows)
        max-r (apply max rows)
        min-c (apply min cols)
        max-c (apply max cols)
        area (* (inc (- max-r min-r)) (inc (- max-c min-c)))]
    (- area (count elves))))

(defn part1 [text]
  "Count empty tiles after 10 rounds."
  (loop [elves (parse-input text)
         directions [:N :S :W :E]
         round 0]
    (if (= round 10)
      (bounding-rect-empty elves)
      (let [[new-elves _] (simulate-round elves directions)]
        (recur new-elves (rotate-directions directions) (inc round))))))

(defn part2 [text]
  "Find first round where no elf moves."
  (loop [elves (parse-input text)
         directions [:N :S :W :E]
         round 1]
    (let [[new-elves moved?] (simulate-round elves directions)]
      (if (not moved?)
        round
        (recur new-elves (rotate-directions directions) (inc round))))))

(defn -main []
  (let [script-dir (-> *file* java.io.File. .getParent)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
