#!/usr/bin/env bb
;;; Day 25: Code Chronicle - Lock and key matching

(require '[clojure.string :as str])

(defn parse-schematic [schematic]
  "Parse a single schematic into either a lock or key with heights."
  (let [lines (str/split-lines schematic)
        top-row (first lines)]
    (if (= top-row "#####")
      ;; Lock: count # from top (excluding top row)
      [:lock
       (vec (for [col (range 5)]
              (loop [row 1
                     height 0]
                (if (and (< row 7) (= (get (nth lines row) col) \#))
                  (recur (inc row) (inc height))
                  height))))]
      ;; Key: count # from bottom (excluding bottom row)
      [:key
       (vec (for [col (range 5)]
              (loop [row 5
                     height 0]
                (if (and (>= row 0) (= (get (nth lines row) col) \#))
                  (recur (dec row) (inc height))
                  height))))])))

(defn parse-input [text]
  "Parse input text into locks and keys."
  (let [schematics (str/split (str/trim text) #"\n\n")
        parsed (map parse-schematic schematics)
        locks (mapv second (filter #(= (first %) :lock) parsed))
        keys (mapv second (filter #(= (first %) :key) parsed))]
    [locks keys]))

(defn fits? [lock key]
  "Check if a key fits a lock (no column sum exceeds 5)."
  (every? #(<= % 5) (map + lock key)))

(defn part1 [locks keys]
  "Count unique lock/key pairs that fit together."
  (count
    (for [lock locks
          key keys
          :when (fits? lock key)]
      [lock key])))

(defn main []
  (let [input-file (str (System/getProperty "user.dir") "/../input.txt")
        text (slurp input-file)
        [locks keys] (parse-input text)
        answer1 (part1 locks keys)]
    (println (str "Part 1: " answer1))
    (println "Part 2: Merry Christmas!")))

(main)
