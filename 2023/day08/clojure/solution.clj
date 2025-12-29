#!/usr/bin/env bb
;; Advent of Code 2023 - Day 8: Haunted Wasteland

(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(defn parse-input
  "Parse the input into instructions and network map."
  [text]
  (let [lines (str/split-lines (str/trim text))
        instructions (first lines)
        network (->> (drop 2 lines)
                     (remove str/blank?)
                     (map (fn [line]
                            (let [[node rest] (str/split line #" = ")
                                  [left right] (-> rest
                                                   (subs 1 (dec (count rest)))
                                                   (str/split #", "))]
                              [node [left right]])))
                     (into {}))]
    [instructions network]))

(defn follow-instructions
  "Follow instructions from start until end condition is met."
  [instructions network start end-pred]
  (loop [current start
         steps 0
         instrs (cycle instructions)]
    (if (end-pred current)
      steps
      (let [[left right] (network current)
            next-node (if (= (first instrs) \L) left right)]
        (recur next-node (inc steps) (rest instrs))))))

(defn part1
  "Navigate from AAA to ZZZ following L/R instructions."
  [instructions network]
  (follow-instructions instructions network "AAA" #(= % "ZZZ")))

(defn gcd
  "Greatest common divisor."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Least common multiple."
  [a b]
  (/ (* a b) (gcd a b)))

(defn part2
  "Navigate all nodes ending in A simultaneously to nodes ending in Z."
  [instructions network]
  (let [start-nodes (filter #(str/ends-with? % "A") (keys network))
        cycle-lengths (map #(follow-instructions instructions network % (fn [s] (str/ends-with? s "Z")))
                           start-nodes)]
    (reduce lcm 1 cycle-lengths)))

(defn -main []
  (let [input-file (io/file (System/getProperty "user.dir") ".." "input.txt")
        text (slurp input-file)
        [instructions network] (parse-input text)]
    (println "Part 1:" (part1 instructions network))
    (println "Part 2:" (part2 instructions network))))

(-main)
