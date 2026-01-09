(ns solution
  (:require [clojure.string :as str]))

(defn simulate-cpu
  "Simulate CPU and return a lazy sequence of [cycle x] pairs for each cycle."
  [instructions]
  (letfn [(process [instrs cycle x]
            (lazy-seq
             (when (seq instrs)
               (let [line (first instrs)]
                 (if (= line "noop")
                   (cons [cycle x] (process (rest instrs) (inc cycle) x))
                   ;; addx V: two cycles, x changes after second cycle
                   (let [v (Integer/parseInt (second (str/split line #"\s+")))]
                     (cons [cycle x]
                           (cons [(inc cycle) x]
                                 (process (rest instrs) (+ cycle 2) (+ x v))))))))))]
    (process instructions 1 1)))

(defn part1
  "Sum signal strengths at cycles 20, 60, 100, 140, 180, 220."
  [instructions]
  (let [target-cycles #{20 60 100 140 180 220}]
    (->> (simulate-cpu instructions)
         (filter (fn [[cycle _]] (target-cycles cycle)))
         (map (fn [[cycle x]] (* cycle x)))
         (reduce +))))

(defn part2
  "Render CRT display. Sprite is 3 pixels wide centered at X."
  [instructions]
  (->> (simulate-cpu instructions)
       (take 240)
       (map (fn [[cycle x]]
              (let [pos (mod (dec cycle) 40)]
                (if (<= (abs (- pos x)) 1)
                  \#
                  \.))))
       (partition 40)
       (map #(apply str %))
       (str/join "\n")))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getAbsoluteFile .getParent)
        input-file (str script-dir "/../input.txt")
        instructions (-> (slurp input-file)
                         str/trim
                         (str/split #"\n"))]
    (println "Part 1:" (part1 instructions))
    (println "Part 2:")
    (println (part2 instructions))))

(-main)
