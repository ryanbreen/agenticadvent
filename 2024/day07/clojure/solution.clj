#!/usr/bin/env clojure

(require '[clojure.string :as str])

(defn parse-input [text]
  "Parse input into sequence of [target [nums...]] pairs"
  (->> (str/split-lines text)
       (map (fn [line]
              (let [[target nums-str] (str/split line #": ")]
                [(parse-long target)
                 (mapv parse-long (str/split nums-str #" "))])))))

(defn evaluate [nums ops]
  "Evaluate nums with ops left-to-right"
  (loop [result (first nums)
         i 0]
    (if (>= i (count ops))
      result
      (let [op (nth ops i)
            next-num (nth nums (inc i))]
        (recur (case op
                 :+ (+ result next-num)
                 :* (* result next-num)
                 :|| (parse-long (str result next-num)))
               (inc i))))))

(defn all-operator-combinations [operators n]
  "Generate all combinations of operators of length n"
  (if (zero? n)
    [[]]
    (for [op operators
          rest (all-operator-combinations operators (dec n))]
      (cons op rest))))

(defn can-make-target? [target nums operators]
  "Check if any combination of operators can produce target"
  (let [n-ops (dec (count nums))]
    (some #(= target (evaluate nums %))
          (all-operator-combinations operators n-ops))))

(defn part1 [equations]
  "Sum targets that can be made with + and *"
  (let [operators [:+ :*]]
    (->> equations
         (filter (fn [[target nums]] (can-make-target? target nums operators)))
         (map first)
         (reduce +))))

(defn part2 [equations]
  "Sum targets that can be made with +, *, and ||"
  (let [operators [:+ :* :||]]
    (->> equations
         (filter (fn [[target nums]] (can-make-target? target nums operators)))
         (map first)
         (reduce +))))

(defn -main []
  (let [input-text (slurp "../input.txt")
        equations (parse-input input-text)]
    (println "Part 1:" (part1 equations))
    (println "Part 2:" (part2 equations))))

(-main)
