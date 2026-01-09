#!/usr/bin/env bb

(ns solution
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[name job] (str/split line #": ")
        parts (str/split job #" ")]
    (if (= 1 (count parts))
      [name (parse-long (first parts))]
      [name [(first parts) (second parts) (nth parts 2)]])))

(defn parse-input [text]
  (->> (str/split-lines (str/trim text))
       (map parse-line)
       (into {})))

(defn evaluate
  "Recursively evaluate a monkey's value."
  [monkeys name memo]
  (if-let [cached (get @memo name)]
    cached
    (let [job (get monkeys name)
          result (if (number? job)
                   job
                   (let [[left op right] job
                         left-val (evaluate monkeys left memo)
                         right-val (evaluate monkeys right memo)]
                     (case op
                       "+" (+ left-val right-val)
                       "-" (- left-val right-val)
                       "*" (* left-val right-val)
                       "/" (quot left-val right-val))))]
      (swap! memo assoc name result)
      result)))

(defn contains-humn?
  "Check if evaluation tree contains 'humn'."
  [monkeys name memo]
  (if (contains? @memo name)
    (get @memo name)
    (let [result (cond
                   (= name "humn") true
                   (number? (get monkeys name)) false
                   :else (let [[left _ right] (get monkeys name)]
                           (or (contains-humn? monkeys left memo)
                               (contains-humn? monkeys right memo))))]
      (swap! memo assoc name result)
      result)))

(defn solve-for-humn
  "Given that 'name' should equal 'target', find what 'humn' should be."
  [monkeys name target humn-memo]
  (if (= name "humn")
    target
    (let [job (get monkeys name)]
      (when (vector? job)
        (let [[left op right] job
              left-has-humn? (contains-humn? monkeys left humn-memo)]
          (if left-has-humn?
            ;; Solve for left, evaluate right
            (let [right-val (evaluate monkeys right (atom {}))
                  new-target (case op
                               "+" (- target right-val)
                               "-" (+ target right-val)
                               "*" (quot target right-val)
                               "/" (* target right-val))]
              (solve-for-humn monkeys left new-target humn-memo))
            ;; Solve for right, evaluate left
            (let [left-val (evaluate monkeys left (atom {}))
                  new-target (case op
                               "+" (- target left-val)
                               "-" (- left-val target)
                               "*" (quot target left-val)
                               "/" (quot left-val target))]
              (solve-for-humn monkeys right new-target humn-memo))))))))

(defn part1 [text]
  (let [monkeys (parse-input text)]
    (evaluate monkeys "root" (atom {}))))

(defn part2 [text]
  (let [monkeys (parse-input text)
        [left _ right] (get monkeys "root")
        humn-memo (atom {})
        left-has-humn? (contains-humn? monkeys left humn-memo)]
    (if left-has-humn?
      (let [target (evaluate monkeys right (atom {}))]
        (solve-for-humn monkeys left target humn-memo))
      (let [target (evaluate monkeys left (atom {}))]
        (solve-for-humn monkeys right target humn-memo)))))

(defn -main []
  (let [script-dir (-> *file*
                       (java.io.File.)
                       (.getParentFile)
                       (.getAbsolutePath))
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
