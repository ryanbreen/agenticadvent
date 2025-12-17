#!/usr/bin/env clojure -M

;; Day 17: Chronospatial Computer - 3-bit VM emulator

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input
  "Parse registers and program from input."
  [text]
  (let [[line-a line-b line-c _ line-prog] (str/split-lines (str/trim text))
        a (parse-long (second (re-find #": (\d+)" line-a)))
        b (parse-long (second (re-find #": (\d+)" line-b)))
        c (parse-long (second (re-find #": (\d+)" line-c)))
        program-str (second (re-find #"Program: ([\d,]+)" line-prog))
        program (vec (map parse-long (str/split program-str #",")))]
    {:a a :b b :c c :program program}))

(defn combo-operand
  "Get combo operand value."
  [operand a b c]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 a
    5 b
    6 c
    (throw (ex-info "Invalid combo operand" {:operand operand}))))

(defn run-program
  "Execute the 3-bit computer program and return output."
  [a b c program]
  (let [program-len (count program)]
    (loop [ip 0
           a a
           b b
           c c
           output []]
      (if (>= ip program-len)
        output
        (let [opcode (nth program ip)
              operand (nth program (inc ip))]
          (case opcode
            ;; adv - A = A >> combo
            0 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) (bit-shift-right a shift) b c output))

            ;; bxl - B = B XOR literal
            1 (recur (+ ip 2) a (bit-xor b operand) c output)

            ;; bst - B = combo % 8
            2 (let [val (bit-and (combo-operand operand a b c) 7)]
                (recur (+ ip 2) a val c output))

            ;; jnz - jump if A != 0
            3 (if (not= a 0)
                (recur operand a b c output)
                (recur (+ ip 2) a b c output))

            ;; bxc - B = B XOR C (ignores operand)
            4 (recur (+ ip 2) a (bit-xor b c) c output)

            ;; out - output combo % 8
            5 (let [val (bit-and (combo-operand operand a b c) 7)]
                (recur (+ ip 2) a b c (conj output val)))

            ;; bdv - B = A >> combo
            6 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) a (bit-shift-right a shift) c output))

            ;; cdv - C = A >> combo
            7 (let [shift (combo-operand operand a b c)]
                (recur (+ ip 2) a b (bit-shift-right a shift) output))))))))

(defn part1
  "Run the program and return comma-separated output."
  [text]
  (let [{:keys [a b c program]} (parse-input text)
        output (run-program a b c program)]
    (str/join "," output)))

(defn part2
  "Find initial A value that makes program output itself."
  [text]
  (let [{:keys [b c program]} (parse-input text)
        program-len (count program)]

    ;; Work backwards from the last digit - build A 3 bits at a time
    (letfn [(search [target-idx current-a]
              (if (< target-idx 0)
                current-a
                ;; Try all 8 possible 3-bit values for this position
                (some (fn [bits]
                        (let [candidate-a (bit-or (bit-shift-left current-a 3) bits)]
                          ;; A can't be 0 at start (would halt immediately without output)
                          (when-not (and (= candidate-a 0) (= target-idx (dec program-len)))
                            (let [output (run-program candidate-a b c program)
                                  expected (subvec program target-idx)]
                              (when (= output expected)
                                (search (dec target-idx) candidate-a))))))
                      (range 8))))]
      (search (dec program-len) 0))))

(defn -main []
  (let [text (slurp "../input.txt")]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
