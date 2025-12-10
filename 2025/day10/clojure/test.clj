#!/usr/bin/env clojure

(load-file "solution.clj")

;; Test with first example line from problem
(def test-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

(println "Testing Part 1:")
(let [[n-lights target buttons] (day10.solution/parse-line-part1 test-line)]
  (println "n-lights:" n-lights "target:" target "buttons:" buttons)
  (println "Result:" (day10.solution/solve-machine-part1 n-lights target buttons))
  (println "Expected: 2"))

(println "\nTesting Part 2:")
(let [[n-counters joltage buttons] (day10.solution/parse-line-part2 test-line)]
  (println "n-counters:" n-counters "joltage:" joltage "buttons:" buttons)
  (println "Result:" (day10.solution/solve-machine-part2 n-counters joltage buttons))
  (println "Expected: 10"))
