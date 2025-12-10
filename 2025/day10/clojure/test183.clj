#!/usr/bin/env clojure

(load-file "solution.clj")

(def test-line "[..#.#..##.] (1,4,5,7,8) (0) (4,5,6,7,9) (0,4,5,6,7,8) (0,6) (2,3,4,6,7) (1,2,3,4,5,7,8,9) (0,1,4,5,6,7,8) (0,3,7,8) (0,1,2,4,6,7,8,9) (1,4,6,7) (1,2,3,7) (2,4,5,7,9) {76,65,60,52,105,67,71,129,77,42}")

(println "Testing machine 183:")
(let [[n-counters joltage buttons] (day10.solution/parse-line-part2 test-line)]
  (println "n-counters:" n-counters)
  (println "joltage:" joltage)
  (println "buttons:" buttons)
  (println "Result:" (day10.solution/solve-machine-part2 n-counters joltage buttons))
  (println "Expected: 140"))
