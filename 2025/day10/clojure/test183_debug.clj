#!/usr/bin/env clojure

(load-file "solution.clj")

(def test-line "[..#.#..##.] (1,4,5,7,8) (0) (4,5,6,7,9) (0,4,5,6,7,8) (0,6) (2,3,4,6,7) (1,2,3,4,5,7,8,9) (0,1,4,5,6,7,8) (0,3,7,8) (0,1,2,4,6,7,8,9) (1,4,6,7) (1,2,3,7) (2,4,5,7,9) {76,65,60,52,105,67,71,129,77,42}")

(println "Testing machine 183:")
(let [[n-counters joltage buttons] (day10.solution/parse-line-part2 test-line)]
  (println "n-counters:" n-counters)
  (println "n-buttons:" (count buttons))
  (println "joltage:" joltage)

  ;; Check rank
  (println "\nChecking system...")
  (let [A (vec (for [i (range n-counters)]
                 (vec (for [j (range (count buttons))]
                        (if (some #(= % i) (buttons j)) 1 0)))))
        b (vec joltage)]
    (println "Matrix A dimensions:" (count A) "x" (count (first A)))

    ;; Do Gaussian elimination
    (let [{:keys [aug pivot-cols]} (day10.solution/gaussian-elimination-rref A b)]
      (println "Number of pivot columns:" (count pivot-cols))
      (println "Number of free variables:" (- (count buttons) (count pivot-cols)))
      (println "Pivot columns:" (mapv first pivot-cols)))))
