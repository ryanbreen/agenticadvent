#!/usr/bin/env clojure

(load-file "solution.clj")

(def input (slurp "../input.txt"))
(def lines (clojure.string/split-lines input))

(println "Testing first 5 machines:")
(doseq [[i line] (map-indexed vector (take 5 lines))]
  (let [[n-counters joltage buttons] (day10.solution/parse-line-part2 line)
        result (day10.solution/solve-machine-part2 n-counters joltage buttons)]
    (println (str "Machine " (inc i) ": " result))))

(println "\nTotal for first 5:")
(println (reduce + (map (fn [line]
                          (let [[n-counters joltage buttons] (day10.solution/parse-line-part2 line)]
                            (day10.solution/solve-machine-part2 n-counters joltage buttons)))
                        (take 5 lines))))
