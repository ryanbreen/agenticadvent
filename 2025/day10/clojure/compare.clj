#!/usr/bin/env clojure

(load-file "solution.clj")

(def input (slurp "../input.txt"))
(def lines (clojure.string/split-lines input))

(println "Computing all machines:")
(def results
  (mapv (fn [[i line]]
          (let [[n-counters joltage buttons] (day10.solution/parse-line-part2 line)
                result (day10.solution/solve-machine-part2 n-counters joltage buttons)]
            result))
        (map-indexed vector lines)))

(println "Results:")
(doseq [[i r] (map-indexed vector results)]
  (println (str "Machine " (inc i) ": " r)))

(println "\nTotal:" (reduce + results))
