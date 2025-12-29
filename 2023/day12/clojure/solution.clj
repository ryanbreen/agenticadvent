#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-line
  "Parse a line into [pattern groups] where groups is a vector of integers."
  [line]
  (let [[pattern-str groups-str] (str/split (str/trim line) #"\s+")
        groups (mapv #(Integer/parseInt %) (str/split groups-str #","))]
    [pattern-str groups]))

(defn count-arrangements
  "Count valid arrangements using memoized DP.
   State: [position group-index current-run-length]"
  [pattern groups]
  (let [n (count pattern)
        m (count groups)
        ;; Use atom for memoization cache
        cache (atom {})]
    (letfn [(dp [pos group-idx current-run]
              (if-let [cached (get @cache [pos group-idx current-run])]
                cached
                (let [result
                      (cond
                        ;; Base case: reached end of pattern
                        (= pos n)
                        (cond
                          ;; Valid if all groups matched and no active run
                          (and (= group-idx m) (zero? current-run)) 1
                          ;; Or last group matches current run
                          (and (= group-idx (dec m))
                               (= (groups group-idx) current-run)) 1
                          :else 0)

                        :else
                        (let [ch (nth pattern pos)
                              ;; Option 1: Place operational spring (.)
                              op-result
                              (if (or (= ch \.) (= ch \?))
                                (cond
                                  ;; No active run, just move forward
                                  (zero? current-run)
                                  (dp (inc pos) group-idx 0)

                                  ;; End current run if it matches expected group
                                  (and (< group-idx m)
                                       (= (groups group-idx) current-run))
                                  (dp (inc pos) (inc group-idx) 0)

                                  ;; Invalid: run doesn't match group
                                  :else 0)
                                0)

                              ;; Option 2: Place damaged spring (#)
                              dmg-result
                              (if (or (= ch \#) (= ch \?))
                                (if (and (< group-idx m)
                                         (< current-run (groups group-idx)))
                                  ;; Can extend current run
                                  (dp (inc pos) group-idx (inc current-run))
                                  ;; Invalid: exceeds group size or no more groups
                                  0)
                                0)]
                          (+ op-result dmg-result)))]
                  (swap! cache assoc [pos group-idx current-run] result)
                  result)))]
      (dp 0 0 0))))

(defn unfold
  "Unfold pattern and groups by repeating 5 times."
  [pattern groups]
  (let [unfolded-pattern (str/join "?" (repeat 5 pattern))
        unfolded-groups (vec (apply concat (repeat 5 groups)))]
    [unfolded-pattern unfolded-groups]))

(defn part1
  "Sum of arrangement counts for all rows."
  [lines]
  (reduce + (map (fn [line]
                   (let [[pattern groups] (parse-line line)]
                     (count-arrangements pattern groups)))
                 lines)))

(defn part2
  "Sum of arrangement counts for all rows after unfolding."
  [lines]
  (reduce + (map (fn [line]
                   (let [[pattern groups] (parse-line line)
                         [uf-pattern uf-groups] (unfold pattern groups)]
                     (count-arrangements uf-pattern uf-groups)))
                 lines)))

(defn -main []
  (let [input-file (or (first *command-line-args*) "../input.txt")
        content (slurp input-file)
        lines (vec (remove str/blank? (str/split-lines content)))]
    (println "Part 1:" (part1 lines))
    (println "Part 2:" (part2 lines))))

(-main)
