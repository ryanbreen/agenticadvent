#!/usr/bin/env clojure

(require '[clojure.string :as str])

(defn parse-input
  "Parse input into list of patterns (each pattern is a vector of strings)."
  [text]
  (->> (str/split (str/trim text) #"\n\n")
       (mapv str/split-lines)))

(defn count-differences
  "Count character differences between two strings."
  [s1 s2]
  (->> (map not= s1 s2)
       (filter identity)
       count))

(defn find-vertical-reflection
  "Find vertical line of reflection. Returns columns to the left, or 0 if none.
   When smudge-count is 0, looks for perfect reflection.
   When smudge-count is 1, looks for reflection with exactly one difference."
  [pattern smudge-count]
  (let [width (count (first pattern))]
    (loop [col 1]
      (if (>= col width)
        0
        (let [total-diff (reduce
                          (fn [acc row]
                            (let [left (apply str (reverse (subs row 0 col)))
                                  right (subs row col)
                                  min-len (min (count left) (count right))
                                  diff (count-differences (subs left 0 min-len)
                                                         (subs right 0 min-len))
                                  new-acc (+ acc diff)]
                              (if (> new-acc smudge-count)
                                (reduced (inc smudge-count)) ; short-circuit
                                new-acc)))
                          0
                          pattern)]
          (if (= total-diff smudge-count)
            col
            (recur (inc col))))))))

(defn find-horizontal-reflection
  "Find horizontal line of reflection. Returns rows above, or 0 if none.
   When smudge-count is 0, looks for perfect reflection.
   When smudge-count is 1, looks for reflection with exactly one difference."
  [pattern smudge-count]
  (let [height (count pattern)]
    (loop [row 1]
      (if (>= row height)
        0
        (let [top (reverse (take row pattern))
              bottom (drop row pattern)
              min-len (min (count top) (count bottom))
              total-diff (reduce
                          (fn [acc i]
                            (let [diff (count-differences (nth top i) (nth bottom i))
                                  new-acc (+ acc diff)]
                              (if (> new-acc smudge-count)
                                (reduced (inc smudge-count)) ; short-circuit
                                new-acc)))
                          0
                          (range min-len))]
          (if (= total-diff smudge-count)
            row
            (recur (inc row))))))))

(defn summarize-pattern
  "Get the summary value for a pattern with optional smudge count."
  [pattern smudge-count]
  (let [v (find-vertical-reflection pattern smudge-count)]
    (if (> v 0)
      v
      (* 100 (find-horizontal-reflection pattern smudge-count)))))

(defn part1
  "Calculate the sum of all pattern summaries."
  [patterns]
  (reduce + (map #(summarize-pattern % 0) patterns)))

(defn part2
  "Calculate the sum with smudge fixes."
  [patterns]
  (reduce + (map #(summarize-pattern % 1) patterns)))

(defn -main []
  (let [input-file (str (System/getProperty "user.dir") "/../input.txt")
        text (slurp input-file)
        patterns (parse-input text)]
    (println "Part 1:" (part1 patterns))
    (println "Part 2:" (part2 patterns))))

(-main)
