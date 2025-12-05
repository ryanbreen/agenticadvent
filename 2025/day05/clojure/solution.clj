(require '[clojure.string :as str])

;; Read and parse input
(def input-text (slurp "../input.txt"))
(def lines (str/split-lines input-text))

(defn parse-range [line]
  "Parse a range line like '3-5' into [start end]"
  (let [[start end] (str/split line #"-")]
    [(parse-long start) (parse-long end)]))

(defn part1 []
  "Count how many available ingredient IDs fall within any range"
  (let [blank-idx (.indexOf lines "")
        ranges (map parse-range (take blank-idx lines))
        ingredient-ids (map parse-long (filter #(not (str/blank? %)) (drop (inc blank-idx) lines)))]

    ;; Count how many ingredient IDs fall within any range
    (count
      (filter
        (fn [id]
          (some (fn [[start end]]
                  (<= start id end))
                ranges))
        ingredient-ids))))

(defn part2 []
  "Count total unique IDs covered by all ranges (merge overlapping ranges)"
  (let [blank-idx (.indexOf lines "")
        ranges (map parse-range (take blank-idx lines))
        ;; Sort ranges by start position
        sorted-ranges (sort-by first ranges)]

    ;; Merge overlapping or adjacent ranges
    (let [merged (reduce
                   (fn [acc [start end]]
                     (if (and (not-empty acc)
                              (<= start (inc (second (last acc)))))
                       ;; Overlapping or adjacent - merge with last range
                       (let [last-range (last acc)
                             new-range [(first last-range) (max (second last-range) end)]]
                         (conj (vec (butlast acc)) new-range))
                       ;; No overlap - add as new range
                       (conj acc [start end])))
                   []
                   sorted-ranges)]

      ;; Count total unique IDs covered by merged ranges
      (reduce
        (fn [total [start end]]
          (+ total (- end start -1)))
        0
        merged))))

;; Main execution
(println (str "Part 1: " (part1)))
(println (str "Part 2: " (part2)))
