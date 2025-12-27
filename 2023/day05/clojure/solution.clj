(ns solution
  (:require [clojure.string :as str]))

(def input (slurp "../input.txt"))

(defn parse-long-safe [s]
  "Parse a string to long, returning nil for empty strings"
  (when-not (str/blank? s)
    (parse-long s)))

(defn parse-seeds [line]
  "Parse the seeds line into a list of seed numbers"
  (->> (str/split (second (str/split line #": ")) #"\s+")
       (map parse-long-safe)
       (remove nil?)))

(defn parse-map-line [line]
  "Parse a map line into [dst-start src-start length]"
  (->> (str/split (str/trim line) #"\s+")
       (map parse-long)
       vec))

(defn parse-map-section [section]
  "Parse a map section into a vector of ranges"
  (let [lines (str/split-lines section)]
    (->> (rest lines)  ; Skip header line
         (map parse-map-line)
         vec)))

(defn parse-input [text]
  "Parse input into [seeds maps]"
  (let [sections (str/split (str/trim text) #"\n\n")
        seeds (parse-seeds (first sections))
        maps (mapv parse-map-section (rest sections))]
    [seeds maps]))

(defn apply-map-value [value ranges]
  "Apply a single map to transform a value"
  (if-let [match (first (filter (fn [[dst-start src-start length]]
                                   (and (>= value src-start)
                                        (< value (+ src-start length))))
                                 ranges))]
    (let [[dst-start src-start _] match]
      (+ dst-start (- value src-start)))
    value))

(defn seed-to-location [seed maps]
  "Convert a seed number to a location number through all maps"
  (reduce apply-map-value seed maps))

(defn part1 [seeds maps]
  "Find the lowest location number for any initial seed"
  (apply min (map #(seed-to-location % maps) seeds)))

(defn apply-map-to-ranges [input-ranges map-ranges]
  "Apply a map to a list of ranges, returning new ranges"
  (loop [remaining-inputs input-ranges
         mapped []
         unmapped []]
    (if (empty? remaining-inputs)
      ;; After all input ranges processed, remaining unmapped go through as identity
      (concat mapped unmapped)
      (let [[r-start r-end] (first remaining-inputs)
            rest-inputs (rest remaining-inputs)]
        ;; For this input range, try each map range
        (let [[new-mapped new-unmapped]
              (loop [remaining-maps map-ranges
                     current-remaining [[r-start r-end]]
                     result-mapped []]
                (if (or (empty? remaining-maps) (empty? current-remaining))
                  [result-mapped current-remaining]
                  (let [[dst-start src-start length] (first remaining-maps)
                        src-end (+ src-start length)
                        rest-maps (rest remaining-maps)
                        ;; Process all current remaining through this map range
                        processed
                        (for [[rs re] current-remaining]
                          (let [;; Part before map range (stays unmapped for now)
                                before (when (< rs src-start)
                                         [rs (min re src-start)])
                                ;; Overlap with map range (gets mapped)
                                overlap-start (max rs src-start)
                                overlap-end (min re src-end)
                                overlap (when (< overlap-start overlap-end)
                                          (let [offset (- dst-start src-start)]
                                            [(+ overlap-start offset)
                                             (+ overlap-end offset)]))
                                ;; Part after map range (stays unmapped for now)
                                after (when (> re src-end)
                                        [(max rs src-end) re])]
                            {:before before :overlap overlap :after after}))
                        ;; Collect mapped overlaps
                        new-results (->> processed
                                         (map :overlap)
                                         (remove nil?))
                        ;; Collect remaining unmapped (before + after)
                        new-remaining (->> processed
                                           (mapcat (fn [p] [(:before p) (:after p)]))
                                           (remove nil?))]
                    (recur rest-maps
                           new-remaining
                           (concat result-mapped new-results)))))]
          (recur rest-inputs
                 (concat mapped new-mapped)
                 (concat unmapped new-unmapped)))))))

(defn part2 [seeds maps]
  "Find the lowest location for seed ranges"
  ;; Convert seeds to ranges: pairs of [start, start + length)
  (let [seed-ranges (for [pair (partition 2 seeds)
                          :let [[start length] pair]]
                      [start (+ start length)])
        ;; Apply each map to the ranges
        final-ranges (reduce apply-map-to-ranges seed-ranges maps)]
    ;; Find minimum start of any range
    (apply min (map first final-ranges))))

;; Main execution
(let [[seeds maps] (parse-input input)]
  (println "Part 1:" (part1 seeds maps))
  (println "Part 2:" (part2 seeds maps)))
