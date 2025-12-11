#!/usr/bin/env clojure
;;
;; Advent of Code 2024 Day 9: Disk Fragmenter
;;
;; Compact a fragmented disk by moving file blocks to fill gaps.
;; Part 1: Move blocks one at a time from end to leftmost free space
;; Part 2: Move whole files to leftmost span that fits

(require '[clojure.string :as str])

(defn parse-disk-map
  "Parse disk map into expanded block representation.
   Returns vector where each element is file ID or -1 for free space."
  [filename]
  (let [disk-map (str/trim (slurp filename))
        parse-step (fn [[blocks file-id is-file] digit]
                     (let [length (Integer/parseInt (str digit))]
                       (if is-file
                         [(into blocks (repeat length file-id))
                          (inc file-id)
                          false]
                         [(into blocks (repeat length -1))
                          file-id
                          true])))]
    (first (reduce parse-step [[] 0 true] disk-map))))

(defn compact-blocks
  "Compact disk by moving blocks one at a time from end to leftmost free space."
  [blocks]
  (loop [result (vec blocks)
         left 0
         right (dec (count blocks))]
    (cond
      (>= left right) result

      ;; Find leftmost free space
      (not= (nth result left) -1)
      (recur result (inc left) right)

      ;; Find rightmost file block
      (= (nth result right) -1)
      (recur result left (dec right))

      ;; Swap
      :else
      (recur (-> result
                 (assoc left (nth result right))
                 (assoc right -1))
             (inc left)
             (dec right)))))

(defn calculate-checksum
  "Calculate filesystem checksum: sum of position * file_id for each block."
  [blocks]
  (reduce + 0
          (keep-indexed (fn [pos file-id]
                          (when (not= file-id -1)
                            (* pos file-id)))
                        blocks)))

(defn find-files
  "Find all files and return map of file_id -> [start-pos length]."
  [blocks]
  (loop [i 0
         files {}]
    (if (>= i (count blocks))
      files
      (if (not= (nth blocks i) -1)
        (let [file-id (nth blocks i)
              start i
              end (loop [j i]
                    (if (and (< j (count blocks))
                             (= (nth blocks j) file-id))
                      (recur (inc j))
                      j))
              length (- end start)]
          (recur end (assoc files file-id [start length])))
        (recur (inc i) files)))))

(defn find-leftmost-free-span
  "Find leftmost span of free space that fits the given length.
   Must be before the given start position.
   Returns start position of span or nil if none found."
  [blocks length start]
  (loop [i 0]
    (cond
      (>= i start) nil

      (= (nth blocks i) -1)
      (let [span-start i
            span-end (loop [j i]
                       (if (and (< j start)
                                (= (nth blocks j) -1))
                         (recur (inc j))
                         j))
            span-length (- span-end span-start)]
        (if (>= span-length length)
          span-start
          (recur span-end)))

      :else
      (recur (inc i)))))

(defn move-file
  "Move a file from its current position to a new position."
  [blocks file-id old-start length new-start]
  (-> blocks
      ;; Clear old position
      (as-> b (reduce (fn [blks j] (assoc blks j -1))
                      b
                      (range old-start (+ old-start length))))
      ;; Write to new position
      (as-> b (reduce (fn [blks j] (assoc blks j file-id))
                      b
                      (range new-start (+ new-start length))))))

(defn compact-whole-files
  "Compact disk by moving whole files (highest ID first) to leftmost span that fits."
  [blocks]
  (let [files (find-files blocks)
        max-file-id (apply max (keys files))]
    (loop [result (vec blocks)
           file-positions files
           file-id max-file-id]
      (if (< file-id 0)
        result
        (let [[start length] (get file-positions file-id)
              free-start (find-leftmost-free-span result length start)]
          (if free-start
            (recur (move-file result file-id start length free-start)
                   (assoc file-positions file-id [free-start length])
                   (dec file-id))
            (recur result file-positions (dec file-id))))))))

(defn part1
  "Compact by moving individual blocks, return checksum."
  []
  (let [blocks (parse-disk-map "../input.txt")
        compacted (compact-blocks blocks)]
    (calculate-checksum compacted)))

(defn part2
  "Compact by moving whole files (highest ID first), return checksum."
  []
  (let [blocks (parse-disk-map "../input.txt")
        compacted (compact-whole-files blocks)]
    (calculate-checksum compacted)))

(println (str "Part 1: " (part1)))
(println (str "Part 2: " (part2)))
