#!/usr/bin/env clojure -M

(require '[clojure.string :as str])
(require '[clojure.edn :as edn])

(defn packet-compare
  "Compare two packet values recursively.
   Returns: negative if left < right (correct order)
            positive if left > right (wrong order)
            0 if equal"
  [left right]
  (cond
    ;; Both integers
    (and (number? left) (number? right))
    (compare left right)

    ;; Both lists
    (and (vector? left) (vector? right))
    (let [len-left (count left)
          len-right (count right)
          min-len (min len-left len-right)]
      (loop [i 0]
        (if (>= i min-len)
          ;; Exhausted elements - compare lengths
          (compare len-left len-right)
          (let [result (packet-compare (nth left i) (nth right i))]
            (if (zero? result)
              (recur (inc i))
              result)))))

    ;; Mixed types - convert integer to list
    (number? left)
    (packet-compare [left] right)

    :else
    (packet-compare left [right])))

(defn parse-packet
  "Parse a single packet line using EDN reader.
   EDN syntax for vectors matches JSON array syntax."
  [line]
  (edn/read-string line))

(defn part1
  "Sum indices of pairs in correct order."
  [text]
  (let [pairs (str/split (str/trim text) #"\n\n")]
    (->> pairs
         (map-indexed
          (fn [idx pair-text]
            (let [lines (str/split-lines pair-text)
                  left (parse-packet (first lines))
                  right (parse-packet (second lines))]
              (if (neg? (packet-compare left right))
                (inc idx)  ; 1-indexed
                0))))
         (reduce +))))

(defn part2
  "Sort all packets with divider packets, find decoder key."
  [text]
  (let [lines (->> (str/split-lines (str/trim text))
                   (filter #(not (str/blank? %))))
        packets (map parse-packet lines)
        divider1 [[2]]
        divider2 [[6]]
        all-packets (concat packets [divider1 divider2])
        sorted-packets (sort packet-compare all-packets)
        ;; Find 1-indexed positions
        pos1 (inc (.indexOf (vec sorted-packets) divider1))
        pos2 (inc (.indexOf (vec sorted-packets) divider2))]
    (* pos1 pos2)))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
