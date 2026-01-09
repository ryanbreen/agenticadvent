#!/usr/bin/env bb

(require '[clojure.string :as str])

(defn parse-stacks [stack-section]
  "Parse the visual crate diagram into vectors of crates (bottom to top)"
  (let [lines (str/split-lines stack-section)
        ;; Last line has stack numbers - count them
        num-stacks (count (str/split (str/trim (last lines)) #"\s+"))
        ;; Process all lines except the last (numbers line)
        crate-lines (butlast lines)]
    ;; For each stack position, collect crates from top to bottom, then reverse
    (->> (range num-stacks)
         (mapv (fn [stack-idx]
                 (let [pos (+ 1 (* stack-idx 4))] ; Position of crate letter
                   (->> crate-lines
                        (keep (fn [line]
                                (when (< pos (count line))
                                  (let [c (nth line pos)]
                                    (when (not= c \space)
                                      c)))))
                        reverse
                        vec)))))))

(defn parse-moves [move-section]
  "Parse move instructions into [count from-stack to-stack] tuples (0-indexed)"
  (->> (str/split-lines move-section)
       (keep (fn [line]
               (when-let [match (re-find #"move (\d+) from (\d+) to (\d+)" line)]
                 [(parse-long (nth match 1))
                  (dec (parse-long (nth match 2)))  ; 0-indexed
                  (dec (parse-long (nth match 3)))])))))

(defn parse-input [filename]
  (let [content (slurp filename)
        [stack-section move-section] (str/split content #"\n\n")]
    [(parse-stacks stack-section)
     (parse-moves move-section)]))

(defn part1 [stacks moves]
  "Move crates one at a time (reverses order)"
  (let [result-stacks
        (reduce (fn [stacks [n from-stack to-stack]]
                  ;; Move 'n' crates one at a time
                  (loop [stacks stacks
                         remaining n]
                    (if (zero? remaining)
                      stacks
                      (let [crate (peek (nth stacks from-stack))]
                        (recur (-> stacks
                                   (update from-stack pop)
                                   (update to-stack conj crate))
                               (dec remaining))))))
                stacks
                moves)]
    (apply str (map peek result-stacks))))

(defn part2 [stacks moves]
  "Move multiple crates at once (preserves order)"
  (let [result-stacks
        (reduce (fn [stacks [n from-stack to-stack]]
                  (let [from-vec (nth stacks from-stack)
                        split-point (- (count from-vec) n)
                        crates-to-move (subvec from-vec split-point)
                        remaining (subvec from-vec 0 split-point)]
                    (-> stacks
                        (assoc from-stack remaining)
                        (update to-stack into crates-to-move))))
                stacks
                moves)]
    (apply str (map peek result-stacks))))

(defn -main []
  (let [script-dir (-> *file* java.io.File. .getParent)
        input-file (str script-dir "/../input.txt")
        [stacks moves] (parse-input input-file)]
    (println "Part 1:" (part1 stacks moves))
    (println "Part 2:" (part2 stacks moves))))

(-main)
