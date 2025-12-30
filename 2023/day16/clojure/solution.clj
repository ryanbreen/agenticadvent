#!/usr/bin/env -S clojure -M

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

;; Directions: 0=right, 1=down, 2=left, 3=up
(def dr [0 1 0 -1])
(def dc [1 0 -1 0])

;; Reflection maps
(def slash-reflect  {0 3, 1 2, 2 1, 3 0})  ; / mirror
(def bslash-reflect {0 1, 1 0, 2 3, 3 2})  ; \ mirror

(defn parse-input [text]
  (vec (str/split-lines (str/trim text))))

(defn get-next-dirs [cell dir]
  (case cell
    \. [dir]
    \/ [(slash-reflect dir)]
    \\ [(bslash-reflect dir)]
    \| (if (#{0 2} dir) [1 3] [dir])
    \- (if (#{1 3} dir) [0 2] [dir])))

(defn count-energized [grid start-row start-col start-dir]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start-row start-col start-dir])
           visited #{}]
      (if (empty? queue)
        ;; Count unique positions
        (count (set (map (fn [[r c _]] [r c]) visited)))
        (let [[r c d] (peek queue)
              rest-queue (pop queue)]
          (if (or (< r 0) (>= r rows) (< c 0) (>= c cols))
            (recur rest-queue visited)
            (let [state [r c d]]
              (if (visited state)
                (recur rest-queue visited)
                (let [cell (get-in grid [r c])
                      next-dirs (get-next-dirs cell d)
                      new-states (map (fn [nd]
                                        [(+ r (dr nd)) (+ c (dc nd)) nd])
                                      next-dirs)]
                  (recur (reduce conj rest-queue new-states)
                         (conj visited state)))))))))))

(defn part1 [grid]
  (count-energized grid 0 0 0))

(defn part2 [grid]
  (let [rows (count grid)
        cols (count (first grid))
        ;; All starting positions and directions
        top-row (for [c (range cols)] [0 c 1])           ; heading down
        bottom-row (for [c (range cols)] [(dec rows) c 3]) ; heading up
        left-col (for [r (range rows)] [r 0 0])          ; heading right
        right-col (for [r (range rows)] [r (dec cols) 2]) ; heading left
        all-starts (concat top-row bottom-row left-col right-col)]
    (->> all-starts
         (map (fn [[r c d]] (count-energized grid r c d)))
         (apply max))))

(defn -main []
  (let [input-file (io/file (System/getProperty "user.dir") "../input.txt")
        text (slurp input-file)
        grid (parse-input text)]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
