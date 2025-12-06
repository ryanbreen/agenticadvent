(ns solution
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "../input.txt")
      str/trim
      (str/split #"\n")))

(defn char-at [grid row col]
  "Get character at position, returns nil if out of bounds"
  (when (and (>= row 0) (< row (count grid))
             (>= col 0) (< col (count (nth grid row))))
    (get-in grid [row col])))

(defn is-symbol? [ch]
  "Check if character is a symbol (not digit, not period, not nil)"
  (and ch
       (not= ch \.)
       (not (Character/isDigit ch))))

(defn find-numbers [grid]
  "Find all numbers in the grid with their positions.
   Returns a sequence of maps with :value, :row, :start-col, :end-col"
  (for [row (range (count grid))
        :let [line (nth grid row)
              matches (re-seq #"\d+" line)]
        :when (seq matches)]
    (loop [col 0
           remaining matches
           results []]
      (if (empty? remaining)
        results
        (let [num-str (first remaining)
              num-val (parse-long num-str)
              idx (str/index-of line num-str col)]
          (recur (+ idx (count num-str))
                 (rest remaining)
                 (conj results {:value num-val
                               :row row
                               :start-col idx
                               :end-col (+ idx (count num-str) -1)})))))))

(defn get-adjacent-positions [num-info]
  "Get all adjacent positions (including diagonals) for a number"
  (let [{:keys [row start-col end-col]} num-info]
    (for [r (range (dec row) (+ row 2))
          c (range (dec start-col) (+ end-col 2))
          :when (not (and (= r row)
                         (>= c start-col)
                         (<= c end-col)))]
      [r c])))

(defn adjacent-to-symbol? [grid num-info]
  "Check if a number is adjacent to any symbol"
  (some (fn [[r c]]
          (is-symbol? (char-at grid r c)))
        (get-adjacent-positions num-info)))

(defn part1 []
  (let [numbers (flatten (find-numbers input))
        part-numbers (filter #(adjacent-to-symbol? input %) numbers)]
    (reduce + 0 (map :value part-numbers))))

(defn find-gears [grid]
  "Find all * symbols and their adjacent numbers.
   Returns gear ratios for gears with exactly 2 adjacent numbers."
  (let [numbers (flatten (find-numbers grid))
        gear-positions (for [row (range (count grid))
                            col (range (count (nth grid row)))
                            :when (= (char-at grid row col) \*)]
                        [row col])]
    (for [[gr gc] gear-positions
          :let [adjacent-numbers
                (filter (fn [num-info]
                         (some (fn [[r c]] (and (= r gr) (= c gc)))
                               (get-adjacent-positions num-info)))
                       numbers)]
          :when (= (count adjacent-numbers) 2)]
      (apply * (map :value adjacent-numbers)))))

(defn part2 []
  (reduce + 0 (find-gears input)))

;; Main execution
(println "Part 1:" (part1))
(println "Part 2:" (part2))
