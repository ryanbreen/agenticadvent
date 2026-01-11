(ns solution
  (:require [clojure.string :as str]))

(defn parse-input []
  (let [input-path (str (System/getProperty "user.dir") "/../input.txt")
        content (str/trim (slurp input-path))
        sections (str/split content #"\n\n")
        numbers (mapv #(Integer/parseInt %) (str/split (first sections) #","))
        boards (mapv (fn [section]
                       (mapv (fn [line]
                               (mapv #(Integer/parseInt %)
                                     (str/split (str/trim line) #"\s+")))
                             (str/split-lines (str/trim section))))
                     (rest sections))]
    [numbers boards]))

(defn check-winner [marked]
  (or
   ;; Check rows
   (some (fn [row] (every? true? row)) marked)
   ;; Check columns
   (some (fn [col] (every? #(nth % col) marked)) (range 5))))

(defn calculate-score [board marked last-number]
  (let [unmarked-sum (reduce +
                             (for [row (range 5)
                                   col (range 5)
                                   :when (not (get-in marked [row col]))]
                               (get-in board [row col])))]
    (* unmarked-sum last-number)))

(defn mark-number [board marked number]
  (reduce (fn [m [row col]]
            (if (= (get-in board [row col]) number)
              (assoc-in m [row col] true)
              m))
          marked
          (for [row (range 5) col (range 5)] [row col])))

(defn part1 [numbers boards]
  (let [initial-marked (vec (repeat (count boards) (vec (repeat 5 (vec (repeat 5 false))))))]
    (loop [nums numbers
           marked initial-marked]
      (when-let [number (first nums)]
        (let [new-marked (reduce-kv (fn [acc i board]
                                      (update acc i #(mark-number board % number)))
                                    marked
                                    boards)]
          (if-let [winner-idx (first (keep-indexed
                                       (fn [i m] (when (check-winner m) i))
                                       new-marked))]
            (calculate-score (boards winner-idx) (new-marked winner-idx) number)
            (recur (rest nums) new-marked)))))))

(defn part2 [numbers boards]
  (let [initial-marked (vec (repeat (count boards) (vec (repeat 5 (vec (repeat 5 false))))))
        initial-won (vec (repeat (count boards) false))]
    (loop [nums numbers
           marked initial-marked
           won initial-won
           last-score nil]
      (if-let [number (first nums)]
        (let [[new-marked new-won new-score]
              (reduce (fn [[m w s] i]
                        (if (w i)
                          [m w s]
                          (let [updated-m (update m i #(mark-number (boards i) % number))]
                            (if (check-winner (updated-m i))
                              [updated-m (assoc w i true) (calculate-score (boards i) (updated-m i) number)]
                              [updated-m w s]))))
                      [marked won last-score]
                      (range (count boards)))]
          (recur (rest nums) new-marked new-won new-score))
        last-score))))

(defn -main []
  (let [[numbers boards] (parse-input)]
    (println (str "Part 1: " (part1 numbers boards)))
    (println (str "Part 2: " (part2 numbers boards)))))

(-main)
