(ns solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> (slurp "../input.txt")
      str/trim
      (str/split #"\n")))

(defn parse-numbers [s]
  "Parse a space-separated string of numbers into a set"
  (->> (str/split (str/trim s) #"\s+")
       (map parse-long)
       set))

(defn parse-card [line]
  "Parse a card line into [winning-set have-set]"
  (let [[_ numbers] (str/split line #":")
        [winning-part have-part] (str/split numbers #"\|")]
    [(parse-numbers winning-part)
     (parse-numbers have-part)]))

(defn parse-cards []
  "Parse all cards from input"
  (map parse-card input))

(defn count-matches [[winning have]]
  "Count how many numbers match between winning and have"
  (count (set/intersection winning have)))

(defn card-score [card]
  "Calculate the score for a card: 2^(matches-1) if matches > 0, else 0"
  (let [matches (count-matches card)]
    (if (pos? matches)
      (bit-shift-left 1 (dec matches))
      0)))

(defn part1 []
  (->> (parse-cards)
       (map card-score)
       (reduce + 0)))

(defn part2 []
  (let [cards (vec (parse-cards))
        n (count cards)
        matches (mapv count-matches cards)
        initial-copies (vec (repeat n 1))]
    (->> (range n)
         (reduce (fn [copies i]
                   (let [m (nth matches i)
                         card-copies (nth copies i)]
                     (reduce (fn [c j]
                               (update c j + card-copies))
                             copies
                             (range (inc i) (min (+ i 1 m) n)))))
                 initial-copies)
         (reduce + 0))))

;; Main execution
(println "Part 1:" (part1))
(println "Part 2:" (part2))
