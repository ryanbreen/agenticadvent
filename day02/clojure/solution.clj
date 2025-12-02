(ns solution
  (:require [clojure.string :as str]))

(defn invalid-id?
  "Check if a number is an invalid ID (a sequence of digits repeated exactly twice)."
  [n]
  (let [s (str n)
        len (count s)]
    ;; Must have even length to be split in half
    (and (even? len)
         (pos? len)
         ;; Check if first half equals second half
         (let [half (/ len 2)
               first-half (subs s 0 half)
               second-half (subs s half)]
           ;; First half must not have leading zeros (e.g., 0101 is not valid)
           (and (not (str/starts-with? first-half "0"))
                (= first-half second-half))))))

(defn count-invalid-ids-in-range
  "Count and sum all invalid IDs in the range [start, end]."
  [start end]
  (reduce (fn [sum n]
            (if (invalid-id? n)
              (+ sum n)
              sum))
          0
          (range start (inc end))))

(defn parse-range
  "Parse a range string like '11-22' into [11 22]."
  [range-str]
  (let [[start end] (str/split range-str #"-")]
    [(parse-long start) (parse-long end)]))

(defn solve
  "Solve the problem by parsing input and summing all invalid IDs."
  [input]
  (let [ranges (-> input
                   str/trim
                   (str/split #","))
        invalid-sum (reduce (fn [total range-str]
                              (let [[start end] (parse-range range-str)]
                                (+ total (count-invalid-ids-in-range start end))))
                            0
                            ranges)]
    invalid-sum))

(defn -main []
  (let [input (slurp "/Users/wrb/fun/code/advent2025/day02/input.txt")
        answer (solve input)]
    (println "Part 1 Answer:" answer)))

(-main)
