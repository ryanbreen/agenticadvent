(ns solution
  (:require [clojure.string :as str]))

(defn parse-machines
  "Parse claw machine configurations from input text."
  [text]
  (let [blocks (str/split text #"\n\n")]
    (for [block blocks]
      (let [lines (str/split-lines block)
            [_ ax ay] (re-find #"Button A: X\+(\d+), Y\+(\d+)" (first lines))
            [_ bx by] (re-find #"Button B: X\+(\d+), Y\+(\d+)" (second lines))
            [_ px py] (re-find #"Prize: X=(\d+), Y=(\d+)" (nth lines 2))]
        [(parse-long ax) (parse-long ay) (parse-long bx) (parse-long by) (parse-long px) (parse-long py)]))))

(defn solve-machine
  "Solve for button presses using Cramer's rule.

  System of equations:
    a*ax + b*bx = px
    a*ay + b*by = py

  Solution:
    det = ax*by - ay*bx
    a = (px*by - py*bx) / det
    b = (ax*py - ay*px) / det

  Returns token cost (3*a + b) or nil if no valid solution."
  ([ax ay bx by px py]
   (solve-machine ax ay bx by px py nil))
  ([ax ay bx by px py max-presses]
   (let [det (- (* ax by) (* ay bx))]
     (when-not (zero? det)
       (let [a-num (- (* px by) (* py bx))
             b-num (- (* ax py) (* ay px))]
         ;; Check if solutions are integers
         (when (and (zero? (mod a-num det))
                    (zero? (mod b-num det)))
           (let [a (quot a-num det)
                 b (quot b-num det)]
             ;; Check non-negative
             (when (and (>= a 0) (>= b 0))
               ;; Check max presses constraint (Part 1)
               (when (or (nil? max-presses)
                         (and (<= a max-presses) (<= b max-presses)))
                 (+ (* 3 a) b))))))))))

(defn part1
  "Part 1: Max 100 presses per button."
  [machines]
  (reduce +
          (keep (fn [[ax ay bx by px py]]
                  (solve-machine ax ay bx by px py 100))
                machines)))

(defn part2
  "Part 2: Prize coordinates shifted by 10^13, no press limit."
  [machines]
  (let [offset 10000000000000]
    (reduce +
            (keep (fn [[ax ay bx by px py]]
                    (solve-machine ax ay bx by
                                   (+ px offset)
                                   (+ py offset)
                                   nil))
                  machines))))

(defn -main []
  (let [input (slurp "../input.txt")
        machines (parse-machines (str/trim input))]
    (println (str "Part 1: " (part1 machines)))
    (println (str "Part 2: " (part2 machines)))))

(-main)
