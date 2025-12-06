(require '[clojure.string :as str])

(def input-text
  (-> "../input.txt"
      slurp
      str/trim))

(def lines (str/split-lines input-text))

(defn find-operator-row
  "Find the index of the operator row (contains only +, *, and spaces)."
  [lines]
  (loop [idx (dec (count lines))]
    (cond
      (< idx 0) nil
      (let [line (nth lines idx)
            trimmed (str/trim line)]
        (and (seq trimmed)
             (every? #(#{\+ \* \space} %) line)))
      idx
      :else (recur (dec idx)))))

(defn parse-problems
  "Parse the worksheet into a list of [numbers operator] pairs for Part 1."
  [lines]
  (let [op-row-idx (find-operator-row lines)]
    (if (nil? op-row-idx)
      []
      (let [op-row (nth lines op-row-idx)
            number-rows (subvec (vec lines) 0 op-row-idx)
            max-width (apply max (map count lines))
            ;; Pad all rows to max width
            padded-number-rows (mapv #(format (str "%-" max-width "s") %) number-rows)
            padded-op-row (format (str "%-" max-width "s") op-row)]

        (loop [col 0
               problems []]
          (if (>= col max-width)
            problems
            ;; Skip separator columns (all spaces)
            (let [col (loop [c col]
                        (if (or (>= c max-width)
                                (not (and (every? #(= (nth % c) \space) padded-number-rows)
                                         (= (nth padded-op-row c) \space))))
                          c
                          (recur (inc c))))]
              (if (>= col max-width)
                problems
                ;; Find end of this problem
                (let [start-col col
                      end-col (loop [c col]
                                (if (>= c max-width)
                                  c
                                  (let [is-separator (and (every? #(= (nth % c) \space) padded-number-rows)
                                                         (= (nth padded-op-row c) \space))]
                                    (if is-separator
                                      c
                                      (recur (inc c))))))
                      ;; Extract numbers
                      numbers (keep (fn [row]
                                     (let [num-str (str/trim (subs row start-col end-col))]
                                       (when (seq num-str)
                                         (parse-long num-str))))
                                   padded-number-rows)
                      ;; Extract operator
                      op-str (str/trim (subs padded-op-row start-col end-col))
                      new-problems (if (and (seq op-str) (seq numbers))
                                    (conj problems [numbers op-str])
                                    problems)]
                  (recur end-col new-problems))))))))))

(defn solve-problem
  "Solve a single problem given numbers and operator."
  [numbers op]
  (case op
    "+" (reduce + numbers)
    "*" (reduce *' numbers)  ;; Use *' for BigInteger multiplication
    0))

(defn part1
  "Solve Part 1: Read numbers horizontally within each problem."
  []
  (let [problems (parse-problems lines)]
    (reduce + (map (fn [[numbers op]] (solve-problem numbers op)) problems))))

(defn parse-problems-part2
  "Parse the worksheet for Part 2 - reading right-to-left columns."
  [lines]
  (let [op-row-idx (find-operator-row lines)]
    (if (nil? op-row-idx)
      []
      (let [op-row (nth lines op-row-idx)
            number-rows (subvec (vec lines) 0 op-row-idx)
            max-width (apply max (map count lines))
            ;; Pad all rows to max width
            padded-number-rows (mapv #(format (str "%-" max-width "s") %) number-rows)
            padded-op-row (format (str "%-" max-width "s") op-row)]

        (loop [col 0
               problems []]
          (if (>= col max-width)
            problems
            ;; Skip separator columns (all spaces)
            (let [col (loop [c col]
                        (if (or (>= c max-width)
                                (not (and (every? #(= (nth % c) \space) padded-number-rows)
                                         (= (nth padded-op-row c) \space))))
                          c
                          (recur (inc c))))]
              (if (>= col max-width)
                problems
                ;; Find end of this problem
                (let [start-col col
                      end-col (loop [c col]
                                (if (>= c max-width)
                                  c
                                  (let [is-separator (and (every? #(= (nth % c) \space) padded-number-rows)
                                                         (= (nth padded-op-row c) \space))]
                                    (if is-separator
                                      c
                                      (recur (inc c))))))
                      ;; For Part 2: Read columns right-to-left
                      numbers (keep (fn [c]
                                     (let [digits (keep (fn [row]
                                                         (let [ch (nth row c)]
                                                           (when (Character/isDigit ch)
                                                             ch)))
                                                       padded-number-rows)]
                                       (when (seq digits)
                                         (parse-long (apply str digits)))))
                                   (range (dec end-col) (dec start-col) -1))
                      ;; Extract operator
                      op-str (str/trim (subs padded-op-row start-col end-col))
                      new-problems (if (and (seq op-str) (seq numbers))
                                    (conj problems [numbers op-str])
                                    problems)]
                  (recur end-col new-problems))))))))))

(defn part2
  "Solve Part 2: Read numbers column-by-column right-to-left."
  []
  (let [problems (parse-problems-part2 lines)]
    (reduce + (map (fn [[numbers op]] (solve-problem numbers op)) problems))))

;; Main execution
(println (str "Part 1: " (part1)))
(println (str "Part 2: " (part2)))
