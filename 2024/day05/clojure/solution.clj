(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

;; Read and parse input
(def input-text
  (str/trim (slurp "../input.txt")))

;; Split into sections
(def sections (str/split input-text #"\n\n"))
(def rules-section (str/split-lines (first sections)))
(def updates-section (str/split-lines (second sections)))

;; Parse rules: X|Y means X must come before Y
;; Store as: rules map where rules[X] = set of pages that must come AFTER X
(def rules
  (reduce
    (fn [acc rule-line]
      (let [[before after] (map #(Integer/parseInt %) (str/split rule-line #"\|"))]
        (update acc before (fnil conj #{}) after)))
    {}
    rules-section))

;; Parse updates
(def updates
  (map (fn [line]
         (map #(Integer/parseInt %) (str/split line #",")))
       updates-section))

(defn is-valid-order?
  "Check if an update is in valid order according to rules."
  [update]
  (let [page-positions (into {} (map-indexed (fn [i page] [page i]) update))]
    (every?
      (fn [[i page]]
        (let [must-be-after (get rules page #{})]
          (every?
            (fn [after-page]
              (if-let [after-pos (get page-positions after-page)]
                (>= after-pos i)
                true))
            must-be-after)))
      (map-indexed vector update))))

(defn part1
  "Sum middle pages of correctly-ordered updates."
  []
  (reduce
    (fn [total update]
      (if (is-valid-order? update)
        (let [update-vec (vec update)
              middle-idx (quot (count update-vec) 2)]
          (+ total (nth update-vec middle-idx)))
        total))
    0
    updates))

(defn fix-order
  "Reorder an update to satisfy all rules using custom comparator."
  [update]
  (sort
    (fn [a b]
      (cond
        ;; If a must come before b
        (contains? (get rules a #{}) b) -1
        ;; If b must come before a
        (contains? (get rules b #{}) a) 1
        ;; No ordering constraint
        :else 0))
    update))

(defn part2
  "Sum middle pages of incorrectly-ordered updates after fixing."
  []
  (reduce
    (fn [total update]
      (if-not (is-valid-order? update)
        (let [fixed (fix-order update)
              fixed-vec (vec fixed)
              middle-idx (quot (count fixed-vec) 2)]
          (+ total (nth fixed-vec middle-idx)))
        total))
    0
    updates))

;; Main execution
(println (str "Part 1: " (part1)))
(println (str "Part 2: " (part2)))
