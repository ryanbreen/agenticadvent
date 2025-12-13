(require '[clojure.string :as str])

(def input-text
  (-> "../input.txt"
      slurp
      str/trim))

;; Parse input - space-separated numbers
(def stones
  (->> (str/split input-text #"\s+")
       (map #(Long/parseLong %))))

;; Memoized function to count stones after N blinks
(def count-stones
  (memoize
   (fn [value blinks]
     (if (zero? blinks)
       1
       (cond
         ;; Rule 1: 0 becomes 1
         (zero? value)
         (count-stones 1 (dec blinks))

         ;; Rule 2: Even number of digits -> split
         (let [s (str value)]
           (even? (count s)))
         (let [s (str value)
               mid (quot (count s) 2)
               left (Long/parseLong (subs s 0 mid))
               right (Long/parseLong (subs s mid))]
           (+ (count-stones left (dec blinks))
              (count-stones right (dec blinks))))

         ;; Rule 3: Multiply by 2024
         :else
         (count-stones (* value 2024) (dec blinks)))))))

(defn part1 []
  (reduce + (map #(count-stones % 25) stones)))

(defn part2 []
  (reduce + (map #(count-stones % 75) stones)))

(println "Part 1:" (part1))
(println "Part 2:" (part2))
