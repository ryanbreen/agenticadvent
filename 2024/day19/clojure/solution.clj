(require '[clojure.string :as str])

(def input-text
  (-> "../input.txt"
      slurp
      str/trim))

;; Parse input
(def parts (str/split input-text #"\n\n"))
(def patterns
  (->> (str/split (first parts) #",")
       (map str/trim)
       vec))
(def designs (str/split-lines (second parts)))

;; Count number of ways to form design from patterns using DP
;; Returns 0 if impossible, otherwise the count of ways
(defn count-ways [design]
  (let [n (count design)
        ;; Build DP table from end to start
        ;; dp[i] = number of ways to form design[i:]
        dp (atom (vec (repeat (inc n) 0)))]
    ;; Base case: empty suffix has 1 way
    (swap! dp assoc n 1)
    ;; Fill from right to left
    (doseq [pos (range (dec n) -1 -1)]
      (let [suffix (subs design pos)
            ways (reduce
                  (fn [total pattern]
                    (let [plen (count pattern)]
                      (if (and (<= plen (count suffix))
                               (= pattern (subs design pos (+ pos plen))))
                        (+ total (@dp (+ pos plen)))
                        total)))
                  0
                  patterns)]
        (swap! dp assoc pos ways)))
    (@dp 0)))

(defn part1 []
  ;; Count designs that can be formed (ways > 0)
  (count (filter #(pos? (count-ways %)) designs)))

(defn part2 []
  ;; Sum the number of ways for all designs
  (reduce + (map count-ways designs)))

(defn -main [& args]
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))

;; Run when script is executed
(-main)
