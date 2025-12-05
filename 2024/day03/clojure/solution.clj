(ns solution)

(defn part1
  "Find all valid mul(X,Y) instructions and sum their products."
  [data]
  (let [pattern #"mul\((\d{1,3}),(\d{1,3})\)"
        matches (re-seq pattern data)]
    (->> matches
         (map (fn [[_ x y]]
                (* (parse-long x) (parse-long y))))
         (reduce + 0))))

(defn part2
  "Like part1, but do() enables and don't() disables mul instructions."
  [data]
  (let [mul-pattern #"mul\((\d{1,3}),(\d{1,3})\)"
        do-pattern #"do\(\)"
        dont-pattern #"don't\(\)"

        ;; Use re-matcher to get all matches with positions
        events (concat
                ;; Get all mul instructions with positions
                (loop [matcher (re-matcher mul-pattern data)
                       results []]
                  (if (.find matcher)
                    (recur matcher
                           (conj results {:pos (.start matcher)
                                         :type :mul
                                         :x (parse-long (.group matcher 1))
                                         :y (parse-long (.group matcher 2))}))
                    results))

                ;; Get all do() instructions with positions
                (loop [matcher (re-matcher do-pattern data)
                       results []]
                  (if (.find matcher)
                    (recur matcher
                           (conj results {:pos (.start matcher)
                                         :type :do}))
                    results))

                ;; Get all don't() instructions with positions
                (loop [matcher (re-matcher dont-pattern data)
                       results []]
                  (if (.find matcher)
                    (recur matcher
                           (conj results {:pos (.start matcher)
                                         :type :dont}))
                    results)))

        ;; Sort events by position
        sorted-events (sort-by :pos events)]

    ;; Process events in order
    (loop [events sorted-events
           enabled true
           total 0]
      (if (empty? events)
        total
        (let [event (first events)]
          (case (:type event)
            :do (recur (rest events) true total)
            :dont (recur (rest events) false total)
            :mul (if enabled
                   (recur (rest events) enabled (+ total (* (:x event) (:y event))))
                   (recur (rest events) enabled total))))))))

(defn -main []
  (let [;; Read from ../input.txt relative to current working directory
        input-path "../input.txt"
        data (slurp input-path)]
    (println "Part 1:" (part1 data))
    (println "Part 2:" (part2 data))))

(-main)
