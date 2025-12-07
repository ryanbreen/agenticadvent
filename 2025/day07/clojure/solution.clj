(require '[clojure.string :as str])

(defn read-input []
  (-> (slurp "../input.txt")
      (str/trim)
      (str/split #"\n")))

(defn find-start-col [lines]
  (let [first-row (first lines)]
    (some #(when (= \S (nth first-row %)) %)
          (range (count first-row)))))

(defn part1 []
  (let [lines (read-input)
        rows (count lines)
        cols (count (first lines))
        start-col (find-start-col lines)]
    (if (nil? start-col)
      0
      (loop [row 1
             active-beams #{start-col}
             split-count 0]
        (if (or (>= row rows) (empty? active-beams))
          split-count
          (let [new-beams (reduce (fn [beams col]
                                    (if (and (>= col 0) (< col cols))
                                      (let [cell (nth (nth lines row) col)]
                                        (cond
                                          (= cell \^)
                                          ;; Beam hits splitter - emit left and right
                                          (-> beams
                                              (cond-> (>= (dec col) 0) (conj (dec col)))
                                              (cond-> (< (inc col) cols) (conj (inc col))))

                                          :else
                                          ;; Beam continues straight down
                                          (conj beams col)))
                                      beams))
                                  #{}
                                  active-beams)
                ;; Count splits - one for each active beam that hit a splitter
                splits-this-row (count (filter (fn [col]
                                                 (and (>= col 0) (< col cols)
                                                      (= \^ (nth (nth lines row) col))))
                                               active-beams))]
            (recur (inc row) new-beams (+ split-count splits-this-row))))))))

(defn part2 []
  (let [lines (read-input)
        rows (count lines)
        cols (count (first lines))
        start-col (find-start-col lines)]
    (if (nil? start-col)
      0
      (loop [row 1
             timelines {start-col 1N}]
        (if (or (>= row rows) (empty? timelines))
          (reduce + (vals timelines))
          (let [new-timelines (reduce (fn [new-tl [col count]]
                                        (if (and (>= col 0) (< col cols))
                                          (let [cell (nth (nth lines row) col)]
                                            (cond
                                              (= cell \^)
                                              ;; Each timeline splits into 2 (left and right)
                                              (-> new-tl
                                                  (cond-> (>= (dec col) 0)
                                                    (update (dec col) (fnil + 0N) count))
                                                  (cond-> (< (inc col) cols)
                                                    (update (inc col) (fnil + 0N) count)))

                                              :else
                                              ;; Timelines continue straight down
                                              (update new-tl col (fnil + 0N) count)))
                                          new-tl))
                                      {}
                                      timelines)]
            (recur (inc row) new-timelines)))))))

(defn -main []
  (println (str "Part 1: " (part1)))
  (println (str "Part 2: " (part2))))

(-main)
