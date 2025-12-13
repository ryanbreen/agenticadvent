(require '[clojure.string :as str])

;; Directions: up, down, left, right
(def dirs [[-1 0] [1 0] [0 -1] [0 1]])

;; Empty queue constant - PersistentQueue has no reader literal
(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn parse-grid
  "Parse input text into a 2D vector grid of digits."
  [text]
  (->> text
       str/split-lines
       (mapv (fn [line]
               (mapv #(Character/digit % 10) line)))))

(defn find-trailheads
  "Find all positions with height 0."
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows)
          c (range cols)
          :when (= 0 (get-in grid [r c]))]
      [r c])))

(defn in-bounds?
  "Check if position is within grid bounds."
  [grid r c]
  (and (>= r 0) (< r (count grid))
       (>= c 0) (< c (count (first grid)))))

(defn count-reachable-nines
  "BFS to find all 9s reachable from a trailhead."
  [grid start-r start-c]
  (loop [queue (conj empty-queue [start-r start-c])
         visited #{[start-r start-c]}
         nines #{}]
    (if (empty? queue)
      (count nines)
      (let [[r c] (peek queue)
            queue' (pop queue)
            current-height (get-in grid [r c])]
        (if (= current-height 9)
          (recur queue' visited (conj nines [r c]))
          (let [neighbors (for [[dr dc] dirs
                                :let [nr (+ r dr)
                                      nc (+ c dc)]
                                :when (and (in-bounds? grid nr nc)
                                          (not (visited [nr nc]))
                                          (= (get-in grid [nr nc])
                                             (inc current-height)))]
                            [nr nc])
                new-visited (into visited neighbors)
                new-queue (into queue' neighbors)]
            (recur new-queue new-visited nines)))))))

(defn count-distinct-trails
  "DFS to count all distinct trails from a trailhead to any 9."
  [grid start-r start-c]
  (letfn [(dfs [r c]
            (let [current-height (get-in grid [r c])]
              (if (= current-height 9)
                1
                (->> dirs
                     (map (fn [[dr dc]]
                            (let [nr (+ r dr)
                                  nc (+ c dc)]
                              (when (and (in-bounds? grid nr nc)
                                        (= (get-in grid [nr nc])
                                           (inc current-height)))
                                (dfs nr nc)))))
                     (remove nil?)
                     (reduce + 0)))))]
    (dfs start-r start-c)))

(defn part1
  [grid]
  (->> grid
       find-trailheads
       (map (fn [[r c]] (count-reachable-nines grid r c)))
       (reduce +)))

(defn part2
  [grid]
  (->> grid
       find-trailheads
       (map (fn [[r c]] (count-distinct-trails grid r c)))
       (reduce +)))

;; Main execution
(let [grid (-> (slurp "../input.txt")
               parse-grid)]
  (println (str "Part 1: " (part1 grid)))
  (println (str "Part 2: " (part2 grid))))
