#!/usr/bin/env clojure -M

;;; Day 23: A Long Walk - Longest path through hiking trails

(require '[clojure.string :as str])

(defn parse-input [filename]
  "Parse the grid from input file."
  (vec (str/split-lines (str/trim (slurp filename)))))

(defn find-char-index [s ch]
  "Find index of character in string."
  (first (keep-indexed (fn [i c] (when (= c ch) i)) s)))

(defn get-cell [grid r c]
  "Get cell at position, nil if out of bounds."
  (when (and (>= r 0) (< r (count grid))
             (>= c 0) (< c (count (first grid))))
    (get (get grid r) c)))

(defn walkable? [grid r c]
  "Check if cell is walkable (not forest)."
  (let [cell (get-cell grid r c)]
    (and cell (not= cell \#))))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(def slope-dirs
  "Direction mappings for slopes."
  {\^ [-1 0]
   \v [1 0]
   \< [0 -1]
   \> [0 1]})

(defn find-junctions [grid]
  "Find all junction points (start, end, and intersections)."
  (let [rows (count grid)
        cols (count (first grid))
        start [0 (find-char-index (first grid) \.)]
        end [(dec rows) (find-char-index (last grid) \.)]]
    (into #{start end}
          (for [r (range rows)
                c (range cols)
                :when (not= (get-cell grid r c) \#)
                :let [neighbors (count (filter (fn [[dr dc]]
                                                 (walkable? grid (+ r dr) (+ c dc)))
                                               directions))]
                :when (>= neighbors 3)]
            [r c]))))

(defn explore-from-junction [grid junctions start-junction respect-slopes?]
  "BFS from a junction to find all reachable junctions with distances."
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [stack [[start-junction 0]]
           visited #{start-junction}
           edges {}]
      (if (empty? stack)
        edges
        (let [[[r c] dist] (peek stack)
              stack' (pop stack)]
          (if (and (pos? dist) (contains? junctions [r c]))
            ;; Found another junction
            (recur stack' visited (assoc edges [r c] dist))
            ;; Explore neighbors
            (let [current-cell (get-cell grid r c)
                  valid-moves (for [[dr dc] directions
                                    :let [nr (+ r dr)
                                          nc (+ c dc)]
                                    :when (and (>= nr 0) (< nr rows)
                                               (>= nc 0) (< nc cols)
                                               (not= (get-cell grid nr nc) \#)
                                               (not (contains? visited [nr nc]))
                                               ;; Slope check for Part 1
                                               (or (not respect-slopes?)
                                                   (not (contains? slope-dirs current-cell))
                                                   (= [dr dc] (slope-dirs current-cell))))]
                                [[nr nc] (inc dist)])
                  new-visited (into visited (map first valid-moves))]
              (recur (into stack' valid-moves)
                     new-visited
                     edges))))))))

(defn build-graph [grid junctions respect-slopes?]
  "Build a graph of junctions with edge weights (distances)."
  (into {} (for [junction junctions]
             [junction (explore-from-junction grid junctions junction respect-slopes?)])))

(defn longest-path-dfs [graph start end]
  "Find longest path using DFS with backtracking."
  (letfn [(dfs [node visited]
            (if (= node end)
              0
              (let [neighbors (get graph node)
                    valid-next (filter #(not (contains? visited (first %))) neighbors)]
                (if (empty? valid-next)
                  nil  ; Dead end
                  (let [results (for [[neighbor dist] valid-next
                                      :let [result (dfs neighbor (conj visited node))]
                                      :when result]
                                  (+ dist result))]
                    (when (seq results)
                      (apply max results)))))))]
    (dfs start #{})))

(defn solve [grid respect-slopes?]
  "Solve for either part."
  (let [rows (count grid)
        start [0 (find-char-index (first grid) \.)]
        end [(dec rows) (find-char-index (last grid) \.)]
        junctions (find-junctions grid)
        graph (build-graph grid junctions respect-slopes?)]
    (longest-path-dfs graph start end)))

(defn part1 [grid]
  "Part 1: Respect slope directions."
  (solve grid true))

(defn part2 [grid]
  "Part 2: Ignore slopes (treat as regular paths)."
  (solve grid false))

(defn -main []
  (let [grid (parse-input "../input.txt")]
    (println "Part 1:" (part1 grid))
    (println "Part 2:" (part2 grid))))

(-main)
