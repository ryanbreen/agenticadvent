#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(def ^:private directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn parse-grid
  "Parse the grid and locate start/end positions."
  [input]
  (let [lines (str/split-lines (str/trim input))
        grid (mapv vec lines)]
    (reduce
      (fn [acc r]
        (reduce
          (fn [acc2 c]
            (case (get-in grid [r c])
              \S (assoc acc2 :start [r c])
              \E (assoc acc2 :end [r c])
              acc2))
          acc
          (range (count (grid r)))))
      {:grid grid :start nil :end nil}
      (range (count grid)))))

(defn trace-path
  "BFS from start to end, returning distance map for all track positions."
  [grid start end]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
           dist {start 0}]
      (if (empty? queue)
        dist
        (let [[r c] (peek queue)
              queue' (pop queue)]
          (if (= [r c] end)
            dist
            (let [current-dist (dist [r c])
                  [new-dist new-queue]
                  (reduce
                    (fn [[d q] [dr dc]]
                      (let [nr (+ r dr)
                            nc (+ c dc)]
                        (if (and (<= 0 nr (dec rows))
                                 (<= 0 nc (dec cols))
                                 (not= \# (get-in grid [nr nc]))
                                 (not (contains? d [nr nc])))
                          [(assoc d [nr nc] (inc current-dist))
                           (conj q [nr nc])]
                          [d q])))
                    [dist queue']
                    directions)]
              (recur new-queue new-dist))))))))

(defn count-cheats
  "Count cheats that save at least min-savings using up to max-cheat-time steps.
   Uses Java arrays for O(n^2) performance on large inputs."
  [dist ^long max-cheat-time ^long min-savings]
  (let [positions (keys dist)
        n (int (count positions))
        ^ints rs (int-array (map first positions))
        ^ints cs (int-array (map second positions))
        ^ints ds (int-array (map dist positions))]
    (loop [i (int 0), cnt (long 0)]
      (if (>= i n)
        cnt
        (let [r1 (aget rs i)
              c1 (aget cs i)
              d1 (aget ds i)
              inner-cnt
              (loop [j (int 0), ic (long 0)]
                (if (>= j n)
                  ic
                  (let [cheat-cost (+ (Math/abs (- (aget rs j) r1))
                                      (Math/abs (- (aget cs j) c1)))]
                    (recur
                      (inc j)
                      (if (and (<= cheat-cost max-cheat-time)
                               (>= (- (aget ds j) d1 cheat-cost) min-savings))
                        (inc ic)
                        ic)))))]
          (recur (inc i) (+ cnt inner-cnt)))))))

(defn -main []
  (let [input (slurp "../input.txt")
        {:keys [grid start end]} (parse-grid input)
        dist (trace-path grid start end)]
    (println "Part 1:" (count-cheats dist 2 100))
    (println "Part 2:" (count-cheats dist 20 100))))

(-main)
