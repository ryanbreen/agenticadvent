#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-grid [input]
  (let [lines (str/split-lines (str/trim input))
        grid (mapv vec lines)]
    (reduce (fn [acc r]
              (reduce (fn [acc2 c]
                        (case (get-in grid [r c])
                          \S (assoc acc2 :start [r c])
                          \E (assoc acc2 :end [r c])
                          acc2))
                      acc
                      (range (count (grid r)))))
            {:grid grid :start nil :end nil}
            (range (count grid)))))

(defn trace-path [grid start end]
  (let [rows (count grid)
        cols (count (first grid))
        directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
           dist {start 0}]
      (if (empty? queue)
        dist
        (let [[r c] (peek queue)
              queue (pop queue)]
          (if (= [r c] end)
            dist
            (let [current-dist (dist [r c])
                  [new-dist new-queue]
                  (reduce (fn [[d q] [dr dc]]
                            (let [nr (+ r dr)
                                  nc (+ c dc)]
                              (if (and (>= nr 0) (< nr rows)
                                       (>= nc 0) (< nc cols)
                                       (not= \# (get-in grid [nr nc]))
                                       (not (contains? d [nr nc])))
                                [(assoc d [nr nc] (inc current-dist))
                                 (conj q [nr nc])]
                                [d q])))
                          [dist queue]
                          directions)]
              (recur new-queue new-dist))))))))

(defn count-cheats [dist ^long max-cheat-time ^long min-savings]
  (let [positions (keys dist)
        n (count positions)
        ;; Convert to arrays for speed
        ^ints rs (int-array (map first positions))
        ^ints cs (int-array (map second positions))
        ^ints ds (int-array (map #(dist %) positions))]
    (loop [i 0 cnt 0]
      (if (>= i n)
        cnt
        (let [r1 (aget rs i)
              c1 (aget cs i)
              d1 (aget ds i)
              inner-cnt
              (loop [j 0 ic 0]
                (if (>= j n)
                  ic
                  (let [r2 (aget rs j)
                        c2 (aget cs j)
                        dr (Math/abs (- r2 r1))
                        dc (Math/abs (- c2 c1))
                        cheat-cost (+ dr dc)]
                    (recur (inc j)
                           (if (<= cheat-cost max-cheat-time)
                             (let [d2 (aget ds j)
                                   savings (- d2 d1 cheat-cost)]
                               (if (>= savings min-savings)
                                 (inc ic)
                                 ic))
                             ic)))))]
          (recur (inc i) (+ cnt inner-cnt)))))))

(defn -main []
  (let [input (slurp "../input.txt")
        {:keys [grid start end]} (parse-grid input)
        dist (trace-path grid start end)]
    (println "Part 1:" (count-cheats dist 2 100))
    (println "Part 2:" (count-cheats dist 20 100))))

(-main)
