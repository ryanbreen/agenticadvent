#!/usr/bin/env bb

(require '[clojure.string :as str])

(defn parse-input [filename]
  (let [lines (str/split-lines (slurp filename))
        rows (count lines)
        cols (count (first lines))
        antennas (->> lines
                      (map-indexed vector)
                      (reduce (fn [acc [r row]]
                                (reduce (fn [acc2 [c ch]]
                                          (if (not= ch \.)
                                            (update acc2 ch (fnil conj []) [r c])
                                            acc2))
                                        acc
                                        (map-indexed vector row)))
                              {}))]
    {:rows rows :cols cols :antennas antennas}))

(defn in-bounds? [rows cols [r c]]
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defn combinations [coll]
  (for [i (range (count coll))
        j (range (inc i) (count coll))]
    [(nth coll i) (nth coll j)]))

(defn find-antinodes-part1 [rows cols [[r1 c1] [r2 c2]]]
  (let [ar1 (- (* 2 r1) r2)
        ac1 (- (* 2 c1) c2)
        ar2 (- (* 2 r2) r1)
        ac2 (- (* 2 c2) c1)]
    (cond-> []
      (in-bounds? rows cols [ar1 ac1]) (conj [ar1 ac1])
      (in-bounds? rows cols [ar2 ac2]) (conj [ar2 ac2]))))

(defn points-along-line [rows cols [r c] [dr dc]]
  (->> (iterate (fn [[r c]] [(+ r dr) (+ c dc)]) [r c])
       (take-while #(in-bounds? rows cols %))))

(defn find-antinodes-part2 [rows cols [[r1 c1] [r2 c2]]]
  (let [dr (- r2 r1)
        dc (- c2 c1)]
    (concat
      (points-along-line rows cols [r1 c1] [dr dc])
      (points-along-line rows cols [(- r1 dr) (- c1 dc)] [(- dr) (- dc)]))))

(defn solve [antenna-fn]
  (let [{:keys [rows cols antennas]} (parse-input "../input.txt")]
    (->> antennas
         vals
         (mapcat combinations)
         (mapcat (partial antenna-fn rows cols))
         (into #{})
         count)))

(defn part1 []
  (solve find-antinodes-part1))

(defn part2 []
  (solve find-antinodes-part2))

(defn -main []
  (println "Part 1:" (part1))
  (println "Part 2:" (part2)))

(-main)
