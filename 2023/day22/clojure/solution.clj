#!/usr/bin/env clojure -M

(ns solution
  (:require [clojure.string :as str]))

(defn parse-brick [line]
  "Parse a brick from 'x1,y1,z1~x2,y2,z2' format."
  (let [[left right] (str/split line #"~")
        [x1 y1 z1] (mapv parse-long (str/split left #","))
        [x2 y2 z2] (mapv parse-long (str/split right #","))
        ;; Ensure z1 <= z2 for consistent processing
        [x1 y1 z1 x2 y2 z2] (if (> z1 z2)
                              [x2 y2 z2 x1 y1 z1]
                              [x1 y1 z1 x2 y2 z2])]
    {:x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2}))

(defn parse-input [filename]
  "Parse all bricks from input file."
  (->> (slurp filename)
       str/trim
       str/split-lines
       (mapv parse-brick)))

(defn brick-xy-footprint [{:keys [x1 y1 x2 y2]}]
  "Get all (x, y) coordinates covered by a brick's footprint."
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(defn brick-cells [{:keys [x1 y1 z1 x2 y2 z2]}]
  "Get all (x, y, z) cells occupied by a brick."
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))
        z (range (min z1 z2) (inc (max z1 z2)))]
    [x y z]))

(defn find-drop-distance [brick occupied]
  "Find how far a brick can drop before hitting ground or another brick."
  (let [{:keys [z1]} brick
        footprint (brick-xy-footprint brick)]
    (reduce
     (fn [min-drop [x y]]
       (loop [z (dec z1)]
         (cond
           (<= z 0) (min min-drop (dec z1))
           (contains? occupied [x y z]) (min min-drop (- z1 z 1))
           :else (recur (dec z)))))
     (dec z1)  ; Maximum possible drop (to z=1)
     footprint)))

(defn drop-brick [brick drop-dist]
  "Create a new brick dropped by the given distance."
  (-> brick
      (update :z1 - drop-dist)
      (update :z2 - drop-dist)))

(defn find-supporters [brick brick-idx occupied]
  "Find indices of bricks that support this brick (directly below)."
  (let [{:keys [z1]} brick
        footprint (brick-xy-footprint brick)]
    (reduce
     (fn [supporters [x y]]
       (if-let [supporter-idx (get occupied [x y (dec z1)])]
         (conj supporters supporter-idx)
         supporters))
     #{}
     footprint)))

(defn settle-bricks [bricks]
  "Simulate all bricks falling and settling.
   Returns {:settled [...], :supports {...}, :supporters {...}}"
  (let [;; Sort bricks by minimum z coordinate, keeping original indices
        indexed-bricks (map-indexed vector bricks)
        sorted-bricks (sort-by (fn [[_ brick]] (:z1 brick)) indexed-bricks)
        n (count bricks)]
    (loop [remaining sorted-bricks
           settled (vec (repeat n nil))
           occupied {}  ; (x,y,z) -> brick-idx
           supports {}  ; brick-idx -> set of bricks it supports
           supporters {}]  ; brick-idx -> set of bricks that support it
      (if (empty? remaining)
        {:settled settled
         :supports supports
         :supporters supporters}
        (let [[orig-idx brick] (first remaining)
              drop-dist (find-drop-distance brick occupied)
              new-brick (drop-brick brick drop-dist)
              brick-supporters (find-supporters new-brick orig-idx occupied)
              new-cells (brick-cells new-brick)]
          (recur
           (rest remaining)
           (assoc settled orig-idx new-brick)
           (reduce (fn [occ cell] (assoc occ cell orig-idx)) occupied new-cells)
           (reduce (fn [sup supporter-idx]
                     (update sup supporter-idx (fnil conj #{}) orig-idx))
                   supports
                   brick-supporters)
           (assoc supporters orig-idx brick-supporters)))))))

(defn can-safely-remove? [brick-idx supports supporters]
  "Check if a brick can be safely removed without causing others to fall."
  (let [supported-bricks (get supports brick-idx #{})]
    (every? (fn [supported]
              (> (count (get supporters supported #{})) 1))
            supported-bricks)))

(defn part1 [bricks]
  "Count bricks that can be safely disintegrated."
  (let [{:keys [supports supporters]} (settle-bricks bricks)]
    (count (filter #(can-safely-remove? % supports supporters)
                   (range (count bricks))))))

(defn count-chain-reaction [start-idx supports supporters]
  "Count how many bricks would fall if we remove start-idx.
   Uses BFS to simulate chain reaction."
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-idx)
         falling #{start-idx}]
    (if (empty? queue)
      (dec (count falling))  ; Don't count the initial brick
      (let [brick (peek queue)
            queue' (pop queue)
            supported-bricks (get supports brick #{})
            ;; Find bricks that would now fall
            new-falling (filter
                         (fn [supported]
                           (and (not (contains? falling supported))
                                (every? #(contains? falling %)
                                        (get supporters supported #{}))))
                         supported-bricks)]
        (recur
         (reduce conj queue' new-falling)
         (reduce conj falling new-falling))))))

(defn part2 [bricks]
  "Sum of bricks that would fall for each possible disintegration."
  (let [{:keys [supports supporters]} (settle-bricks bricks)]
    (reduce + (map #(count-chain-reaction % supports supporters)
                   (range (count bricks))))))

(defn -main []
  (let [input-path (str (-> (java.io.File. *file*)
                            .getParentFile
                            .getParent)
                        "/input.txt")
        bricks (parse-input input-path)]
    (println "Part 1:" (part1 bricks))
    (println "Part 2:" (part2 bricks))))

(-main)
