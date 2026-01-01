#!/usr/bin/env clojure -M

;; Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem

(require '[clojure.string :as str])

(defn parse-line
  "Parse a single dig plan instruction into [direction distance color]."
  [line]
  (let [[dir dist color-part] (str/split line #"\s+")
        distance (parse-long dist)
        color (subs color-part 2 (dec (count color-part)))]  ; Remove (# and )
    [dir distance color]))

(defn parse-input
  "Parse the input file into a sequence of instructions."
  [filename]
  (->> (slurp filename)
       str/trim
       str/split-lines
       (map parse-line)))

(defn shoelace-area
  "Calculate the area of a polygon using the Shoelace formula."
  [vertices]
  (let [n (count vertices)
        pairs (map vector vertices (concat (rest vertices) [(first vertices)]))]
    (/ (Math/abs (reduce (fn [acc [[r1 c1] [r2 c2]]]
                           (+ acc (- (* r1 c2) (* r2 c1))))
                         0
                         pairs))
       2)))

(defn calculate-total-area
  "Calculate total area using Shoelace formula and Pick's theorem.
   Total points = interior + boundary = shoelace_area + perimeter/2 + 1"
  [vertices perimeter]
  (+ (shoelace-area vertices) (/ perimeter 2) 1))

(def direction-map-part1
  {"R" [0 1]
   "D" [1 0]
   "L" [0 -1]
   "U" [-1 0]})

(defn trace-vertices-part1
  "Trace the polygon vertices following Part 1 instructions."
  [instructions]
  (loop [remaining instructions
         r 0
         c 0
         vertices [[0 0]]
         perimeter 0]
    (if (empty? remaining)
      [vertices perimeter]
      (let [[direction distance _] (first remaining)
            [dr dc] (direction-map-part1 direction)
            new-r (+ r (* dr distance))
            new-c (+ c (* dc distance))]
        (recur (rest remaining)
               new-r
               new-c
               (conj vertices [new-r new-c])
               (+ perimeter distance))))))

(defn part1
  "Part 1: Follow the dig plan directions."
  [instructions]
  (let [[vertices perimeter] (trace-vertices-part1 instructions)]
    (long (calculate-total-area vertices perimeter))))

(def direction-map-part2
  {\0 [0 1]    ; R
   \1 [1 0]    ; D
   \2 [0 -1]   ; L
   \3 [-1 0]}) ; U

(defn trace-vertices-part2
  "Trace the polygon vertices following Part 2 hex-encoded instructions."
  [instructions]
  (loop [remaining instructions
         r 0
         c 0
         vertices [[0 0]]
         perimeter 0]
    (if (empty? remaining)
      [vertices perimeter]
      (let [[_ _ color] (first remaining)
            distance (Long/parseLong (subs color 0 5) 16)
            direction-char (nth color 5)
            [dr dc] (direction-map-part2 direction-char)
            new-r (+ r (* dr distance))
            new-c (+ c (* dc distance))]
        (recur (rest remaining)
               new-r
               new-c
               (conj vertices [new-r new-c])
               (+ perimeter distance))))))

(defn part2
  "Part 2: Decode instructions from hex color codes."
  [instructions]
  (let [[vertices perimeter] (trace-vertices-part2 instructions)]
    (long (calculate-total-area vertices perimeter))))

(defn -main
  []
  (let [instructions (parse-input "../input.txt")]
    (println "Part 1:" (part1 instructions))
    (println "Part 2:" (part2 instructions))))

(-main)
