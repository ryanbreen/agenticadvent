(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [input]
  "Parse input into vector of [x y] coordinate pairs"
  (->> (str/split-lines input)
       (map #(str/split % #","))
       (mapv (fn [[x y]] [(parse-long x) (parse-long y)]))))

(defn part1 [points]
  "Find the largest rectangle area using two red tiles as opposite corners"
  (let [n (count points)]
    (reduce max
            (for [i (range n)
                  j (range (inc i) n)
                  :let [[x1 y1] (nth points i)
                        [x2 y2] (nth points j)
                        width (inc (Math/abs (- x2 x1)))
                        height (inc (Math/abs (- y2 y1)))
                        area (* width height)]]
              area))))

(defn build-edges [points]
  "Build horizontal and vertical edges from consecutive points"
  (let [n (count points)]
    (reduce (fn [[h-edges v-edges] i]
              (let [[x1 y1] (nth points i)
                    [x2 y2] (nth points (mod (inc i) n))]
                (if (= y1 y2)
                  ;; Horizontal edge
                  [(conj h-edges [y1 (min x1 x2) (max x1 x2)]) v-edges]
                  ;; Vertical edge
                  [h-edges (conj v-edges [x1 (min y1 y2) (max y1 y2)])])))
            [[] []]
            (range n))))

(defn is-inside-polygon? [x y vert-by-x]
  "Check if point (x, y) is inside the polygon using ray casting"
  (let [crossings (reduce (fn [acc vx]
                            (if (<= vx x)
                              acc
                              (reduce (fn [inner-acc [y-min y-max]]
                                        (cond
                                          (and (< y-min y) (< y y-max)) (inc inner-acc)
                                          (or (= y y-min) (= y y-max)) (+ inner-acc 0.5)
                                          :else inner-acc))
                                      acc
                                      (get vert-by-x vx))))
                          0
                          (sort (keys vert-by-x)))]
    (= 1 (mod crossings 2))))

(defn rectangle-valid? [x1 y1 x2 y2 vert-by-x horiz-by-y]
  "Check if rectangle from (x1,y1) to (x2,y2) is entirely inside polygon"
  (let [min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    ;; Check if any vertical edge crosses through the rectangle interior
    (and
     (not-any? (fn [[vx edges]]
                 (and (< min-x vx max-x)
                      (some (fn [[y-min y-max]]
                              (not (or (<= y-max min-y) (<= max-y y-min))))
                            edges)))
               vert-by-x)
     ;; Check if any horizontal edge crosses through the rectangle interior
     (not-any? (fn [[hy edges]]
                 (and (< min-y hy max-y)
                      (some (fn [[x-min x-max]]
                              (not (or (<= x-max min-x) (<= max-x x-min))))
                            edges)))
               horiz-by-y)
     ;; Check that center is inside the polygon
     (let [center-x (/ (+ min-x max-x) 2.0)
           center-y (/ (+ min-y max-y) 2.0)]
       (is-inside-polygon? center-x center-y vert-by-x)))))

(defn part2 [points]
  "Find the largest rectangle using only red and green tiles"
  (let [[horizontal-edges vertical-edges] (build-edges points)
        ;; Build maps for efficient lookup
        vert-by-x (reduce (fn [acc [x y-min y-max]]
                           (update acc x (fnil conj []) [y-min y-max]))
                         {}
                         vertical-edges)
        horiz-by-y (reduce (fn [acc [y x-min x-max]]
                            (update acc y (fnil conj []) [x-min x-max]))
                          {}
                          horizontal-edges)
        n (count points)]
    (reduce max 0
            (for [i (range n)
                  j (range (inc i) n)
                  :let [[x1 y1] (nth points i)
                        [x2 y2] (nth points j)]
                  :when (rectangle-valid? x1 y1 x2 y2 vert-by-x horiz-by-y)
                  :let [width (inc (Math/abs (- x2 x1)))
                        height (inc (Math/abs (- y2 y1)))
                        area (* width height)]]
              area))))

(defn -main []
  (let [input (slurp "../input.txt")
        points (parse-input (str/trim input))]
    (println (str "Part 1: " (part1 points)))
    (println (str "Part 2: " (part2 points)))))

(-main)
