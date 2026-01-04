#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-input [filename]
  "Parse hailstone positions and velocities from input file."
  (->> (slurp filename)
       str/split-lines
       (mapv (fn [line]
               (let [[pos vel] (str/split line #"@")
                     [px py pz] (map #(Long/parseLong (str/trim %)) (str/split (str/trim pos) #","))
                     [vx vy vz] (map #(Long/parseLong (str/trim %)) (str/split (str/trim vel) #","))]
                 {:pos [px py pz] :vel [vx vy vz]})))))

(defn find-intersection-2d [h1 h2]
  "Find intersection of two hailstone paths in 2D (XY plane).
   Returns {:x x :y y :t1 t1 :t2 t2} or nil if parallel."
  (let [[px1 py1 _] (:pos h1)
        [vx1 vy1 _] (:vel h1)
        [px2 py2 _] (:pos h2)
        [vx2 vy2 _] (:vel h2)
        ;; Solve for t1 and t2 using Cramer's rule
        ;; vx1*t1 - vx2*t2 = px2 - px1
        ;; vy1*t1 - vy2*t2 = py2 - py1
        det (- (* vx1 (- vy2)) (* (- vx2) vy1))]
    (when-not (zero? det)
      (let [dx (- px2 px1)
            dy (- py2 py1)
            t1 (/ (- (* dx (- vy2)) (* (- vx2) dy)) det)
            t2 (/ (- (* vx1 dy) (* dx vy1)) det)
            x (+ px1 (* vx1 t1))
            y (+ py1 (* vy1 t1))]
        {:x x :y y :t1 t1 :t2 t2}))))

(defn part1 [hailstones & {:keys [min-coord max-coord]
                           :or {min-coord 200000000000000
                                max-coord 400000000000000}}]
  "Count intersections within test area, in the future for both hailstones."
  (let [n (count hailstones)]
    (count
     (for [i (range n)
           j (range (inc i) n)
           :let [h1 (nth hailstones i)
                 h2 (nth hailstones j)
                 result (find-intersection-2d h1 h2)]
           :when result
           :let [{:keys [x y t1 t2]} result]
           :when (and (>= t1 0)
                      (>= t2 0)
                      (<= min-coord x max-coord)
                      (<= min-coord y max-coord))]
       [i j]))))

(defn solve-system [matrix rhs]
  "Solve system of linear equations using Gaussian elimination with exact rational arithmetic."
  (let [n (count matrix)
        ;; Build augmented matrix using Clojure ratios
        aug (vec (map-indexed
                  (fn [i row]
                    (vec (concat (map #(/ % 1) row) [(/ (nth rhs i) 1)])))
                  matrix))]
    ;; Forward elimination
    (let [aug-after-fwd
          (reduce
           (fn [aug col]
             (let [;; Find pivot (max element in column)
                   max-row (reduce (fn [best row]
                                    (if (> (abs (get-in aug [row col]))
                                           (abs (get-in aug [best col])))
                                      row
                                      best))
                                  col
                                  (range col n))
                   ;; Swap rows
                   aug (-> aug
                           (assoc col (nth aug max-row))
                           (assoc max-row (nth aug col)))
                   pivot (get-in aug [col col])]
               (if (zero? pivot)
                 aug
                 ;; Eliminate column below pivot
                 (reduce
                  (fn [aug row]
                    (if (zero? (get-in aug [row col]))
                      aug
                      (let [factor (/ (get-in aug [row col]) pivot)]
                        (update aug row
                                (fn [r]
                                  (vec (map-indexed
                                        (fn [j val]
                                          (if (< j col)
                                            val
                                            (- val (* factor (get-in aug [col j])))))
                                        r)))))))
                  aug
                  (range (inc col) n)))))
           aug
           (range n))]
      ;; Back substitution
      (reduce
       (fn [solution i]
         (let [sum (reduce (fn [s j]
                            (+ s (* (get-in aug-after-fwd [i j]) (nth solution j))))
                          0
                          (range (inc i) n))
               val (/ (- (get-in aug-after-fwd [i n]) sum)
                      (get-in aug-after-fwd [i i]))]
           (assoc solution i val)))
       (vec (repeat n 0))
       (range (dec n) -1 -1)))))

(defn part2 [hailstones]
  "Find rock position and velocity that hits all hailstones.
   Uses linearization by taking differences between pairs of hailstones."
  (let [h (vec (take 5 hailstones))
        ;; Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
        matrix-xy (vec (for [i (range 4)
                             :let [[px1 py1 _] (:pos (nth h i))
                                   [vx1 vy1 _] (:vel (nth h i))
                                   [px2 py2 _] (:pos (nth h (inc i)))
                                   [vx2 vy2 _] (:vel (nth h (inc i)))
                                   a (- vy1 vy2)
                                   b (- vx2 vx1)
                                   c (- py2 py1)
                                   d (- px1 px2)]]
                         [a b c d]))
        rhs-xy (vec (for [i (range 4)
                          :let [[px1 py1 _] (:pos (nth h i))
                                [vx1 vy1 _] (:vel (nth h i))
                                [px2 py2 _] (:pos (nth h (inc i)))
                                [vx2 vy2 _] (:vel (nth h (inc i)))]]
                      (- (- (* px1 vy1) (* py1 vx1))
                         (- (* px2 vy2) (* py2 vx2)))))
        [rx ry _rvx _rvy] (solve-system matrix-xy rhs-xy)
        ;; Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
        matrix-xz (vec (for [i (range 4)
                             :let [[px1 _ pz1] (:pos (nth h i))
                                   [vx1 _ vz1] (:vel (nth h i))
                                   [px2 _ pz2] (:pos (nth h (inc i)))
                                   [vx2 _ vz2] (:vel (nth h (inc i)))
                                   a (- vz1 vz2)
                                   b (- vx2 vx1)
                                   c (- pz2 pz1)
                                   d (- px1 px2)]]
                         [a b c d]))
        rhs-xz (vec (for [i (range 4)
                          :let [[px1 _ pz1] (:pos (nth h i))
                                [vx1 _ vz1] (:vel (nth h i))
                                [px2 _ pz2] (:pos (nth h (inc i)))
                                [vx2 _ vz2] (:vel (nth h (inc i)))]]
                      (- (- (* px1 vz1) (* pz1 vx1))
                         (- (* px2 vz2) (* pz2 vx2)))))
        [_rx2 rz _rvx2 _rvz] (solve-system matrix-xz rhs-xz)]
    (long (+ rx ry rz))))

(defn -main [& args]
  (let [input-file (or (first args) "../input.txt")
        hailstones (parse-input input-file)]
    (println "Part 1:" (part1 hailstones))
    (println "Part 2:" (part2 hailstones))))

(-main)
