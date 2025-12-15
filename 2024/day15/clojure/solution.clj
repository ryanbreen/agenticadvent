(require '[clojure.string :as str])

(defn parse-input [text]
  (let [[grid-text moves-text] (str/split text #"\n\n")
        grid (->> (str/split-lines grid-text)
                  (mapv vec))
        moves (str/replace moves-text #"\s" "")]
    [grid moves]))

(defn find-robot [grid]
  (first
   (for [r (range (count grid))
         c (range (count (grid r)))
         :when (= (get-in grid [r c]) \@)]
     [r c])))

(defn move-robot [grid robot-pos direction]
  (let [deltas {\< [0 -1], \> [0 1], \^ [-1 0], \v [1 0]}
        [dr dc] (deltas direction)
        [r c] robot-pos
        [nr nc] [(+ r dr) (+ c dc)]
        target (get-in grid [nr nc])]

    (cond
      ;; Hit a wall
      (= target \#)
      robot-pos

      ;; Empty space
      (= target \.)
      (do
        [(assoc-in (assoc-in grid [r c] \.) [nr nc] \@)
         [nr nc]])

      ;; Box
      (= target \O)
      (let [;; Find end of box chain
            find-end (fn []
                       (loop [check-r nr
                              check-c nc]
                         (if (= (get-in grid [check-r check-c]) \O)
                           (recur (+ check-r dr) (+ check-c dc))
                           [check-r check-c])))
            [end-r end-c] (find-end)]

        (if (= (get-in grid [end-r end-c]) \#)
          robot-pos
          (do
            [(-> grid
                 (assoc-in [end-r end-c] \O)
                 (assoc-in [r c] \.)
                 (assoc-in [nr nc] \@))
             [nr nc]])))

      :else
      robot-pos)))

(defn calculate-gps [grid box-char]
  (reduce +
          (for [r (range (count grid))
                c (range (count (grid r)))
                :when (= (get-in grid [r c]) box-char)]
            (+ (* 100 r) c))))

(defn part1 [input-text]
  (let [[grid moves] (parse-input input-text)]
    (loop [g grid
           pos (find-robot grid)
           remaining-moves (seq moves)]
      (if (empty? remaining-moves)
        (calculate-gps g \O)
        (let [move (first remaining-moves)
              result (move-robot g pos move)]
          (if (vector? (first result))
            (recur (first result) (second result) (rest remaining-moves))
            (recur g result (rest remaining-moves))))))))

(defn scale-grid [grid]
  (mapv (fn [row]
          (vec (mapcat (fn [cell]
                         (case cell
                           \# [\# \#]
                           \O [\[ \]]
                           \. [\. \.]
                           \@ [\@ \.]))
                       row)))
        grid))

(defn can-move-box-vertical? [grid box-left-c r dr]
  (let [nr (+ r dr)
        left-c box-left-c
        right-c (inc box-left-c)
        left-target (get-in grid [nr left-c])
        right-target (get-in grid [nr right-c])]

    (cond
      ;; Hit wall
      (or (= left-target \#) (= right-target \#))
      false

      ;; Both empty
      (and (= left-target \.) (= right-target \.))
      true

      ;; Check boxes in the way
      :else
      (let [boxes-to-check (cond-> #{}
                             (= left-target \[)
                             (conj [nr left-c])

                             (= left-target \])
                             (conj [nr (dec left-c)])

                             (= right-target \[)
                             (conj [nr right-c])

                             (= right-target \])
                             (conj [nr (dec right-c)]))]
        (every? (fn [[box-r box-c]]
                  (can-move-box-vertical? grid box-c box-r dr))
                boxes-to-check)))))

(defn collect-boxes-vertical [grid box-left-c r dr collected]
  (let [collected (conj collected [r box-left-c])
        nr (+ r dr)
        left-c box-left-c
        right-c (inc box-left-c)
        left-target (get-in grid [nr left-c])
        right-target (get-in grid [nr right-c])

        boxes-to-check (cond-> #{}
                         (= left-target \[)
                         (conj [nr left-c])

                         (= left-target \])
                         (conj [nr (dec left-c)])

                         (= right-target \[)
                         (conj [nr right-c])

                         (= right-target \])
                         (conj [nr (dec right-c)]))]

    (reduce (fn [coll [box-r box-c]]
              (if (contains? coll [box-r box-c])
                coll
                (collect-boxes-vertical grid box-c box-r dr coll)))
            collected
            boxes-to-check)))

(defn move-robot-wide [grid robot-pos direction]
  (let [deltas {\< [0 -1], \> [0 1], \^ [-1 0], \v [1 0]}
        [dr dc] (deltas direction)
        [r c] robot-pos
        [nr nc] [(+ r dr) (+ c dc)]
        target (get-in grid [nr nc])]

    (cond
      ;; Hit wall
      (= target \#)
      [grid robot-pos]

      ;; Empty space
      (= target \.)
      [(assoc-in (assoc-in grid [r c] \.) [nr nc] \@)
       [nr nc]]

      ;; Box - horizontal movement
      (and (or (= target \[) (= target \])) (not= dc 0))
      (let [;; Find end of box chain
            find-end (fn []
                       (loop [check-c nc]
                         (if (or (= (get-in grid [r check-c]) \[)
                                 (= (get-in grid [r check-c]) \]))
                           (recur (+ check-c dc))
                           check-c)))
            check-c (find-end)]

        (if (= (get-in grid [r check-c]) \#)
          [grid robot-pos]
          (let [;; Shift all boxes
                new-grid (if (> dc 0) ;; Moving right
                           (reduce (fn [g col]
                                     (assoc-in g [r col] (get-in g [r (dec col)])))
                                   grid
                                   (range check-c (dec nc) -1))
                           ;; Moving left
                           (reduce (fn [g col]
                                     (assoc-in g [r col] (get-in g [r (inc col)])))
                                   grid
                                   (range check-c nc)))
                final-grid (-> new-grid
                               (assoc-in [r c] \.)
                               (assoc-in [nr nc] \@))]
            [final-grid [nr nc]])))

      ;; Box - vertical movement
      (and (or (= target \[) (= target \])) (not= dr 0))
      (let [box-left-c (if (= target \[) nc (dec nc))]

        (if (not (can-move-box-vertical? grid box-left-c nr dr))
          [grid robot-pos]
          (let [;; Collect all boxes to move
                boxes-to-move (collect-boxes-vertical grid box-left-c nr dr #{})

                ;; Sort boxes by row
                sorted-boxes (if (> dr 0)
                               (sort-by first > boxes-to-move)
                               (sort-by first boxes-to-move))

                ;; Move all boxes
                new-grid (reduce (fn [g [box-r box-c]]
                                   (-> g
                                       (assoc-in [box-r box-c] \.)
                                       (assoc-in [box-r (inc box-c)] \.)
                                       (assoc-in [(+ box-r dr) box-c] \[)
                                       (assoc-in [(+ box-r dr) (inc box-c)] \])))
                                 grid
                                 sorted-boxes)

                final-grid (-> new-grid
                               (assoc-in [r c] \.)
                               (assoc-in [nr nc] \@))]
            [final-grid [nr nc]])))

      :else
      [grid robot-pos])))

(defn part2 [input-text]
  (let [[grid moves] (parse-input input-text)
        scaled-grid (scale-grid grid)]
    (loop [g scaled-grid
           pos (find-robot scaled-grid)
           remaining-moves (seq moves)]
      (if (empty? remaining-moves)
        (calculate-gps g \[)
        (let [move (first remaining-moves)
              [new-grid new-pos] (move-robot-wide g pos move)]
          (recur new-grid new-pos (rest remaining-moves)))))))

(defn -main []
  (let [input-text (slurp "../input.txt")]
    (println "Part 1:" (part1 input-text))
    (println "Part 2:" (part2 input-text))))

(-main)
