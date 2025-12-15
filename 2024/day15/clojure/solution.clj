(require '[clojure.string :as str])

(defn parse-input [text]
  (let [[grid-text moves-text] (str/split text #"\n\n")
        grid (->> (str/split-lines grid-text)
                  (mapv vec))
        moves (str/replace moves-text #"\s" "")]
    [grid moves]))

(defn find-robot [grid]
  (let [height (count grid)
        width (count (grid 0))]
    (first
     (for [r (range height)
           c (range width)
           :when (= (get-in grid [r c]) \@)]
       [r c]))))

;; Direction to delta lookup
(def direction-deltas
  {\< [0 -1], \> [0 1], \^ [-1 0], \v [1 0]})

;; Direct grid updates - optimized for small update counts
(defn grid-set-multi [grid updates]
  "Apply multiple grid updates efficiently"
  (case (count updates)
    2 (let [[[r1 c1 v1] [r2 c2 v2]] updates]
        (-> grid
            (assoc-in [r1 c1] v1)
            (assoc-in [r2 c2] v2)))
    3 (let [[[r1 c1 v1] [r2 c2 v2] [r3 c3 v3]] updates]
        (-> grid
            (assoc-in [r1 c1] v1)
            (assoc-in [r2 c2] v2)
            (assoc-in [r3 c3] v3)))
    ;; For larger updates, group by row
    (let [by-row (group-by first updates)]
      (reduce-kv (fn [g r row-updates]
                   (update g r
                           (fn [row]
                             (reduce (fn [r [_ c val]]
                                       (assoc r c val))
                                     row
                                     row-updates))))
                 grid
                 by-row))))

(defn move-robot [grid robot-pos direction]
  (let [[dr dc] (direction-deltas direction)
        [r c] robot-pos
        [nr nc] [(+ r dr) (+ c dc)]
        next-row (grid nr)
        target (next-row nc)]

    (cond
      ;; Hit a wall
      (= target \#)
      [grid robot-pos]

      ;; Empty space
      (= target \.)
      [(grid-set-multi grid [[r c \.] [nr nc \@]])
       [nr nc]]

      ;; Box
      (= target \O)
      (let [;; Find end of box chain
            find-end (fn find-end [check-r check-c]
                       (let [row (grid check-r)
                             cell (row check-c)]
                         (if (= cell \O)
                           (recur (+ check-r dr) (+ check-c dc))
                           [check-r check-c])))
            [end-r end-c] (find-end nr nc)
            end-row (grid end-r)]

        (if (= (end-row end-c) \#)
          [grid robot-pos]
          [(grid-set-multi grid [[end-r end-c \O] [r c \.] [nr nc \@]])
           [nr nc]]))

      :else
      [grid robot-pos])))

(defn calculate-gps [grid box-char]
  (let [height (count grid)
        width (count (grid 0))]
    (reduce +
            (for [r (range height)
                  c (range width)
                  :when (= (get-in grid [r c]) box-char)]
              (+ (* 100 r) c)))))

(defn part1 [input-text]
  (let [[grid moves] (parse-input input-text)
        initial-pos (find-robot grid)
        [final-grid _] (reduce (fn [[g pos] move]
                                 (move-robot g pos move))
                               [grid initial-pos]
                               moves)]
    (calculate-gps final-grid \O)))

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
        next-row (grid nr)
        left-target (next-row left-c)
        right-target (next-row right-c)]

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
        next-row (grid nr)
        left-target (next-row left-c)
        right-target (next-row right-c)

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
  (let [[dr dc] (direction-deltas direction)
        [r c] robot-pos
        [nr nc] [(+ r dr) (+ c dc)]
        target (get-in grid [nr nc])]

    (cond
      ;; Hit wall
      (= target \#)
      [grid robot-pos]

      ;; Empty space
      (= target \.)
      [(grid-set-multi grid [[r c \.] [nr nc \@]])
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
          (let [;; Build all updates at once
                row-data (grid r)
                updates (if (> dc 0) ;; Moving right
                          (concat
                           (for [col (range check-c (dec nc) -1)]
                             [r col (row-data (dec col))])
                           [[r c \.] [nr nc \@]])
                          ;; Moving left
                          (concat
                           (for [col (range check-c nc)]
                             [r col (row-data (inc col))])
                           [[r c \.] [nr nc \@]]))]
            [(grid-set-multi grid updates) [nr nc]])))

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

                ;; Build all updates at once
                box-updates (mapcat (fn [[box-r box-c]]
                                      [[box-r box-c \.]
                                       [box-r (inc box-c) \.]
                                       [(+ box-r dr) box-c \[]
                                       [(+ box-r dr) (inc box-c) \]]])
                                    sorted-boxes)
                all-updates (concat box-updates [[r c \.] [nr nc \@]])]
            [(grid-set-multi grid all-updates) [nr nc]])))

      :else
      [grid robot-pos])))

(defn part2 [input-text]
  (let [[grid moves] (parse-input input-text)
        scaled-grid (scale-grid grid)
        initial-pos (find-robot scaled-grid)
        [final-grid _] (reduce (fn [[g pos] move]
                                 (move-robot-wide g pos move))
                               [scaled-grid initial-pos]
                               moves)]
    (calculate-gps final-grid \[)))

(defn -main []
  (let [input-text (slurp "../input.txt")]
    (println "Part 1:" (part1 input-text))
    (println "Part 2:" (part2 input-text))))

(-main)
