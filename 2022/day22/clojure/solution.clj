#!/usr/bin/env bb

(ns solution
  (:require [clojure.string :as str]))

(defn parse-instructions [path]
  "Parse path instructions into a sequence of moves (integers) and turns (keywords)."
  (loop [remaining path
         result []]
    (if (empty? remaining)
      result
      (let [ch (first remaining)]
        (cond
          (Character/isDigit ch)
          (let [digits (take-while #(Character/isDigit %) remaining)
                num (Integer/parseInt (apply str digits))]
            (recur (drop (count digits) remaining)
                   (conj result num)))

          (= ch \R)
          (recur (rest remaining) (conj result :R))

          (= ch \L)
          (recur (rest remaining) (conj result :L))

          :else
          (recur (rest remaining) result))))))

(defn parse-input [text]
  "Parse the map and path instructions."
  (let [parts (str/split text #"\n\n")
        grid-lines (str/split-lines (first parts))
        path (str/trim (second parts))
        height (count grid-lines)
        width (apply max (map count grid-lines))
        ;; Pad lines to consistent width
        grid (mapv (fn [line]
                     (let [padded (str line (apply str (repeat (- width (count line)) \space)))]
                       padded))
                   grid-lines)
        instructions (parse-instructions path)]
    {:grid grid
     :height height
     :width width
     :instructions instructions}))

(def DR [0 1 0 -1])  ; right, down, left, up
(def DC [1 0 -1 0])

(defn find-start [grid]
  "Find the starting position (leftmost open tile on top row)."
  [0 (str/index-of (first grid) \.)])

(defn get-cell [grid row col]
  "Get character at position, or space if out of bounds."
  (if (and (>= row 0) (< row (count grid))
           (>= col 0) (< col (count (nth grid row))))
    (nth (nth grid row) col)
    \space))

(defn wrap-flat [grid row col facing height width]
  "Wrap around using flat 2D wrapping."
  (case facing
    0 ;; Right - find leftmost non-space on this row
    (loop [nc 0]
      (if (= (get-cell grid row nc) \space)
        (recur (inc nc))
        [row nc]))

    2 ;; Left - find rightmost non-space on this row
    (loop [nc (dec width)]
      (if (= (get-cell grid row nc) \space)
        (recur (dec nc))
        [row nc]))

    1 ;; Down - find topmost non-space on this column
    (loop [nr 0]
      (if (= (get-cell grid nr col) \space)
        (recur (inc nr))
        [nr col]))

    3 ;; Up - find bottommost non-space on this column
    (loop [nr (dec height)]
      (if (= (get-cell grid nr col) \space)
        (recur (dec nr))
        [nr col]))))

(defn part1 [{:keys [grid height width instructions]}]
  "Navigate the map with 2D flat wrapping."
  (let [[start-row start-col] (find-start grid)]
    (loop [row start-row
           col start-col
           facing 0
           instrs instructions]
      (if (empty? instrs)
        ;; Calculate password: 1000*row + 4*col + facing (1-indexed)
        (+ (* 1000 (inc row)) (* 4 (inc col)) facing)
        (let [instr (first instrs)]
          (if (keyword? instr)
            ;; Turn
            (let [new-facing (case instr
                               :R (mod (inc facing) 4)
                               :L (mod (dec facing) 4))]
              (recur row col new-facing (rest instrs)))
            ;; Move forward
            (let [[final-row final-col]
                  (loop [r row c col steps instr]
                    (if (zero? steps)
                      [r c]
                      (let [dr (nth DR facing)
                            dc (nth DC facing)
                            nr (+ r dr)
                            nc (+ c dc)
                            cell (get-cell grid nr nc)
                            ;; Check if we need to wrap
                            [wr wc] (if (or (< nr 0) (>= nr height)
                                            (< nc 0) (>= nc width)
                                            (= cell \space))
                                      (wrap-flat grid r c facing height width)
                                      [nr nc])
                            wrapped-cell (get-cell grid wr wc)]
                        (if (= wrapped-cell \#)
                          ;; Hit a wall, stop
                          [r c]
                          ;; Move to new position
                          (recur wr wc (dec steps))))))]
              (recur final-row final-col facing (rest instrs)))))))))

(defn get-cube-face-and-local [row col face-size]
  "Determine which face and local coordinates based on the cube layout:
     12
     3
    45
    6"
  (let [face-row (quot row face-size)
        face-col (quot col face-size)
        local-r (mod row face-size)
        local-c (mod col face-size)]
    (cond
      (and (= face-row 0) (= face-col 1)) [1 local-r local-c]
      (and (= face-row 0) (= face-col 2)) [2 local-r local-c]
      (and (= face-row 1) (= face-col 1)) [3 local-r local-c]
      (and (= face-row 2) (= face-col 0)) [4 local-r local-c]
      (and (= face-row 2) (= face-col 1)) [5 local-r local-c]
      (and (= face-row 3) (= face-col 0)) [6 local-r local-c]
      :else [-1 local-r local-c])))

(defn wrap-cube [row col facing face-size]
  "Handle cube wrapping for the actual input layout."
  (let [S face-size
        [face lr lc] (get-cube-face-and-local row col S)]
    (case [face facing]
      ;; Face 1
      [1 3] [(+ (* 3 S) lc) 0 0]                    ; Up -> Face 6 left edge, facing right
      [1 2] [(- (* 3 S) 1 lr) 0 0]                  ; Left -> Face 4 left edge, facing right (inverted)

      ;; Face 2
      [2 0] [(- (* 3 S) 1 lr) (- (* 2 S) 1) 2]      ; Right -> Face 5 right edge, facing left (inverted)
      [2 1] [(+ S lc) (- (* 2 S) 1) 2]              ; Down -> Face 3 right edge, facing left
      [2 3] [(- (* 4 S) 1) lc 3]                    ; Up -> Face 6 bottom edge, facing up

      ;; Face 3
      [3 0] [(- S 1) (+ (* 2 S) lr) 3]              ; Right -> Face 2 bottom edge, facing up
      [3 2] [(* 2 S) lr 1]                          ; Left -> Face 4 top edge, facing down

      ;; Face 4
      [4 3] [(+ S lc) S 0]                          ; Up -> Face 3 left edge, facing right
      [4 2] [(- (- S 1) lr) S 0]                    ; Left -> Face 1 left edge, facing right (inverted)

      ;; Face 5
      [5 0] [(- (- S 1) lr) (- (* 3 S) 1) 2]        ; Right -> Face 2 right edge, facing left (inverted)
      [5 1] [(+ (* 3 S) lc) (- S 1) 2]              ; Down -> Face 6 right edge, facing left

      ;; Face 6
      [6 0] [(- (* 3 S) 1) (+ S lr) 3]              ; Right -> Face 5 bottom edge, facing up
      [6 1] [0 (+ (* 2 S) lc) 1]                    ; Down -> Face 2 top edge, facing down
      [6 2] [0 (+ S lr) 1]                          ; Left -> Face 1 top edge, facing down

      ;; Default - shouldn't happen
      [row col facing])))

(defn part2 [{:keys [grid height width instructions]}]
  "Navigate the map with cube wrapping."
  (let [face-size (if (> height 50) 50 4)
        [start-row start-col] (find-start grid)]
    (loop [row start-row
           col start-col
           facing 0
           instrs instructions]
      (if (empty? instrs)
        (+ (* 1000 (inc row)) (* 4 (inc col)) facing)
        (let [instr (first instrs)]
          (if (keyword? instr)
            (let [new-facing (case instr
                               :R (mod (inc facing) 4)
                               :L (mod (dec facing) 4))]
              (recur row col new-facing (rest instrs)))
            ;; Move forward
            (let [[final-row final-col final-facing]
                  (loop [r row c col f facing steps instr]
                    (if (zero? steps)
                      [r c f]
                      (let [dr (nth DR f)
                            dc (nth DC f)
                            nr (+ r dr)
                            nc (+ c dc)
                            cell (get-cell grid nr nc)
                            ;; Check if we need to wrap
                            need-wrap (or (< nr 0) (>= nr height)
                                          (< nc 0) (>= nc width)
                                          (= cell \space))
                            [wr wc wf] (if need-wrap
                                         (wrap-cube r c f face-size)
                                         [nr nc f])
                            wrapped-cell (get-cell grid wr wc)]
                        (if (= wrapped-cell \#)
                          [r c f]
                          (recur wr wc wf (dec steps))))))]
              (recur final-row final-col final-facing (rest instrs)))))))))

(defn -main []
  (let [input-file (str (-> *file* java.io.File. .getParent) "/../input.txt")
        text (slurp input-file)
        parsed (parse-input text)]
    (println "Part 1:" (part1 parsed))
    (println "Part 2:" (part2 parsed))))

(-main)
