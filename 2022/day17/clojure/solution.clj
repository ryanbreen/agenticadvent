#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

;; Rock shapes as vectors of [dx dy] offsets from bottom-left
(def rocks
  [[[0 0] [1 0] [2 0] [3 0]]           ; Horizontal line
   [[1 0] [0 1] [1 1] [2 1] [1 2]]     ; Plus
   [[0 0] [1 0] [2 0] [2 1] [2 2]]     ; L shape
   [[0 0] [0 1] [0 2] [0 3]]           ; Vertical line
   [[0 0] [1 0] [0 1] [1 1]]])         ; Square

(def width 7)

(defn parse-input [text]
  "Parse jet pattern from input."
  (str/trim text))

(defn can-move? [occupied rock x y]
  "Check if rock at position (x,y) fits without collision."
  (every? (fn [[dx dy]]
            (let [nx (+ x dx)
                  ny (+ y dy)]
              (and (>= nx 0)
                   (< nx width)
                   (>= ny 0)
                   (not (contains? occupied [nx ny])))))
          rock))

(defn place-rock [occupied rock x y]
  "Place rock into occupied set and return new set and max height."
  (reduce (fn [[occ max-h] [dx dy]]
            (let [nx (+ x dx)
                  ny (+ y dy)]
              [(conj occ [nx ny]) (max max-h (inc ny))]))
          [occupied 0]
          rock))

(defn get-surface-profile [occupied height]
  "Get surface profile for cycle detection - relative heights of top surface."
  (let [profile-depth 30]
    (vec (for [col (range width)]
           (loop [row 0]
             (if (>= row profile-depth)
               profile-depth
               (if (contains? occupied [col (- height 1 row)])
                 row
                 (recur (inc row)))))))))

(defn simulate-rock [jets occupied height jet-idx rock]
  "Simulate a single rock falling. Returns [new-occupied new-height new-jet-idx]."
  (loop [x 2
         y (+ height 3)
         jet-idx jet-idx]
    (let [jet (nth jets jet-idx)
          next-jet-idx (mod (inc jet-idx) (count jets))
          dx (if (= jet \>) 1 -1)
          ;; Try horizontal movement
          new-x (if (can-move? occupied rock (+ x dx) y)
                  (+ x dx)
                  x)]
      ;; Try falling down
      (if (can-move? occupied rock new-x (dec y))
        (recur new-x (dec y) next-jet-idx)
        ;; Rock stops
        (let [[new-occupied rock-max-h] (place-rock occupied rock new-x y)
              new-height (max height rock-max-h)]
          [new-occupied new-height next-jet-idx])))))

(defn simulate [jets num-rocks]
  "Simulate falling rocks and return final height."
  (loop [occupied #{}
         height 0
         jet-idx 0
         rock-num 0
         states {}
         heights []]
    (if (>= rock-num num-rocks)
      height
      (let [rock-type (mod rock-num 5)
            rock (nth rocks rock-type)
            [new-occupied new-height new-jet-idx] (simulate-rock jets occupied height jet-idx rock)
            new-heights (conj heights new-height)]
        ;; Cycle detection for Part 2
        (if (> num-rocks 10000)
          (let [profile (get-surface-profile new-occupied new-height)
                state [rock-type new-jet-idx profile]]
            (if (contains? states state)
              ;; Found cycle
              (let [cycle-start (get states state)
                    cycle-len (- rock-num cycle-start)
                    cycle-height (- new-height (nth heights cycle-start))
                    remaining (- num-rocks rock-num 1)
                    full-cycles (quot remaining cycle-len)
                    leftover (mod remaining cycle-len)
                    final-height (+ new-height (* full-cycles cycle-height))]
                (if (> leftover 0)
                  (+ final-height (- (nth heights (+ cycle-start leftover)) (nth heights cycle-start)))
                  final-height))
              ;; No cycle yet, continue
              (recur new-occupied new-height new-jet-idx (inc rock-num)
                     (assoc states state rock-num) new-heights)))
          ;; Part 1, no cycle detection needed
          (recur new-occupied new-height new-jet-idx (inc rock-num) states new-heights))))))

(defn part1 [text]
  "Simulate 2022 rocks."
  (let [jets (parse-input text)]
    (simulate jets 2022)))

(defn part2 [text]
  "Simulate 1000000000000 rocks with cycle detection."
  (let [jets (parse-input text)]
    (simulate jets 1000000000000)))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
