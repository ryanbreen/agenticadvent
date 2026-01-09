#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(def directions
  {"U" [0 1]
   "D" [0 -1]
   "L" [-1 0]
   "R" [1 0]})

(defn sign [x]
  (cond
    (pos? x) 1
    (neg? x) -1
    :else 0))

(defn move-tail
  "Move tail toward head if not adjacent."
  [[hx hy] [tx ty]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
      [tx ty]
      [(+ tx (sign dx)) (+ ty (sign dy))])))

(defn step-rope
  "Move all knots one step, head moves by delta."
  [knots [dx dy]]
  (let [new-head [(+ (first (first knots)) dx)
                  (+ (second (first knots)) dy)]]
    (reduce
     (fn [new-knots knot]
       (conj new-knots (move-tail (last new-knots) knot)))
     [new-head]
     (rest knots))))

(defn simulate-rope
  "Simulate rope with given length and return count of unique tail positions."
  [moves rope-length]
  (let [initial-knots (vec (repeat rope-length [0 0]))]
    (loop [lines moves
           knots initial-knots
           visited #{[0 0]}]
      (if (empty? lines)
        (count visited)
        (let [[dir cnt] (str/split (first lines) #" ")
              count-steps (parse-long cnt)
              [dx dy] (directions dir)
              ;; Process all steps for this move
              [new-knots new-visited]
              (loop [steps count-steps
                     k knots
                     v visited]
                (if (zero? steps)
                  [k v]
                  (let [new-k (step-rope k [dx dy])]
                    (recur (dec steps)
                           new-k
                           (conj v (last new-k))))))]
          (recur (rest lines) new-knots new-visited))))))

(defn part1 [moves]
  (simulate-rope moves 2))

(defn part2 [moves]
  (simulate-rope moves 10))

(defn -main []
  (let [script-dir (-> (java.io.File. *file*) .getParentFile .getAbsolutePath)
        input-file (str script-dir "/../input.txt")
        moves (-> (slurp input-file) str/trim (str/split #"\n"))]
    (println "Part 1:" (part1 moves))
    (println "Part 2:" (part2 moves))))

(-main)
