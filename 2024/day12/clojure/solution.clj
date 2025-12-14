(require '[clojure.string :as str])

;; Direction vectors for up, down, left, right
(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn parse-grid [input-text]
  "Parse input text into a 2D grid vector."
  (mapv vec (str/split-lines input-text)))

(defn make-grid-context [grid]
  "Create a context map with grid and its dimensions."
  {:grid grid
   :rows (count grid)
   :cols (count (first grid))})

(defn in-bounds? [{:keys [rows cols]} r c]
  "Check if position is within grid bounds."
  (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

(defn get-cell [{:keys [grid] :as ctx} r c]
  "Get the cell value at position [r c]."
  (when (in-bounds? ctx r c)
    (get-in grid [r c])))

(defn bfs-region [ctx start-r start-c plant]
  "Find all cells in a region starting from [start-r start-c] using BFS.
   Returns a set of [r c] coordinates in the region."
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start-r start-c])
         visited #{}
         region #{}]
    (if (empty? queue)
      region
      (let [[cr cc] (peek queue)
            queue' (pop queue)]
        (cond
          ;; Already visited
          (visited [cr cc])
          (recur queue' visited region)

          ;; Out of bounds or wrong plant type
          (or (not (in-bounds? ctx cr cc))
              (not= (get-cell ctx cr cc) plant))
          (recur queue' visited region)

          ;; Valid cell in region
          :else
          (let [visited' (conj visited [cr cc])
                region' (conj region [cr cc])
                ;; Add unvisited neighbors to queue
                neighbors (sequence
                            (comp
                              (map (fn [[dr dc]] [(+ cr dr) (+ cc dc)]))
                              (remove visited'))
                            directions)
                queue'' (reduce conj queue' neighbors)]
            (recur queue'' visited' region')))))))

(defn find-all-grid-positions [ctx]
  "Generate all grid positions as a lazy sequence."
  (for [r (range (:rows ctx))
        c (range (:cols ctx))]
    [r c]))

(defn find-regions [ctx]
  "Find all connected regions in the grid using BFS.
   Separates grid iteration from BFS logic."
  (loop [positions (find-all-grid-positions ctx)
         visited #{}
         regions []]
    (if-let [[r c] (first positions)]
      (if (visited [r c])
        (recur (rest positions) visited regions)
        (let [plant (get-cell ctx r c)
              region (bfs-region ctx r c plant)
              visited' (into visited region)]
          (recur (rest positions) visited' (conj regions region))))
      regions)))

(defn calculate-perimeter [region]
  "Calculate perimeter of a region (edges not touching same region).
   Uses transducer for efficient computation."
  (transduce
    (comp
      (mapcat (fn [[r c]]
                (map (fn [[dr dc]] [(+ r dr) (+ c dc)]) directions)))
      (remove region)
      (map (constantly 1)))
    +
    region))

(defn count-corners-at-cell [region [r c]]
  "Count corners at a specific cell in the region."
  (let [up (region [(dec r) c])
        down (region [(inc r) c])
        left (region [r (dec c)])
        right (region [r (inc c)])
        up-left (region [(dec r) (dec c)])
        up-right (region [(dec r) (inc c)])
        down-left (region [(inc r) (dec c)])
        down-right (region [(inc r) (inc c)])]
    (cond-> 0
      ;; Top-left corner
      (or (and (not up) (not left))           ; convex
          (and up left (not up-left)))        ; concave
      inc

      ;; Top-right corner
      (or (and (not up) (not right))          ; convex
          (and up right (not up-right)))      ; concave
      inc

      ;; Bottom-left corner
      (or (and (not down) (not left))         ; convex
          (and down left (not down-left)))    ; concave
      inc

      ;; Bottom-right corner
      (or (and (not down) (not right))        ; convex
          (and down right (not down-right)))  ; concave
      inc)))

(defn count-sides [region]
  "Count number of sides (corners) in a region.
   A corner is either:
   - Convex: both orthogonal neighbors are outside the region
   - Concave: both orthogonal neighbors are inside, but diagonal is outside
   Uses transducer for efficient computation."
  (transduce
    (map #(count-corners-at-cell region %))
    +
    region))

(defn calculate-fencing-cost [regions metric-fn]
  "Generic function to calculate fencing cost using a given metric.
   Uses transducer for efficient computation."
  (transduce
    (map (fn [region]
           (let [area (count region)
                 metric (metric-fn region)]
             (* area metric))))
    +
    regions))

(defn part1 [ctx]
  "Calculate total fencing cost: sum of area * perimeter for each region."
  (let [regions (find-regions ctx)]
    (calculate-fencing-cost regions calculate-perimeter)))

(defn part2 [ctx]
  "Calculate total fencing cost using sides instead of perimeter."
  (let [regions (find-regions ctx)]
    (calculate-fencing-cost regions count-sides)))

;; Main
(let [input-text (slurp "../input.txt")
      grid (parse-grid input-text)
      ctx (make-grid-context grid)]
  (println "Part 1:" (part1 ctx))
  (println "Part 2:" (part2 ctx)))
