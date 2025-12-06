#!/usr/bin/env clojure

(require '[clojure.string :as str])

;; Read input from ../input.txt
(def input-text
  (str/trim (slurp "../input.txt")))

;; Parse a single draw like "3 blue, 4 red"
(defn parse-draw [draw-str]
  (let [parts (str/split (str/trim draw-str) #",\s*")]
    (reduce (fn [acc part]
              (let [[num color] (str/split (str/trim part) #"\s+")
                    count (Integer/parseInt num)]
                (assoc acc (keyword color) count)))
            {}
            parts)))

;; Parse a game line like "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue"
(defn parse-game [line]
  (let [[game-part draws-part] (str/split line #":\s*")
        game-id (Integer/parseInt (re-find #"\d+" game-part))
        draws (str/split draws-part #";\s*")
        parsed-draws (map parse-draw draws)]
    {:id game-id
     :draws parsed-draws}))

;; Part 1: Check if a game is possible with max constraints
(defn game-possible? [game max-red max-green max-blue]
  (every? (fn [draw]
            (and (<= (get draw :red 0) max-red)
                 (<= (get draw :green 0) max-green)
                 (<= (get draw :blue 0) max-blue)))
          (:draws game)))

(defn part1 []
  (let [games (map parse-game (str/split-lines input-text))
        max-red 12
        max-green 13
        max-blue 14
        possible-games (filter #(game-possible? % max-red max-green max-blue) games)]
    (reduce + (map :id possible-games))))

;; Part 2: Find minimum cubes needed for each game and calculate power
(defn min-cubes [game]
  (let [draws (:draws game)
        min-red (apply max (map #(get % :red 0) draws))
        min-green (apply max (map #(get % :green 0) draws))
        min-blue (apply max (map #(get % :blue 0) draws))]
    {:red min-red
     :green min-green
     :blue min-blue}))

(defn power [cubes]
  (* (:red cubes) (:green cubes) (:blue cubes)))

(defn part2 []
  (let [games (map parse-game (str/split-lines input-text))
        min-cube-sets (map min-cubes games)
        powers (map power min-cube-sets)]
    (reduce + powers)))

;; Main execution
(println "Part 1:" (part1))
(println "Part 2:" (part2))
