(ns solution
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Long/parseLong %) (str/split (str/trim line) #"\s+")))

(defn parse-input [text]
  (->> (str/split-lines text)
       (filter #(not (str/blank? %)))
       (mapv parse-line)))

(defn get-differences [seq]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 seq)))

(defn all-zeros? [seq]
  (every? zero? seq))

(defn build-difference-pyramid [seq]
  (loop [current seq
         pyramid [seq]]
    (if (all-zeros? current)
      pyramid
      (let [diffs (get-differences current)]
        (recur diffs (conj pyramid diffs))))))

(defn extrapolate-next [seq]
  (let [pyramid (build-difference-pyramid seq)]
    (reduce (fn [acc level]
              (+ (last level) acc))
            0
            (reverse pyramid))))

(defn extrapolate-prev [seq]
  (let [pyramid (build-difference-pyramid seq)]
    (reduce (fn [acc level]
              (- (first level) acc))
            0
            (reverse pyramid))))

(defn part1 [histories]
  (reduce + (map extrapolate-next histories)))

(defn part2 [histories]
  (reduce + (map extrapolate-prev histories)))

(defn -main []
  (let [input-text (slurp "../input.txt")
        histories (parse-input input-text)]
    (println (str "Part 1: " (part1 histories)))
    (println (str "Part 2: " (part2 histories)))))

(-main)
