#!/usr/bin/env bb
;; Day 20: Grove Positioning System

(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [text]
  (->> (str/split-lines (str/trim text))
       (mapv parse-long)))

(defn find-position [indexed orig-idx]
  "Find the current position of element with original index orig-idx."
  (loop [pos 0]
    (if (= (first (indexed pos)) orig-idx)
      pos
      (recur (inc pos)))))

(defn mix-once [indexed n]
  "Perform one round of mixing on the indexed vector."
  (loop [indexed indexed
         orig-idx 0]
    (if (>= orig-idx n)
      indexed
      (let [curr-pos (find-position indexed orig-idx)
            [_ val] (indexed curr-pos)
            ;; Remove element from current position
            without (into (subvec indexed 0 curr-pos)
                          (subvec indexed (inc curr-pos)))
            ;; Calculate new position modulo (n-1) since element is removed
            new-pos (mod (+ curr-pos val) (dec n))
            ;; Handle negative modulo properly
            new-pos (if (neg? new-pos)
                      (+ new-pos (dec n))
                      new-pos)
            ;; Insert at new position
            with-element (into (subvec without 0 new-pos)
                               (cons [orig-idx val]
                                     (subvec without new-pos)))]
        (recur with-element (inc orig-idx))))))

(defn mix [numbers times]
  "Mix the list of numbers the specified number of times."
  (let [n (count numbers)
        ;; Create indexed pairs [original-index, value]
        indexed (vec (map-indexed vector numbers))]
    (loop [indexed indexed
           t 0]
      (if (>= t times)
        (mapv second indexed)
        (recur (mix-once indexed n) (inc t))))))

(defn grove-coordinates [mixed]
  "Find sum of 1000th, 2000th, 3000th values after 0."
  (let [n (count mixed)
        zero-idx (.indexOf mixed 0)]
    (reduce + (map #(mixed (mod (+ zero-idx %) n)) [1000 2000 3000]))))

(defn part1 [text]
  "Mix once and find grove coordinates."
  (let [numbers (parse-input text)
        mixed (mix numbers 1)]
    (grove-coordinates mixed)))

(defn part2 [text]
  "Multiply by decryption key, mix 10 times."
  (let [numbers (parse-input text)
        decryption-key 811589153
        scaled (mapv #(* % decryption-key) numbers)
        mixed (mix scaled 10)]
    (grove-coordinates mixed)))

(defn -main []
  (let [script-dir (-> *file*
                       (java.io.File.)
                       (.getParentFile)
                       (.getCanonicalPath))
        input-file (str script-dir "/../input.txt")
        text (slurp input-file)]
    (println "Part 1:" (part1 text))
    (println "Part 2:" (part2 text))))

(-main)
