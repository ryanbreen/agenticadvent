(ns solution
  (:require [clojure.string :as str]))

(defn parse-input []
  (let [input-path (str (.getParent (java.io.File. *file*)) "/../input.txt")]
    (->> (slurp input-path)
         str/split-lines
         (filter (complement str/blank?))
         (map (fn [line]
                (let [[cmd val] (str/split line #"\s+")]
                  [cmd (parse-long val)]))))))

(defn part1 [commands]
  (let [{:keys [horizontal depth]}
        (reduce (fn [{:keys [horizontal depth] :as state} [cmd val]]
                  (case cmd
                    "forward" (assoc state :horizontal (+ horizontal val))
                    "down"    (assoc state :depth (+ depth val))
                    "up"      (assoc state :depth (- depth val))
                    state))
                {:horizontal 0 :depth 0}
                commands)]
    (* horizontal depth)))

(defn part2 [commands]
  (let [{:keys [horizontal depth]}
        (reduce (fn [{:keys [horizontal depth aim] :as state} [cmd val]]
                  (case cmd
                    "forward" (-> state
                                  (assoc :horizontal (+ horizontal val))
                                  (assoc :depth (+ depth (* aim val))))
                    "down"    (assoc state :aim (+ aim val))
                    "up"      (assoc state :aim (- aim val))
                    state))
                {:horizontal 0 :depth 0 :aim 0}
                commands)]
    (* horizontal depth)))

(defn -main []
  (let [commands (parse-input)]
    (println "Part 1:" (part1 commands))
    (println "Part 2:" (part2 commands))))

(-main)
