#!/usr/bin/env clojure -M

(require '[clojure.string :as str])

(defn parse-rule
  "Parse a single rule like 'x>10:one' or 'A' (default)."
  [rule-str]
  (if (str/includes? rule-str ":")
    (let [[condition destination] (str/split rule-str #":")
          attr (keyword (subs condition 0 1))
          op (subs condition 1 2)
          value (parse-long (subs condition 2))]
      {:attr attr :op op :value value :dest destination})
    {:attr nil :op nil :value nil :dest rule-str}))

(defn parse-workflow
  "Parse a workflow line like 'px{a<2006:qkq,m>2090:A,rfg}'."
  [line]
  (let [[name rules-str] (str/split line #"\{")
        rules-str (subs rules-str 0 (dec (count rules-str)))
        rules (mapv parse-rule (str/split rules-str #","))]
    [name rules]))

(defn parse-part
  "Parse a part line like '{x=787,m=2655,a=1222,s=2876}'."
  [line]
  (let [content (subs line 1 (dec (count line)))
        pairs (str/split content #",")]
    (into {}
          (for [pair pairs
                :let [[k v] (str/split pair #"=")]]
            [(keyword k) (parse-long v)]))))

(defn parse-input
  "Parse the input file into workflows map and parts vector."
  [filename]
  (let [text (slurp filename)
        [workflow-section parts-section] (str/split text #"\n\n")
        workflows (into {} (map parse-workflow (str/split-lines workflow-section)))
        parts (mapv parse-part (str/split-lines (str/trim parts-section)))]
    [workflows parts]))

(defn process-part
  "Process a part through workflows, return true if accepted."
  [workflows part]
  (loop [current "in"]
    (cond
      (= current "A") true
      (= current "R") false
      :else
      (let [rules (get workflows current)]
        (recur
         (loop [rules rules]
           (let [{:keys [attr op value dest]} (first rules)]
             (cond
               (nil? attr) dest
               (and (= op "<") (< (get part attr) value)) dest
               (and (= op ">") (> (get part attr) value)) dest
               :else (recur (rest rules))))))))))

(defn part1
  "Sum ratings of accepted parts."
  [workflows parts]
  (->> parts
       (filter #(process-part workflows %))
       (map #(+ (:x %) (:m %) (:a %) (:s %)))
       (reduce + 0)))

(defn count-accepted
  "Count combinations of xmas values that lead to acceptance using range splitting."
  [workflows workflow ranges]
  (cond
    (= workflow "R") 0
    (= workflow "A") (->> (vals ranges)
                          (map (fn [[lo hi]] (max 0 (inc (- hi lo)))))
                          (reduce * 1))
    :else
    (let [rules (get workflows workflow)]
      (loop [rules rules
             ranges ranges
             total 0]
        (if (empty? rules)
          total
          (let [{:keys [attr op value dest]} (first rules)]
            (if (nil? attr)
              ;; Default rule
              (+ total (count-accepted workflows dest ranges))
              ;; Conditional rule
              (let [[lo hi] (get ranges attr)]
                (if (= op "<")
                  ;; Split: [lo, value-1] goes to dest, [value, hi] continues
                  (let [matches? (< lo value)
                        continues? (>= hi value)
                        match-count (if matches?
                                      (count-accepted workflows dest
                                                      (assoc ranges attr [lo (min hi (dec value))]))
                                      0)]
                    (if continues?
                      (recur (rest rules)
                             (assoc ranges attr [(max lo value) hi])
                             (+ total match-count))
                      (+ total match-count)))
                  ;; op == ">"
                  ;; Split: [value+1, hi] goes to dest, [lo, value] continues
                  (let [matches? (> hi value)
                        continues? (<= lo value)
                        match-count (if matches?
                                      (count-accepted workflows dest
                                                      (assoc ranges attr [(max lo (inc value)) hi]))
                                      0)]
                    (if continues?
                      (recur (rest rules)
                             (assoc ranges attr [lo (min hi value)])
                             (+ total match-count))
                      (+ total match-count))))))))))))

(defn part2
  "Count all possible accepted combinations (1-4000 for each rating)."
  [workflows]
  (let [initial-ranges {:x [1 4000]
                        :m [1 4000]
                        :a [1 4000]
                        :s [1 4000]}]
    (count-accepted workflows "in" initial-ranges)))

(defn -main []
  (let [input-file (str (.getParent (java.io.File. *file*)) "/../input.txt")
        [workflows parts] (parse-input input-file)]
    (println "Part 1:" (part1 workflows parts))
    (println "Part 2:" (part2 workflows))))

(-main)
