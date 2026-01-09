(ns solution
  (:require [clojure.string :as str]))

(defn parse-input [text]
  (let [pattern #"Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."]
    (->> (str/split-lines text)
         (keep #(when-let [m (re-matches pattern %)]
                  (mapv parse-long (rest m))))
         vec)))

(defn max-geodes [bp time-limit]
  (let [[_bp-id ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs] bp
        ;; Max robots needed per type
        max-ore (max ore-ore clay-ore obs-ore geo-ore)
        max-clay obs-clay
        max-obs geo-obs
        ;; Atom for tracking best result
        best (atom 0)
        ;; HashMap for memoization
        seen (java.util.HashMap.)]

    (letfn [(dfs [time ore clay obs geodes ore-r clay-r obs-r geo-r]
              ;; Pruning: upper bound on possible geodes
              (let [remaining (- time-limit time)
                    upper-bound (+ geodes
                                   (* geo-r remaining)
                                   (quot (* remaining (dec remaining)) 2))]
                (when (> upper-bound @best)
                  (if (= time time-limit)
                    ;; At end, update best
                    (swap! best max geodes)
                    ;; Continue search
                    (let [;; Cap resources to avoid state explosion
                          capped-ore (min ore (* remaining max-ore))
                          capped-clay (min clay (* remaining max-clay))
                          capped-obs (min obs (* remaining max-obs))
                          ;; State key for deduplication
                          state-key (str time "," capped-ore "," capped-clay "," capped-obs ","
                                         ore-r "," clay-r "," obs-r "," geo-r)
                          prev (.get seen state-key)]
                      ;; Check memoization
                      (when (or (nil? prev) (> geodes prev))
                        (.put seen state-key geodes)
                        ;; Collect resources
                        (let [new-ore (+ capped-ore ore-r)
                              new-clay (+ capped-clay clay-r)
                              new-obs (+ capped-obs obs-r)
                              new-geodes (+ geodes geo-r)]
                          ;; Try building geode robot (always do if possible)
                          (if (and (>= capped-ore geo-ore) (>= capped-obs geo-obs))
                            ;; If we can build geode, always do it and return
                            (dfs (inc time)
                                 (- new-ore geo-ore) new-clay (- new-obs geo-obs) new-geodes
                                 ore-r clay-r obs-r (inc geo-r))
                            ;; Otherwise try other options
                            (do
                              ;; Try building obsidian robot
                              (when (and (>= capped-ore obs-ore)
                                         (>= capped-clay obs-clay)
                                         (< obs-r max-obs))
                                (dfs (inc time)
                                     (- new-ore obs-ore) (- new-clay obs-clay) new-obs new-geodes
                                     ore-r clay-r (inc obs-r) geo-r))
                              ;; Try building clay robot
                              (when (and (>= capped-ore clay-ore)
                                         (< clay-r max-clay))
                                (dfs (inc time)
                                     (- new-ore clay-ore) new-clay new-obs new-geodes
                                     ore-r (inc clay-r) obs-r geo-r))
                              ;; Try building ore robot
                              (when (and (>= capped-ore ore-ore)
                                         (< ore-r max-ore))
                                (dfs (inc time)
                                     (- new-ore ore-ore) new-clay new-obs new-geodes
                                     (inc ore-r) clay-r obs-r geo-r))
                              ;; Do nothing (wait)
                              (dfs (inc time)
                                   new-ore new-clay new-obs new-geodes
                                   ore-r clay-r obs-r geo-r))))))))))]
      ;; Start DFS with initial state
      (dfs 0 0 0 0 0 1 0 0 0))
    @best))

(defn part1 [blueprints]
  (reduce + (map (fn [bp]
                   (let [geodes (max-geodes bp 24)]
                     (* (first bp) geodes)))
                 blueprints)))

(defn part2 [blueprints]
  (reduce * (map #(max-geodes % 32) (take 3 blueprints))))

(defn -main []
  (let [input (slurp "../input.txt")
        blueprints (parse-input input)]
    (println "Part 1:" (part1 blueprints))
    (println "Part 2:" (part2 blueprints))))

(-main)
