#!/usr/bin/env clojure -M

;; Day 20: Pulse Propagation - Module communication simulation

(require '[clojure.string :as str])

(defn parse-line [line]
  "Parse a single module configuration line."
  (let [[name-part dest-part] (str/split line #" -> ")
        destinations (mapv str/trim (str/split dest-part #","))]
    (cond
      (= name-part "broadcaster")
      ["broadcaster" {:type :broadcaster :destinations destinations}]

      (str/starts-with? name-part "%")
      [(subs name-part 1) {:type :flip-flop :destinations destinations :state false}]

      (str/starts-with? name-part "&")
      [(subs name-part 1) {:type :conjunction :destinations destinations :memory {}}]

      :else
      [name-part {:type :output :destinations destinations}])))

(defn parse-input [filename]
  "Parse module configuration from input file."
  (let [lines (str/split-lines (str/trim (slurp filename)))
        modules (into {} (map parse-line lines))]
    ;; Initialize conjunction memory for all inputs
    (reduce (fn [mods [name module]]
              (reduce (fn [m dest]
                        (if (and (contains? m dest)
                                 (= :conjunction (get-in m [dest :type])))
                          (assoc-in m [dest :memory name] false)
                          m))
                      mods
                      (:destinations module)))
            modules
            modules)))

(defn process-pulse [modules source dest pulse watch-nodes]
  "Process a single pulse and return [updated-modules new-pulses high-sender]."
  (let [high-sender (when (and (contains? watch-nodes source) pulse) source)]
    (if-not (contains? modules dest)
      [modules [] high-sender]
      (let [module (get modules dest)]
        (case (:type module)
          :broadcaster
          [modules
           (mapv #(vector dest % pulse) (:destinations module))
           high-sender]

          :flip-flop
          (if pulse
            ;; High pulse - ignore
            [modules [] high-sender]
            ;; Low pulse - toggle state
            (let [new-state (not (:state module))
                  updated-modules (assoc-in modules [dest :state] new-state)]
              [updated-modules
               (mapv #(vector dest % new-state) (:destinations module))
               high-sender]))

          :conjunction
          (let [updated-memory (assoc (:memory module) source pulse)
                updated-modules (assoc-in modules [dest :memory] updated-memory)
                output (not (every? true? (vals updated-memory)))]
            [updated-modules
             (mapv #(vector dest % output) (:destinations module))
             high-sender])

          ;; Default (output or unknown)
          [modules [] high-sender])))))

(defn simulate-button-press [modules watch-nodes]
  "Simulate a single button press.
   Returns [updated-modules low-count high-count high-senders]."
  (loop [mods modules
         queue (conj clojure.lang.PersistentQueue/EMPTY ["button" "broadcaster" false])
         low-count 0
         high-count 0
         high-senders #{}]
    (if (empty? queue)
      [mods low-count high-count high-senders]
      (let [[source dest pulse] (peek queue)
            remaining (pop queue)
            [new-low new-high] (if pulse [low-count (inc high-count)] [(inc low-count) high-count])
            [updated-mods new-pulses high-sender] (process-pulse mods source dest pulse watch-nodes)
            updated-senders (if high-sender (conj high-senders high-sender) high-senders)]
        (recur updated-mods
               (into remaining new-pulses)
               new-low
               new-high
               updated-senders)))))

(defn reset-state [modules]
  "Reset all module states to initial values."
  (reduce (fn [mods [name module]]
            (case (:type module)
              :flip-flop (assoc-in mods [name :state] false)
              :conjunction (assoc-in mods [name :memory]
                                     (into {} (map (fn [[k _]] [k false]) (:memory module))))
              mods))
          modules
          modules))

(defn part1 [modules]
  "Part 1: Count pulses after 1000 button presses."
  (loop [mods (reset-state modules)
         presses 0
         total-low 0
         total-high 0]
    (if (>= presses 1000)
      (* total-low total-high)
      (let [[new-mods low high _] (simulate-button-press mods #{})]
        (recur new-mods (inc presses) (+ total-low low) (+ total-high high))))))

(defn find-rx-input [modules]
  "Find the module that feeds into rx."
  (first (for [[name module] modules
               :when (some #(= % "rx") (:destinations module))]
           name)))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn part2 [modules]
  "Part 2: Find minimum button presses for rx to receive a low pulse.
   rx receives from a conjunction. For it to send low, all its inputs
   must have sent high. Find the cycle length for each input and compute LCM."
  (let [rx-input (find-rx-input modules)]
    (if (nil? rx-input)
      0
      (let [watch-nodes (set (keys (get-in modules [rx-input :memory])))]
        (loop [mods (reset-state modules)
               button-press 0
               cycle-lengths {}]
          (if (= (count cycle-lengths) (count watch-nodes))
            (reduce lcm 1 (vals cycle-lengths))
            (let [press (inc button-press)
                  [new-mods _ _ high-senders] (simulate-button-press mods watch-nodes)
                  new-cycles (reduce (fn [cycles node]
                                       (if (contains? cycles node)
                                         cycles
                                         (assoc cycles node press)))
                                     cycle-lengths
                                     high-senders)]
              (recur new-mods press new-cycles))))))))

(defn -main []
  (let [input-file (str (-> *file* java.io.File. .getParent) "/../input.txt")
        modules (parse-input input-file)]
    (println "Part 1:" (part1 modules))
    (println "Part 2:" (part2 (parse-input input-file)))))

(-main)
