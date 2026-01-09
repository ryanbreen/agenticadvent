#!/usr/bin/env sbcl --script

;;; Day 19: Not Enough Minerals
;;; Robot factory optimization using DFS with aggressive pruning

(defstruct blueprint
  id ore-ore clay-ore obs-ore obs-clay geo-ore geo-obs)

(defun extract-numbers (line)
  "Extract all integers from a string."
  (let ((nums nil)
        (start nil))
    (dotimes (i (length line))
      (let ((c (char line i)))
        (if (digit-char-p c)
            (unless start (setf start i))
            (when start
              (push (parse-integer line :start start :end i) nums)
              (setf start nil)))))
    (when start
      (push (parse-integer line :start start) nums))
    (nreverse nums)))

(defun parse-blueprint (line)
  "Parse a single blueprint line into a blueprint struct."
  (let ((nums (extract-numbers line)))
    (when (>= (length nums) 7)
      (make-blueprint
       :id (nth 0 nums)
       :ore-ore (nth 1 nums)
       :clay-ore (nth 2 nums)
       :obs-ore (nth 3 nums)
       :obs-clay (nth 4 nums)
       :geo-ore (nth 5 nums)
       :geo-obs (nth 6 nums)))))

(defun read-input (filename)
  "Read and parse all blueprints from input file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          for bp = (parse-blueprint line)
          when bp collect bp)))

(defun max-geodes (bp time-limit)
  "Find maximum geodes producible by blueprint in time-limit minutes."
  (let* ((ore-ore (blueprint-ore-ore bp))
         (clay-ore (blueprint-clay-ore bp))
         (obs-ore (blueprint-obs-ore bp))
         (obs-clay (blueprint-obs-clay bp))
         (geo-ore (blueprint-geo-ore bp))
         (geo-obs (blueprint-geo-obs bp))
         ;; Max robots needed per type
         (max-ore (max ore-ore clay-ore obs-ore geo-ore))
         (max-clay obs-clay)
         (max-obs geo-obs)
         (best 0)
         (seen (make-hash-table :test #'equal)))

    (labels ((dfs (time ore clay obs geodes ore-r clay-r obs-r geo-r)
               ;; Pruning: upper bound on possible geodes
               (let* ((remaining (- time-limit time))
                      (upper-bound (+ geodes
                                      (* geo-r remaining)
                                      (floor (* remaining (1- remaining)) 2))))
                 (when (<= upper-bound best)
                   (return-from dfs nil)))

               ;; Time's up
               (when (= time time-limit)
                 (setf best (max best geodes))
                 (return-from dfs nil))

               ;; Cap resources at what we could possibly use
               (let* ((remaining (- time-limit time))
                      (capped-ore (min ore (* remaining max-ore)))
                      (capped-clay (min clay (* remaining max-clay)))
                      (capped-obs (min obs (* remaining max-obs)))
                      ;; State key for memoization
                      (key (list time capped-ore capped-clay capped-obs
                                 ore-r clay-r obs-r geo-r)))

                 ;; State deduplication
                 (let ((prev (gethash key seen)))
                   (when (and prev (>= prev geodes))
                     (return-from dfs nil)))
                 (setf (gethash key seen) geodes)

                 ;; Collect resources
                 (let ((new-ore (+ capped-ore ore-r))
                       (new-clay (+ capped-clay clay-r))
                       (new-obs (+ capped-obs obs-r))
                       (new-geodes (+ geodes geo-r)))

                   ;; Try building geode robot (always do if possible)
                   (when (and (>= capped-ore geo-ore) (>= capped-obs geo-obs))
                     (dfs (1+ time)
                          (- new-ore geo-ore) new-clay (- new-obs geo-obs)
                          new-geodes ore-r clay-r obs-r (1+ geo-r))
                     (return-from dfs nil)) ; If we can build geode, always do

                   ;; Try building obsidian robot
                   (when (and (>= capped-ore obs-ore)
                              (>= capped-clay obs-clay)
                              (< obs-r max-obs))
                     (dfs (1+ time)
                          (- new-ore obs-ore) (- new-clay obs-clay) new-obs
                          new-geodes ore-r clay-r (1+ obs-r) geo-r))

                   ;; Try building clay robot
                   (when (and (>= capped-ore clay-ore)
                              (< clay-r max-clay))
                     (dfs (1+ time)
                          (- new-ore clay-ore) new-clay new-obs
                          new-geodes ore-r (1+ clay-r) obs-r geo-r))

                   ;; Try building ore robot
                   (when (and (>= capped-ore ore-ore)
                              (< ore-r max-ore))
                     (dfs (1+ time)
                          (- new-ore ore-ore) new-clay new-obs
                          new-geodes (1+ ore-r) clay-r obs-r geo-r))

                   ;; Do nothing (wait)
                   (dfs (1+ time)
                        new-ore new-clay new-obs new-geodes
                        ore-r clay-r obs-r geo-r)))))

      (dfs 0 0 0 0 0 1 0 0 0)
      best)))

(defun part1 (blueprints)
  "Calculate sum of quality levels (blueprint-id * max-geodes in 24 min)."
  (loop for bp in blueprints
        sum (* (blueprint-id bp) (max-geodes bp 24))))

(defun part2 (blueprints)
  "Calculate product of max geodes for first 3 blueprints in 32 min."
  (let ((first-three (subseq blueprints 0 (min 3 (length blueprints)))))
    (reduce #'* (mapcar (lambda (bp) (max-geodes bp 32)) first-three))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-path (merge-pathnames "../input.txt" script-dir))
         (blueprints (read-input input-path)))
    (format t "Part 1: ~A~%" (part1 blueprints))
    (format t "Part 2: ~A~%" (part2 blueprints))))

(main)
