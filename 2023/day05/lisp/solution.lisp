;;;; Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer
;;;; Solution in Common Lisp

(defun read-input (filename)
  "Read and return the entire file contents as a string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-string (string separator)
  "Split a string by a separator string."
  (let ((result '())
        (start 0)
        (sep-len (length separator)))
    (loop for pos = (search separator string :start2 start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (+ pos sep-len)))
    (push (subseq string start) result)
    (nreverse result)))

(defun trim-whitespace (string)
  "Remove leading and trailing whitespace from a string."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-numbers (line)
  "Parse a line of space-separated numbers."
  (let ((numbers '())
        (start 0)
        (len (length line)))
    (loop while (< start len)
          do (let ((end start))
               ;; Skip leading spaces
               (loop while (and (< start len)
                               (char= (char line start) #\Space))
                     do (incf start))
               (setf end start)
               ;; Find end of number
               (loop while (and (< end len)
                               (not (char= (char line end) #\Space)))
                     do (incf end))
               (when (> end start)
                 (let ((num-str (subseq line start end)))
                   (push (parse-integer num-str) numbers)))
               (setf start end)))
    (nreverse numbers)))

(defun parse-input (text)
  "Parse input into seeds and list of maps."
  (let* ((sections (split-string (trim-whitespace text) (format nil "~%~%")))
         (seed-line (first sections))
         (seeds (parse-numbers (subseq seed-line (1+ (position #\: seed-line)))))
         (maps '()))
    ;; Parse each map section
    (dolist (section (rest sections))
      (let* ((lines (split-string (trim-whitespace section) (format nil "~%")))
             (ranges '()))
        ;; Skip header line, parse ranges
        (dolist (line (rest lines))
          (let ((nums (parse-numbers line)))
            (when (= (length nums) 3)
              (push (list (first nums) (second nums) (third nums)) ranges))))
        (push (nreverse ranges) maps)))
    (values seeds (nreverse maps))))

(defun apply-map (value ranges)
  "Apply a single map to transform a value."
  (dolist (range ranges value)
    (let ((dst-start (first range))
          (src-start (second range))
          (length (third range)))
      (when (and (>= value src-start)
                 (< value (+ src-start length)))
        (return-from apply-map (+ dst-start (- value src-start)))))))

(defun seed-to-location (seed maps)
  "Convert a seed number to a location number through all maps."
  (let ((value seed))
    (dolist (map-ranges maps value)
      (setf value (apply-map value map-ranges)))))

(defun part1 (seeds maps)
  "Find the lowest location number for any initial seed."
  (loop for seed in seeds
        minimize (seed-to-location seed maps)))

(defun apply-map-to-ranges (input-ranges map-ranges)
  "Apply a map to a list of ranges, returning new ranges."
  (let ((result '()))
    (dolist (input-range input-ranges)
      (let ((start (first input-range))
            (end (second input-range))
            (remaining (list (list (first input-range) (second input-range)))))
        (declare (ignore start end))
        (dolist (map-range map-ranges)
          (let ((dst-start (first map-range))
                (src-start (second map-range))
                (length (third map-range)))
            (let ((src-end (+ src-start length))
                  (new-remaining '()))
              (dolist (rem remaining)
                (let ((r-start (first rem))
                      (r-end (second rem)))
                  ;; Part before the map range (unmapped)
                  (when (< r-start src-start)
                    (push (list r-start (min r-end src-start)) new-remaining))
                  ;; Part within the map range (mapped)
                  (let ((overlap-start (max r-start src-start))
                        (overlap-end (min r-end src-end)))
                    (when (< overlap-start overlap-end)
                      (let ((offset (- dst-start src-start)))
                        (push (list (+ overlap-start offset) (+ overlap-end offset)) result))))
                  ;; Part after the map range (unmapped)
                  (when (> r-end src-end)
                    (push (list (max r-start src-end) r-end) new-remaining))))
              (setf remaining new-remaining))))
        ;; Any remaining parts are unmapped (identity)
        (dolist (rem remaining)
          (push rem result))))
    result))

(defun part2 (seeds maps)
  "Find the lowest location for seed ranges."
  ;; Convert seeds to ranges: pairs of (start, start + length)
  (let ((ranges '()))
    (loop for i from 0 below (length seeds) by 2
          for start = (nth i seeds)
          for length = (nth (1+ i) seeds)
          do (push (list start (+ start length)) ranges))
    (setf ranges (nreverse ranges))
    ;; Apply each map to the ranges
    (dolist (map-ranges maps)
      (setf ranges (apply-map-to-ranges ranges map-ranges)))
    ;; Find minimum start of any range
    (loop for range in ranges
          minimize (first range))))

(defun main ()
  (let* ((input-path (merge-pathnames "../input.txt" *load-truename*))
         (text (read-input input-path)))
    (multiple-value-bind (seeds maps) (parse-input text)
      (format t "Part 1: ~A~%" (part1 seeds maps))
      (format t "Part 2: ~A~%" (part2 seeds maps)))))

(main)
