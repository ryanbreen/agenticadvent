#!/usr/bin/env sbcl --script

(defun read-input-file (filename)
  "Read the entire input file and return lines as a list of strings."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (loop with result = '()
        with current = (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
        for char across string
        do (if (char= char delimiter)
               (progn
                 (push (copy-seq current) result)
                 (setf (fill-pointer current) 0))
               (vector-push-extend char current))
        finally (push (copy-seq current) result)
                (return (nreverse result))))

(defun parse-range (line)
  "Parse a range line like '3-5' into (start . end) cons."
  (let* ((parts (split-string line #\-))
         (start (parse-integer (first parts)))
         (end (parse-integer (second parts))))
    (cons start end)))

(defun find-blank-line-index (lines)
  "Find the index of the first blank line."
  (position "" lines :test #'string=))

(defun in-range-p (id range)
  "Check if an ID falls within a range (inclusive)."
  (let ((start (car range))
        (end (cdr range)))
    (and (>= id start) (<= id end))))

(defun part1 (lines)
  "Count how many available IDs fall within ANY range."
  (let* ((blank-idx (find-blank-line-index lines))
         (range-lines (subseq lines 0 blank-idx))
         (id-lines (remove-if #'(lambda (s) (string= s ""))
                              (subseq lines (1+ blank-idx))))
         (ranges (mapcar #'parse-range range-lines))
         (ingredient-ids (mapcar #'parse-integer id-lines))
         (fresh-count 0))

    ;; Count how many ingredient IDs fall within any range
    (dolist (ingredient-id ingredient-ids)
      (when (some #'(lambda (range) (in-range-p ingredient-id range)) ranges)
        (incf fresh-count)))

    fresh-count))

(defun merge-ranges (ranges)
  "Merge overlapping or adjacent ranges."
  ;; Sort ranges by start position
  (let ((sorted-ranges (sort (copy-list ranges) #'< :key #'car))
        (merged nil))

    (dolist (range sorted-ranges)
      (let ((start (car range))
            (end (cdr range)))
        (if (and merged
                 (<= start (1+ (cdr (first merged)))))
            ;; Overlapping or adjacent - merge with the last range
            (setf (cdr (first merged))
                  (max (cdr (first merged)) end))
            ;; No overlap - add as new range
            (push (cons start end) merged))))

    (nreverse merged)))

(defun part2 (lines)
  "Count total unique IDs covered by ALL ranges (merge overlapping ranges)."
  (let* ((blank-idx (find-blank-line-index lines))
         (range-lines (subseq lines 0 blank-idx))
         (ranges (mapcar #'parse-range range-lines))
         (merged (merge-ranges ranges))
         (total-count 0))

    ;; Count total unique IDs covered by merged ranges
    (dolist (range merged)
      (let ((start (car range))
            (end (cdr range)))
        (incf total-count (1+ (- end start)))))

    total-count))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt"
                                      (make-pathname :directory
                                                     (pathname-directory *load-pathname*))))
         (lines (read-input-file input-file)))

    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

;; Run main
(main)
