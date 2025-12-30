#!/usr/bin/env sbcl --script

(defun read-input-file (filename)
  "Read the entire file into a string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-lines (text)
  "Split text into lines, handling CR/LF variations."
  (loop with start = 0
        with len = (length text)
        for end = (or (position #\Newline text :start start) len)
        collect (string-trim '(#\Return) (subseq text start end))
        while (< end len)
        do (setf start (1+ end))))

(defun parse-input (text)
  "Parse input into list of patterns (each pattern is a list of strings).
   Patterns are separated by blank lines."
  (let ((patterns nil)
        (current-pattern nil))
    (dolist (line (split-lines (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
      (if (zerop (length line))
          (when current-pattern
            (push (nreverse current-pattern) patterns)
            (setf current-pattern nil))
          (push line current-pattern)))
    (when current-pattern
      (push (nreverse current-pattern) patterns))
    (nreverse patterns)))

(defun count-row-differences (row1 row2)
  "Count character differences between two rows."
  (loop for c1 across row1
        for c2 across row2
        count (char/= c1 c2)))

(defun count-mirror-differences (row col)
  "Count character differences for a mirrored comparison around col."
  (let ((compare-len (min col (- (length row) col))))
    (loop for offset from 0 below compare-len
          count (char/= (char row (- col offset 1))
                        (char row (+ col offset))))))

(defun count-vertical-differences (pattern col)
  "Count total differences at vertical reflection at given column."
  (loop for row in pattern
        sum (count-mirror-differences row col)))

(defun count-horizontal-differences (pattern-vec row height)
  "Count total differences at horizontal reflection at given row."
  (loop for i from 0 below (min row (- height row))
        sum (count-row-differences (aref pattern-vec (- row 1 i))
                                   (aref pattern-vec (+ row i)))))

(defun find-vertical-reflection (pattern target-diff)
  "Find vertical line of reflection with exactly target-diff total differences.
   Returns columns to the left, or 0 if none found."
  (if (null pattern)
      0
      (let ((width (length (first pattern))))
        (or (loop for col from 1 below width
                  when (= (count-vertical-differences pattern col) target-diff)
                    return col)
            0))))

(defun find-horizontal-reflection (pattern target-diff)
  "Find horizontal line of reflection with exactly target-diff total differences.
   Returns rows above, or 0 if none found."
  (if (null pattern)
      0
      (let* ((height (length pattern))
             (pattern-vec (coerce pattern 'vector)))
        (or (loop for row from 1 below height
                  when (= (count-horizontal-differences pattern-vec row height) target-diff)
                    return row)
            0))))

(defun summarize-pattern (pattern target-diff)
  "Get the summary value for a pattern with given target difference count."
  (let ((v (find-vertical-reflection pattern target-diff)))
    (if (plusp v)
        v
        (* (find-horizontal-reflection pattern target-diff) 100))))

(defun part1 (patterns)
  "Calculate the sum of all pattern summaries (exact reflection)."
  (loop for pattern in patterns
        sum (summarize-pattern pattern 0)))

(defun part2 (patterns)
  "Calculate the sum with smudge fixes (exactly one difference)."
  (loop for pattern in patterns
        sum (summarize-pattern pattern 1)))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (text (read-input-file input-file))
         (patterns (parse-input text)))
    (format t "Part 1: ~a~%" (part1 patterns))
    (format t "Part 2: ~a~%" (part2 patterns))))

(main)
