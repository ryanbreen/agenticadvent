#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 3: Gear Ratios

(defun read-input-file (filename)
  "Read input file and return list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun digit-char-p* (char)
  "Check if a character is a digit."
  (and char (digit-char-p char)))

(defun symbol-char-p (char)
  "Check if a character is a symbol (not a digit, not a period, not nil)."
  (and char (char/= char #\.) (not (digit-char-p char))))

(defun get-char-at (grid row col)
  "Safely get character at grid position, returns nil if out of bounds."
  (when (and (>= row 0) (< row (length grid))
             (>= col 0) (< col (length (nth row grid))))
    (char (nth row grid) col)))

(defun find-numbers-in-line (line row-idx)
  "Find all numbers in a line with their positions. Returns list of (number start-col end-col row)."
  (let ((numbers nil)
        (current-num nil)
        (start-col nil))
    (loop for col from 0 below (length line)
          for char = (char line col)
          do (if (digit-char-p char)
                 (progn
                   (when (null start-col)
                     (setf start-col col))
                   (push char current-num))
                 (when current-num
                   (let ((num-str (coerce (nreverse current-num) 'string)))
                     (push (list (parse-integer num-str) start-col (1- col) row-idx)
                           numbers)
                     (setf current-num nil
                           start-col nil)))))
    ;; Handle number at end of line
    (when current-num
      (let ((num-str (coerce (nreverse current-num) 'string)))
        (push (list (parse-integer num-str) start-col (1- (length line)) row-idx)
              numbers)))
    (nreverse numbers)))

(defun find-all-numbers (grid)
  "Find all numbers in the grid with their positions."
  (loop for line in grid
        for row-idx from 0
        append (find-numbers-in-line line row-idx)))

(defun adjacent-to-symbol-p (grid number-info)
  "Check if a number is adjacent to any symbol."
  (destructuring-bind (number start-col end-col row) number-info
    (declare (ignore number))
    ;; Check all adjacent positions (including diagonals)
    (loop for r from (1- row) to (1+ row)
          do (loop for c from (1- start-col) to (1+ end-col)
                   for char = (get-char-at grid r c)
                   when (symbol-char-p char)
                   do (return-from adjacent-to-symbol-p t)))
    nil))

(defun part1 (grid)
  "Part 1: Sum all numbers adjacent to symbols."
  (let ((numbers (find-all-numbers grid)))
    (loop for num-info in numbers
          when (adjacent-to-symbol-p grid num-info)
          sum (first num-info))))

(defun find-gears (grid)
  "Find all potential gear positions (all * symbols)."
  (loop for line in grid
        for row from 0
        append (loop for col from 0 below (length line)
                     when (char= (char line col) #\*)
                     collect (list row col))))

(defun number-adjacent-to-pos-p (number-info row col)
  "Check if a number is adjacent to a specific position."
  (destructuring-bind (number start-col end-col num-row) number-info
    (declare (ignore number))
    ;; Check if the position is adjacent to any cell of the number
    (and (>= row (1- num-row))
         (<= row (1+ num-row))
         (>= col (1- start-col))
         (<= col (1+ end-col)))))

(defun find-adjacent-numbers (numbers gear-pos)
  "Find all numbers adjacent to a gear position."
  (destructuring-bind (gear-row gear-col) gear-pos
    (loop for num-info in numbers
          when (number-adjacent-to-pos-p num-info gear-row gear-col)
          collect (first num-info))))

(defun part2 (grid)
  "Part 2: Sum all gear ratios (product of two numbers adjacent to a * symbol)."
  (let ((numbers (find-all-numbers grid))
        (gears (find-gears grid)))
    (loop for gear in gears
          for adjacent-nums = (find-adjacent-numbers numbers gear)
          when (= (length adjacent-nums) 2)
          sum (* (first adjacent-nums) (second adjacent-nums)))))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (grid (read-input-file input-file)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

(main)
