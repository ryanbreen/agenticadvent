#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 4: Scratchcards

(defun read-input-file (filename)
  "Read input file and return list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-numbers (str)
  "Parse space-separated numbers from a string into a list."
  (with-input-from-string (s str)
    (loop for num = (read s nil nil)
          while num
          collect num)))

(defun parse-card (line)
  "Parse a card line into (winning-numbers . your-numbers) as sets (lists)."
  (let* ((colon-pos (position #\: line))
         (bar-pos (position #\| line))
         (winning-part (subseq line (1+ colon-pos) bar-pos))
         (have-part (subseq line (1+ bar-pos))))
    (cons (parse-numbers winning-part)
          (parse-numbers have-part))))

(defun count-matches (card)
  "Count how many of your numbers are winning numbers."
  (let ((winning (car card))
        (have (cdr card)))
    (loop for num in have
          count (member num winning))))

(defun card-score (matches)
  "Calculate score: 2^(matches-1) if matches > 0, else 0."
  (if (> matches 0)
      (expt 2 (1- matches))
      0))

(defun part1 (cards)
  "Part 1: Sum of card scores based on matching numbers."
  (loop for card in cards
        for matches = (count-matches card)
        sum (card-score matches)))

(defun part2 (cards)
  "Part 2: Count total scratchcards after cascading copies."
  (let* ((n (length cards))
         (matches (map 'vector #'count-matches cards))
         (copies (make-array n :initial-element 1)))
    ;; For each card, add copies to subsequent cards based on matches
    (loop for i from 0 below n
          for m = (aref matches i)
          do (loop for j from (1+ i) below (min (+ i 1 m) n)
                   do (incf (aref copies j) (aref copies i))))
    ;; Return total count
    (loop for c across copies sum c)))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (lines (read-input-file input-file))
         (cards (mapcar #'parse-card lines)))
    (format t "Part 1: ~A~%" (part1 cards))
    (format t "Part 2: ~A~%" (part2 cards))))

(main)
