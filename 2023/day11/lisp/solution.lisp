#!/usr/bin/env sbcl --script
;;; Advent of Code 2023 Day 11: Cosmic Expansion
;;; Common Lisp Solution

(defun read-input (filename)
  "Read the input file and return a list of lines."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect line)))

(defun parse-galaxies (lines)
  "Parse the grid and return a list of galaxy positions as (row . col) pairs."
  (let ((galaxies nil))
    (loop for row from 0
          for line in lines
          do (loop for col from 0 below (length line)
                   when (char= (char line col) #\#)
                   do (push (cons row col) galaxies)))
    (nreverse galaxies)))

(defun find-empty-rows (lines)
  "Find row indices that contain no galaxies."
  (let ((empty-rows nil))
    (loop for row from 0
          for line in lines
          when (not (find #\# line))
          do (push row empty-rows))
    (nreverse empty-rows)))

(defun find-empty-cols (lines)
  "Find column indices that contain no galaxies."
  (when lines
    (let* ((num-cols (length (first lines)))
           (num-rows (length lines))
           (empty-cols nil))
      (loop for col from 0 below num-cols
            when (loop for row from 0 below num-rows
                       never (char= (char (nth row lines) col) #\#))
            do (push col empty-cols))
      (nreverse empty-cols))))

(defun count-in-range (sorted-list min-val max-val)
  "Count how many values in sorted-list fall within [min-val, max-val)."
  (loop for val in sorted-list
        count (and (>= val min-val) (< val max-val))))

(defun calculate-distance (g1 g2 empty-rows empty-cols expansion-factor)
  "Calculate the expanded Manhattan distance between two galaxies."
  (let* ((r1 (car g1))
         (c1 (cdr g1))
         (r2 (car g2))
         (c2 (cdr g2))
         (min-r (min r1 r2))
         (max-r (max r1 r2))
         (min-c (min c1 c2))
         (max-c (max c1 c2))
         (base-dist (+ (- max-r min-r) (- max-c min-c)))
         (empty-row-count (count-in-range empty-rows min-r max-r))
         (empty-col-count (count-in-range empty-cols min-c max-c)))
    (+ base-dist
       (* empty-row-count (- expansion-factor 1))
       (* empty-col-count (- expansion-factor 1)))))

(defun calculate-total-distances (galaxies empty-rows empty-cols expansion-factor)
  "Calculate sum of distances between all pairs of galaxies."
  (let ((total 0)
        (n (length galaxies)))
    (loop for i from 0 below (1- n)
          do (loop for j from (1+ i) below n
                   do (incf total
                            (calculate-distance (nth i galaxies)
                                                (nth j galaxies)
                                                empty-rows
                                                empty-cols
                                                expansion-factor))))
    total))

(defun part1 (lines)
  "Solve Part 1 - expansion factor of 2."
  (let ((galaxies (parse-galaxies lines))
        (empty-rows (find-empty-rows lines))
        (empty-cols (find-empty-cols lines)))
    (calculate-total-distances galaxies empty-rows empty-cols 2)))

(defun part2 (lines)
  "Solve Part 2 - expansion factor of 1,000,000."
  (let ((galaxies (parse-galaxies lines))
        (empty-rows (find-empty-rows lines))
        (empty-cols (find-empty-cols lines)))
    (calculate-total-distances galaxies empty-rows empty-cols 1000000)))

(defun main ()
  "Main entry point."
  (let* ((input-file (if (> (length sb-ext:*posix-argv*) 1)
                         (second sb-ext:*posix-argv*)
                         "../input.txt"))
         (lines (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

(main)
