#!/usr/bin/env sbcl --script

;;; Advent of Code 2025 Day 4: Printing Department
;;; Common Lisp solution (optimized with vectors)

(defparameter *directions*
  '((-1 -1) (-1 0) (-1 1)
    (0 -1)         (0 1)
    (1 -1)  (1 0)  (1 1))
  "Eight adjacent directions for neighbor counting.")

(defun read-input (filename)
  "Read the input file and return it as a vector of strings converted to char vectors."
  (with-open-file (stream filename)
    (let ((lines '()))
      (loop for line = (read-line stream nil)
            while line
            do (push (coerce line 'vector) lines))
      (coerce (nreverse lines) 'vector))))

(defun count-adjacent-rolls (grid row col)
  "Count the number of @ symbols adjacent to position (row, col)."
  (let ((rows (length grid))
        (cols (length (aref grid 0)))
        (count 0))
    (dolist (dir *directions*)
      (let ((nr (+ row (first dir)))
            (nc (+ col (second dir))))
        (when (and (>= nr 0) (< nr rows)
                   (>= nc 0) (< nc cols)
                   (char= (aref (aref grid nr) nc) #\@))
          (incf count))))
    count))

(defun part1 (grid)
  "Count rolls of paper that can be accessed by a forklift.
   A roll can be accessed if it has fewer than 4 adjacent rolls."
  (let ((accessible 0)
        (rows (length grid))
        (cols (length (aref grid 0))))
    (dotimes (r rows)
      (dotimes (c cols)
        (when (char= (aref (aref grid r) c) #\@)
          (when (< (count-adjacent-rolls grid r c) 4)
            (incf accessible)))))
    accessible))

(defun find-removable-rolls (grid)
  "Find all rolls that can be removed (have < 4 adjacent rolls)."
  (let ((removable '())
        (rows (length grid))
        (cols (length (aref grid 0))))
    (dotimes (r rows)
      (dotimes (c cols)
        (when (char= (aref (aref grid r) c) #\@)
          (when (< (count-adjacent-rolls grid r c) 4)
            (push (list r c) removable)))))
    removable))

(defun remove-rolls (grid positions)
  "Remove rolls at the given positions by replacing @ with ."
  (dolist (pos positions)
    (let ((r (first pos))
          (c (second pos)))
      (setf (aref (aref grid r) c) #\.))))

(defun part2 (grid)
  "Count total rolls removed by iteratively removing accessible rolls.
   Keep removing accessible rolls until no more can be removed."
  ;; Create a deep copy of the grid
  (let ((mutable-grid (map 'vector (lambda (row) (copy-seq row)) grid))
        (total-removed 0))
    (loop
      (let ((removable (find-removable-rolls mutable-grid)))
        (when (null removable)
          (return))
        (remove-rolls mutable-grid removable)
        (incf total-removed (length removable))))
    total-removed))

(defun main ()
  "Main entry point."
  (let* ((input-path (merge-pathnames "../input.txt"
                                      (make-pathname :name nil
                                                     :type nil
                                                     :defaults *load-truename*)))
         (grid (read-input input-path)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

;; Run the main function
(main)
