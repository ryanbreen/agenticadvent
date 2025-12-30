#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 16 - Light Beam Bouncing
;;; Uses BFS to track beam paths through mirrors and splitters

(defconstant +right+ 0)
(defconstant +down+ 1)
(defconstant +left+ 2)
(defconstant +up+ 3)

(defparameter *dr* #(0 1 0 -1))
(defparameter *dc* #(1 0 -1 0))

(defun read-grid (filename)
  "Read the input file and return a vector of strings."
  (with-open-file (stream filename :direction :input)
    (let ((lines nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (push line lines))
      (coerce (nreverse lines) 'vector))))

(defun make-state-key (row col dir)
  "Create a unique key for a beam state."
  (+ (* row 1000000) (* col 1000) dir))

(defun count-energized (grid start-row start-col start-dir)
  "Count energized tiles starting from given position and direction.
   Directions: 0=right, 1=down, 2=left, 3=up"
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (visited (make-hash-table :test 'eql))
         (queue (list (list start-row start-col start-dir))))

    (loop while queue do
      (let* ((current (pop queue))
             (r (first current))
             (c (second current))
             (d (third current)))

        ;; Skip if out of bounds
        (when (and (>= r 0) (< r rows) (>= c 0) (< c cols))
          (let ((state-key (make-state-key r c d)))
            ;; Skip if already visited this state
            (unless (gethash state-key visited)
              (setf (gethash state-key visited) t)

              (let* ((cell (char (aref grid r) c))
                     (next-dirs
                       (cond
                         ((char= cell #\.)
                          (list d))
                         ((char= cell #\/)
                          (list (aref #(3 2 1 0) d)))
                         ((char= cell #\\)
                          (list (aref #(1 0 3 2) d)))
                         ((char= cell #\|)
                          (if (or (= d +right+) (= d +left+))
                              (list +down+ +up+)
                              (list d)))
                         ((char= cell #\-)
                          (if (or (= d +down+) (= d +up+))
                              (list +right+ +left+)
                              (list d)))
                         (t (list d)))))

                (dolist (nd next-dirs)
                  (push (list (+ r (aref *dr* nd))
                              (+ c (aref *dc* nd))
                              nd)
                        queue))))))))

    ;; Count unique (row, col) positions
    (let ((positions (make-hash-table :test 'eql)))
      (maphash (lambda (key value)
                 (declare (ignore value))
                 (let* ((row (floor key 1000000))
                        (col (floor (mod key 1000000) 1000))
                        (pos-key (+ (* row 10000) col)))
                   (setf (gethash pos-key positions) t)))
               visited)
      (hash-table-count positions))))

(defun part1 (grid)
  "Part 1: Beam starts at (0,0) heading right."
  (count-energized grid 0 0 +right+))

(defun part2 (grid)
  "Part 2: Find maximum energized tiles from any edge starting position."
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (max-energized 0))

    ;; Top row, heading down
    (dotimes (c cols)
      (setf max-energized (max max-energized (count-energized grid 0 c +down+))))

    ;; Bottom row, heading up
    (dotimes (c cols)
      (setf max-energized (max max-energized (count-energized grid (1- rows) c +up+))))

    ;; Left column, heading right
    (dotimes (r rows)
      (setf max-energized (max max-energized (count-energized grid r 0 +right+))))

    ;; Right column, heading left
    (dotimes (r rows)
      (setf max-energized (max max-energized (count-energized grid r (1- cols) +left+))))

    max-energized))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (grid (read-grid input-file)))
    (format t "Part 1: ~A~%" (part1 grid))
    (format t "Part 2: ~A~%" (part2 grid))))

(main)
