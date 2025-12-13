#!/usr/bin/env sbcl --script

;;; Advent of Code 2024 - Day 10: Hoof It
;;; Common Lisp Solution

(defun read-input-file (filename)
  "Read the input file and return the grid as a 2D array."
  (with-open-file (stream filename)
    (let ((lines '()))
      (loop for line = (read-line stream nil)
            while line
            do (push line lines))
      (setf lines (nreverse lines))
      (let* ((rows (length lines))
             (cols (length (first lines)))
             (grid (make-array (list rows cols) :element-type 'fixnum)))
        (loop for r from 0 below rows
              for line in lines
              do (loop for c from 0 below cols
                       do (setf (aref grid r c)
                                (digit-char-p (char line c)))))
        grid))))

(defun find-trailheads (grid)
  "Find all positions with height 0."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (trailheads '()))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (= (aref grid r c) 0)
                   do (push (cons r c) trailheads)))
    (nreverse trailheads)))

(defun count-reachable-nines (grid start-r start-c)
  "BFS to find all 9s reachable from a trailhead."
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (visited (make-hash-table :test 'equal))
         (queue (list (cons start-r start-c)))
         (nines (make-hash-table :test 'equal))
         (directions '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))

    (setf (gethash (cons start-r start-c) visited) t)

    (loop while queue
          do (destructuring-bind (r . c) (pop queue)
               (let ((current-height (aref grid r c)))
                 (if (= current-height 9)
                     (setf (gethash (cons r c) nines) t)
                     ;; Try all four directions - collect neighbors then append efficiently
                     (let ((neighbors '()))
                       (dolist (dir directions)
                         (destructuring-bind (dr . dc) dir
                           (let ((nr (+ r dr))
                                 (nc (+ c dc)))
                             (when (and (>= nr 0) (< nr rows)
                                        (>= nc 0) (< nc cols))
                               (let ((next-pos (cons nr nc)))
                                 (unless (gethash next-pos visited)
                                   (when (= (aref grid nr nc) (1+ current-height))
                                     (setf (gethash next-pos visited) t)
                                     (push next-pos neighbors))))))))
                       ;; Append all neighbors at once - more efficient than repeated appends
                       (setf queue (nconc queue (nreverse neighbors))))))))

    (hash-table-count nines)))

(defun count-distinct-trails (grid start-r start-c)
  "DFS to count all distinct trails from a trailhead to any 9."
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (directions '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))

    (labels ((dfs (r c)
               (let ((current-height (aref grid r c)))
                 (if (= current-height 9)
                     1
                     (loop for dir in directions
                           sum (destructuring-bind (dr . dc) dir
                                 (let ((nr (+ r dr))
                                       (nc (+ c dc)))
                                   (if (and (>= nr 0) (< nr rows)
                                            (>= nc 0) (< nc cols)
                                            (= (aref grid nr nc) (1+ current-height)))
                                       (dfs nr nc)
                                       0))))))))

      (dfs start-r start-c))))

(defun part1 (grid)
  "Calculate the sum of scores of all trailheads."
  (loop for th in (find-trailheads grid)
        sum (destructuring-bind (r . c) th
              (count-reachable-nines grid r c))))

(defun part2 (grid)
  "Calculate the sum of ratings of all trailheads."
  (loop for th in (find-trailheads grid)
        sum (destructuring-bind (r . c) th
              (count-distinct-trails grid r c))))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (grid (read-input-file input-file)))
    (format t "Part 1: ~a~%" (part1 grid))
    (format t "Part 2: ~a~%" (part2 grid))))

;; Run main
(main)
