#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read the grid from file and return grid, start, and end positions."
  (with-open-file (stream filename :direction :input)
    (let ((lines nil)
          (start nil)
          (end nil))
      (loop for line = (read-line stream nil nil)
            for row from 0
            while line
            do (push line lines)
               (loop for col from 0 below (length line)
                     for ch = (char line col)
                     when (char= ch #\S) do (setf start (cons row col))
                     when (char= ch #\E) do (setf end (cons row col))))
      (values (coerce (nreverse lines) 'vector) start end))))

(defun grid-ref (grid row col)
  "Get character at grid position."
  (char (aref grid row) col))

(defun grid-rows (grid)
  (length grid))

(defun grid-cols (grid)
  (length (aref grid 0)))

;;; Two-list queue implementation for O(1) enqueue/dequeue
(defstruct (queue (:constructor make-queue ()))
  "Functional queue using two lists for O(1) amortized operations."
  (head nil :type list)
  (tail nil :type list))

(defun queue-empty-p (q)
  "Check if queue is empty."
  (and (null (queue-head q)) (null (queue-tail q))))

(defun queue-enqueue (q item)
  "Add item to the back of the queue."
  (push item (queue-tail q)))

(defun queue-dequeue (q)
  "Remove and return item from front of queue."
  (when (null (queue-head q))
    (setf (queue-head q) (nreverse (queue-tail q)))
    (setf (queue-tail q) nil))
  (pop (queue-head q)))

(defun trace-path (grid start end)
  "BFS to trace path, returning hash table of position -> distance from start."
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid))
        (dist (make-hash-table :test 'equal))
        (q (make-queue)))
    (setf (gethash start dist) 0)
    (queue-enqueue q start)
    (loop until (queue-empty-p q)
          for pos = (queue-dequeue q)
          for r = (car pos)
          for c = (cdr pos)
          for d = (gethash pos dist)
          unless (equal pos end)
            do (dolist (delta '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
                 (let* ((nr (+ r (car delta)))
                        (nc (+ c (cdr delta)))
                        (next-pos (cons nr nc)))
                   (when (and (>= nr 0) (< nr rows)
                              (>= nc 0) (< nc cols)
                              (char/= (grid-ref grid nr nc) #\#)
                              (not (gethash next-pos dist)))
                     (setf (gethash next-pos dist) (1+ d))
                     (queue-enqueue q next-pos)))))
    dist))

(defun count-cheats (dist max-cheat-time min-savings)
  "Count cheats that save at least min-savings picoseconds."
  (let ((count 0)
        (positions nil))
    ;; Collect all track positions
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k positions))
             dist)
    ;; Check all pairs
    (dolist (pos1 positions)
      (let ((r1 (car pos1))
            (c1 (cdr pos1))
            (d1 (gethash pos1 dist)))
        (dolist (pos2 positions)
          (let* ((r2 (car pos2))
                 (c2 (cdr pos2))
                 (cheat-cost (+ (abs (- r2 r1)) (abs (- c2 c1)))))
            (when (<= cheat-cost max-cheat-time)
              (let* ((d2 (gethash pos2 dist))
                     (savings (- d2 d1 cheat-cost)))
                (when (>= savings min-savings)
                  (incf count))))))))
    count))

(defun part1 (dist)
  "Count cheats with max 2 steps that save at least 100 picoseconds."
  (count-cheats dist 2 100))

(defun part2 (dist)
  "Count cheats with max 20 steps that save at least 100 picoseconds."
  (count-cheats dist 20 100))

(defun main ()
  (multiple-value-bind (grid start end)
      (read-input "../input.txt")
    (let ((dist (trace-path grid start end)))
      (format t "Part 1: ~A~%" (part1 dist))
      (format t "Part 2: ~A~%" (part2 dist)))))

(main)
