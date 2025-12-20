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
  (let ((line (aref grid row)))
    (if (and (>= col 0) (< col (length line)))
        (char line col)
        #\#)))

(defun grid-rows (grid)
  (length grid))

(defun grid-cols (grid)
  (if (> (length grid) 0)
      (length (aref grid 0))
      0))

(defun trace-path (grid start end)
  "BFS to trace path, returning hash table of position -> distance from start."
  (let ((rows (grid-rows grid))
        (cols (grid-cols grid))
        (dist (make-hash-table :test 'equal))
        (queue nil))
    (setf (gethash start dist) 0)
    (push start queue)
    (loop while queue
          for pos = (pop queue)
          for r = (car pos)
          for c = (cdr pos)
          for d = (gethash pos dist)
          when (equal pos end) return nil
          do (loop for (dr . dc) in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                   for nr = (+ r dr)
                   for nc = (+ c dc)
                   for next-pos = (cons nr nc)
                   when (and (>= nr 0) (< nr rows)
                             (>= nc 0) (< nc cols)
                             (char/= (grid-ref grid nr nc) #\#)
                             (not (gethash next-pos dist)))
                   do (setf (gethash next-pos dist) (1+ d))
                      (setf queue (nconc queue (list next-pos)))))
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

(defun part1 (grid start end)
  (let ((dist (trace-path grid start end)))
    (count-cheats dist 2 100)))

(defun part2 (grid start end)
  (let ((dist (trace-path grid start end)))
    (count-cheats dist 20 100)))

(defun main ()
  (multiple-value-bind (grid start end)
      (read-input "../input.txt")
    (format t "Part 1: ~A~%" (part1 grid start end))
    (format t "Part 2: ~A~%" (part2 grid start end))))

(main)
