#!/usr/bin/env sbcl --script
;;; Day 23: A Long Walk - Longest path through hiking trails

(defun read-grid (filename)
  "Read grid from file and return as vector of strings."
  (let ((grid (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (in filename :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            when (> (length line) 0)
            do (vector-push-extend line grid)))
    grid))

(defun grid-ref (grid row col)
  "Get character at grid position."
  (char (aref grid row) col))

(defun in-bounds-p (grid row col)
  "Check if position is within grid bounds."
  (and (>= row 0)
       (< row (length grid))
       (>= col 0)
       (< col (length (aref grid 0)))))

(defun find-start-end (grid)
  "Find start (top row '.') and end (bottom row '.') positions."
  (let* ((rows (length grid))
         (start-col (position #\. (aref grid 0)))
         (end-col (position #\. (aref grid (1- rows)))))
    (values (cons 0 start-col)
            (cons (1- rows) end-col))))

(defun count-neighbors (grid row col)
  "Count walkable neighbors of a cell."
  (let ((count 0))
    (dolist (delta '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
      (let ((nr (+ row (car delta)))
            (nc (+ col (cdr delta))))
        (when (and (in-bounds-p grid nr nc)
                   (not (char= (grid-ref grid nr nc) #\#)))
          (incf count))))
    count))

(defun find-junctions (grid)
  "Find all junction points: start, end, and cells with 3+ walkable neighbors."
  (let ((junctions (make-hash-table :test 'equal))
        (rows (length grid))
        (cols (length (aref grid 0))))
    ;; Add start and end
    (multiple-value-bind (start end) (find-start-end grid)
      (setf (gethash start junctions) t)
      (setf (gethash end junctions) t))
    ;; Find intersections
    (loop for r from 0 below rows do
      (loop for c from 0 below cols do
        (when (and (not (char= (grid-ref grid r c) #\#))
                   (>= (count-neighbors grid r c) 3))
          (setf (gethash (cons r c) junctions) t))))
    junctions))

(defun get-slope-direction (char)
  "Return required direction for a slope character, or nil if not a slope."
  (case char
    (#\^ '(-1 . 0))
    (#\v '(1 . 0))
    (#\< '(0 . -1))
    (#\> '(0 . 1))
    (otherwise nil)))

(defun build-graph (grid junctions respect-slopes)
  "Build a compressed graph between junctions with edge weights.
   Returns a hash-table mapping junction -> hash-table of (neighbor -> distance)."
  (let ((graph (make-hash-table :test 'equal))
        (rows (length grid))
        (cols (length (aref grid 0))))
    ;; Initialize empty neighbor tables for each junction
    (maphash (lambda (junction _)
               (declare (ignore _))
               (setf (gethash junction graph) (make-hash-table :test 'equal)))
             junctions)
    ;; BFS/DFS from each junction to find connected junctions
    (maphash (lambda (start-junction _)
               (declare (ignore _))
               (let ((stack (list (cons start-junction 0)))
                     (visited (make-hash-table :test 'equal)))
                 (setf (gethash start-junction visited) t)
                 (loop while stack do
                   (let* ((current (pop stack))
                          (pos (car current))
                          (r (car pos))
                          (c (cdr pos))
                          (dist (cdr current)))
                     ;; If we reached another junction (not the start)
                     (when (and (> dist 0)
                                (gethash pos junctions))
                       (setf (gethash pos (gethash start-junction graph)) dist))
                     ;; Otherwise explore neighbors
                     (unless (and (> dist 0) (gethash pos junctions))
                       (dolist (delta '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
                         (let* ((dr (car delta))
                                (dc (cdr delta))
                                (nr (+ r dr))
                                (nc (+ c dc))
                                (next-pos (cons nr nc)))
                           (when (and (>= nr 0) (< nr rows)
                                      (>= nc 0) (< nc cols)
                                      (not (char= (grid-ref grid nr nc) #\#))
                                      (not (gethash next-pos visited)))
                             ;; Check slope constraints for Part 1
                             (let ((can-move t))
                               (when respect-slopes
                                 (let ((slope-dir (get-slope-direction (grid-ref grid r c))))
                                   (when (and slope-dir
                                              (not (equal delta slope-dir)))
                                     (setf can-move nil))))
                               (when can-move
                                 (setf (gethash next-pos visited) t)
                                 (push (cons next-pos (1+ dist)) stack)))))))))))
             junctions)
    graph))

(defun longest-path-dfs (graph start end)
  "Find longest path from start to end using DFS with backtracking."
  (let ((visited (make-hash-table :test 'equal)))
    (labels ((dfs (node)
               (if (equal node end)
                   0
                   (progn
                     (setf (gethash node visited) t)
                     (let ((max-dist most-negative-fixnum))
                       (maphash (lambda (neighbor dist)
                                  (unless (gethash neighbor visited)
                                    (let ((result (dfs neighbor)))
                                      (when (> result most-negative-fixnum)
                                        (setf max-dist (max max-dist (+ dist result)))))))
                                (gethash node graph))
                       (remhash node visited)
                       max-dist)))))
      (dfs start))))

(defun solve (grid respect-slopes)
  "Solve for either part."
  (multiple-value-bind (start end) (find-start-end grid)
    (let* ((junctions (find-junctions grid))
           (graph (build-graph grid junctions respect-slopes)))
      (longest-path-dfs graph start end))))

(defun part1 (grid)
  "Part 1: Respect slope directions."
  (solve grid t))

(defun part2 (grid)
  "Part 2: Ignore slopes (treat as regular paths)."
  (solve grid nil))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (grid (read-grid input-file)))
    (format t "Part 1: ~a~%" (part1 grid))
    (format t "Part 2: ~a~%" (part2 grid))))

(main)
