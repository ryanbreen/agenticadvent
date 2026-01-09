#!/usr/bin/env sbcl --script

;;; Day 22: Monkey Map
;;; Navigate a 2D map with wrapping (Part 1: flat, Part 2: cube)

(defun read-input (filename)
  "Read and parse the input file, returning grid and instructions."
  (with-open-file (stream filename :direction :input)
    (let ((lines nil)
          (path-line nil))
      ;; Read all lines
      (loop for line = (read-line stream nil nil)
            while line
            do (if (and (> (length line) 0)
                        (or (digit-char-p (char line 0))
                            (char= (char line 0) #\L)
                            (char= (char line 0) #\R)))
                   (setf path-line line)
                   (unless (zerop (length line))
                     (push line lines))))
      (setf lines (nreverse lines))
      ;; Pad lines to consistent width
      (let* ((width (reduce #'max lines :key #'length))
             (height (length lines))
             (grid (make-array (list height width) :initial-element #\Space)))
        ;; Fill grid
        (loop for line in lines
              for r from 0
              do (loop for c below (length line)
                       do (setf (aref grid r c) (char line c))))
        ;; Parse instructions
        (let ((instructions nil)
              (i 0))
          (loop while (< i (length path-line))
                do (if (digit-char-p (char path-line i))
                       (let ((j i))
                         (loop while (and (< j (length path-line))
                                          (digit-char-p (char path-line j)))
                               do (incf j))
                         (push (parse-integer (subseq path-line i j)) instructions)
                         (setf i j))
                       (progn
                         (push (char path-line i) instructions)
                         (incf i))))
          (values grid (nreverse instructions)))))))

(defun find-start (grid)
  "Find the starting position (leftmost open tile on top row)."
  (loop for c below (array-dimension grid 1)
        when (char= (aref grid 0 c) #\.)
        return (values 0 c)))

;; Direction deltas: 0=right, 1=down, 2=left, 3=up
(defparameter *dr* #(0 1 0 -1))
(defparameter *dc* #(1 0 -1 0))

(defun wrap-flat (grid row col facing)
  "Handle flat wrapping for Part 1."
  (let ((height (array-dimension grid 0))
        (width (array-dimension grid 1))
        (nr row)
        (nc col))
    (case facing
      (0 ; Right - find leftmost non-space
       (setf nc 0)
       (loop while (char= (aref grid nr nc) #\Space) do (incf nc)))
      (2 ; Left - find rightmost non-space
       (setf nc (1- width))
       (loop while (char= (aref grid nr nc) #\Space) do (decf nc)))
      (1 ; Down - find topmost non-space
       (setf nr 0)
       (loop while (char= (aref grid nr nc) #\Space) do (incf nr)))
      (3 ; Up - find bottommost non-space
       (setf nr (1- height))
       (loop while (char= (aref grid nr nc) #\Space) do (decf nr))))
    (values nr nc facing)))

(defun part1 (grid instructions)
  "Navigate the map with 2D flat wrapping."
  (multiple-value-bind (row col) (find-start grid)
    (let ((facing 0)
          (height (array-dimension grid 0))
          (width (array-dimension grid 1)))
      (dolist (instr instructions)
        (if (numberp instr)
            ;; Move forward
            (dotimes (step instr)
              (let* ((dr (aref *dr* facing))
                     (dc (aref *dc* facing))
                     (nr (+ row dr))
                     (nc (+ col dc))
                     (nf facing)
                     (need-wrap nil))
                ;; Check if we need to wrap
                (cond
                  ((< nc 0) (setf need-wrap t))
                  ((>= nc width) (setf need-wrap t))
                  ((< nr 0) (setf need-wrap t))
                  ((>= nr height) (setf need-wrap t))
                  ((char= (aref grid nr nc) #\Space) (setf need-wrap t)))
                (when need-wrap
                  (multiple-value-setq (nr nc nf) (wrap-flat grid row col facing)))
                ;; Check for wall
                (when (char= (aref grid nr nc) #\#)
                  (return))
                ;; Move
                (setf row nr col nc facing nf)))
            ;; Turn
            (if (char= instr #\R)
                (setf facing (mod (1+ facing) 4))
                (setf facing (mod (1- facing) 4)))))
      ;; Calculate password (1-indexed)
      (+ (* 1000 (1+ row)) (* 4 (1+ col)) facing))))

(defun get-cube-face (row col face-size)
  "Determine which face and local coordinates for cube layout:
     12
     3
    45
    6"
  (let ((face-row (floor row face-size))
        (face-col (floor col face-size))
        (local-r (mod row face-size))
        (local-c (mod col face-size)))
    (let ((face (cond
                  ((and (= face-row 0) (= face-col 1)) 1)
                  ((and (= face-row 0) (= face-col 2)) 2)
                  ((and (= face-row 1) (= face-col 1)) 3)
                  ((and (= face-row 2) (= face-col 0)) 4)
                  ((and (= face-row 2) (= face-col 1)) 5)
                  ((and (= face-row 3) (= face-col 0)) 6)
                  (t -1))))
      (values face local-r local-c))))

(defun wrap-cube (row col facing face-size)
  "Handle cube wrapping for the actual input layout."
  (let ((s face-size))
    (multiple-value-bind (face lr lc) (get-cube-face row col s)
      (case face
        (1 (case facing
             (3 ; Up: goes to face 6, from left, facing right
              (values (+ (* 3 s) lc) 0 0))
             (2 ; Left: goes to face 4, from left, facing right (inverted)
              (values (- (* 3 s) 1 lr) 0 0))
             (t (values row col facing))))
        (2 (case facing
             (0 ; Right: goes to face 5, from right, facing left (inverted)
              (values (- (* 3 s) 1 lr) (1- (* 2 s)) 2))
             (1 ; Down: goes to face 3, from right, facing left
              (values (+ s lc) (1- (* 2 s)) 2))
             (3 ; Up: goes to face 6, from bottom, facing up
              (values (1- (* 4 s)) lc 3))
             (t (values row col facing))))
        (3 (case facing
             (0 ; Right: goes to face 2, from bottom, facing up
              (values (1- s) (+ (* 2 s) lr) 3))
             (2 ; Left: goes to face 4, from top, facing down
              (values (* 2 s) lr 1))
             (t (values row col facing))))
        (4 (case facing
             (3 ; Up: goes to face 3, from left, facing right
              (values (+ s lc) s 0))
             (2 ; Left: goes to face 1, from left, facing right (inverted)
              (values (- s 1 lr) s 0))
             (t (values row col facing))))
        (5 (case facing
             (0 ; Right: goes to face 2, from right, facing left (inverted)
              (values (- s 1 lr) (1- (* 3 s)) 2))
             (1 ; Down: goes to face 6, from right, facing left
              (values (+ (* 3 s) lc) (1- s) 2))
             (t (values row col facing))))
        (6 (case facing
             (0 ; Right: goes to face 5, from bottom, facing up
              (values (1- (* 3 s)) (+ s lr) 3))
             (1 ; Down: goes to face 2, from top, facing down
              (values 0 (+ (* 2 s) lc) 1))
             (2 ; Left: goes to face 1, from top, facing down
              (values 0 (+ s lr) 1))
             (t (values row col facing))))
        (t (values row col facing))))))

(defun part2 (grid instructions)
  "Navigate the map with cube wrapping."
  (multiple-value-bind (row col) (find-start grid)
    (let ((facing 0)
          (height (array-dimension grid 0))
          (width (array-dimension grid 1))
          (face-size (if (> (array-dimension grid 0) 50) 50 4)))
      (dolist (instr instructions)
        (if (numberp instr)
            ;; Move forward
            (dotimes (step instr)
              (let* ((dr (aref *dr* facing))
                     (dc (aref *dc* facing))
                     (nr (+ row dr))
                     (nc (+ col dc))
                     (nf facing)
                     (need-wrap nil))
                ;; Check if we need to wrap
                (cond
                  ((< nc 0) (setf need-wrap t))
                  ((>= nc width) (setf need-wrap t))
                  ((< nr 0) (setf need-wrap t))
                  ((>= nr height) (setf need-wrap t))
                  ((char= (aref grid nr nc) #\Space) (setf need-wrap t)))
                (when need-wrap
                  (multiple-value-setq (nr nc nf) (wrap-cube row col facing face-size)))
                ;; Check for wall
                (when (char= (aref grid nr nc) #\#)
                  (return))
                ;; Move
                (setf row nr col nc facing nf)))
            ;; Turn
            (if (char= instr #\R)
                (setf facing (mod (1+ facing) 4))
                (setf facing (mod (1- facing) 4)))))
      ;; Calculate password (1-indexed)
      (+ (* 1000 (1+ row)) (* 4 (1+ col)) facing))))

(defun main ()
  (let* ((script-path (or *load-pathname* *default-pathname-defaults*))
         (dir (make-pathname :directory (pathname-directory script-path)))
         (input-file (merge-pathnames "../input.txt" dir)))
    (multiple-value-bind (grid instructions) (read-input input-file)
      (format t "Part 1: ~A~%" (part1 grid instructions))
      (format t "Part 2: ~A~%" (part2 grid instructions)))))

(main)
