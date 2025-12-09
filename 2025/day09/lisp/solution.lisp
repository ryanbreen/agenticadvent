#!/usr/bin/env sbcl --script

;;; Day 9: Movie Theater - Common Lisp Solution

(defun read-input (filename)
  "Read input file and parse points as (x y) pairs."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (let* ((comma-pos (position #\, line))
                        (x (parse-integer line :end comma-pos))
                        (y (parse-integer line :start (1+ comma-pos))))
                   (cons x y)))))

(defun part1 (points)
  "Find the largest rectangle area using two red tiles as opposite corners."
  (let ((max-area 0)
        (n (length points)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n do
        (let* ((p1 (nth i points))
               (p2 (nth j points))
               (x1 (car p1))
               (y1 (cdr p1))
               (x2 (car p2))
               (y2 (cdr p2))
               (width (1+ (abs (- x2 x1))))
               (height (1+ (abs (- y2 y1))))
               (area (* width height)))
          (setf max-area (max max-area area)))))
    max-area))

(defun part2 (points)
  "Find the largest rectangle using only red and green tiles."
  (let ((n (length points))
        (horizontal-edges '())
        (vertical-edges '())
        (vert-by-x (make-hash-table))
        (horiz-by-y (make-hash-table)))

    ;; Build edges connecting consecutive points
    (loop for i from 0 below n do
      (let* ((p1 (nth i points))
             (p2 (nth (mod (1+ i) n) points))
             (x1 (car p1))
             (y1 (cdr p1))
             (x2 (car p2))
             (y2 (cdr p2)))
        (if (= y1 y2)
            ;; Horizontal edge
            (push (list y1 (min x1 x2) (max x1 x2)) horizontal-edges)
            ;; Vertical edge
            (push (list x1 (min y1 y2) (max y1 y2)) vertical-edges))))

    ;; Sort vertical edges by x coordinate
    (setf vertical-edges (sort vertical-edges #'< :key #'car))

    ;; Build maps for efficient lookup
    (dolist (edge vertical-edges)
      (let ((x (first edge))
            (y-min (second edge))
            (y-max (third edge)))
        (push (cons y-min y-max) (gethash x vert-by-x '()))))

    (dolist (edge horizontal-edges)
      (let ((y (first edge))
            (x-min (second edge))
            (x-max (third edge)))
        (push (cons x-min x-max) (gethash y horiz-by-y '()))))

    ;; Helper function: check if point is inside polygon using ray casting
    (labels ((is-inside-polygon (x y)
               (let ((crossings 0))
                 (maphash (lambda (vx edges)
                           (when (> vx x)
                             (dolist (edge edges)
                               (let ((y-min (car edge))
                                     (y-max (cdr edge)))
                                 (cond
                                   ((and (< y-min y) (< y y-max))
                                    (incf crossings))
                                   ((or (= y y-min) (= y y-max))
                                    (incf crossings 0.5)))))))
                         vert-by-x)
                 (= (mod crossings 2) 1)))

             (rectangle-valid (x1 y1 x2 y2)
               (let ((min-x (min x1 x2))
                     (max-x (max x1 x2))
                     (min-y (min y1 y2))
                     (max-y (max y1 y2)))

                 ;; Check if any vertical edge crosses through rectangle interior
                 (block vert-check
                   (maphash (lambda (vx edges)
                             (when (and (< min-x vx) (< vx max-x))
                               (dolist (edge edges)
                                 (let ((y-min (car edge))
                                       (y-max (cdr edge)))
                                   (unless (or (<= y-max min-y) (>= y-min max-y))
                                     (return-from rectangle-valid nil))))))
                           vert-by-x))

                 ;; Check if any horizontal edge crosses through rectangle interior
                 (block horiz-check
                   (maphash (lambda (hy edges)
                             (when (and (< min-y hy) (< hy max-y))
                               (dolist (edge edges)
                                 (let ((x-min (car edge))
                                       (x-max (cdr edge)))
                                   (unless (or (<= x-max min-x) (>= x-min max-x))
                                     (return-from rectangle-valid nil))))))
                           horiz-by-y))

                 ;; Check that center point is inside polygon
                 (let ((center-x (/ (+ min-x max-x) 2.0))
                       (center-y (/ (+ min-y max-y) 2.0)))
                   (is-inside-polygon center-x center-y)))))

      ;; Find largest valid rectangle with red corners
      (let ((max-area 0))
        (loop for i from 0 below n do
          (loop for j from (1+ i) below n do
            (let* ((p1 (nth i points))
                   (p2 (nth j points))
                   (x1 (car p1))
                   (y1 (cdr p1))
                   (x2 (car p2))
                   (y2 (cdr p2)))
              (when (rectangle-valid x1 y1 x2 y2)
                (let* ((width (1+ (abs (- x2 x1))))
                       (height (1+ (abs (- y2 y1))))
                       (area (* width height)))
                  (setf max-area (max max-area area)))))))
        max-area))))

(defun main ()
  "Main entry point."
  (let* ((input-path (merge-pathnames "../input.txt"
                                      (make-pathname :defaults *load-truename*)))
         (points (read-input input-path)))
    (format t "Part 1: ~a~%" (part1 points))
    (format t "Part 2: ~a~%" (part2 points))))

;; Run main function
(main)
