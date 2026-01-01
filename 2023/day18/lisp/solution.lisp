#!/usr/bin/env sbcl --script
;;;; Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.

(defun read-input (filename)
  "Read and parse dig plan instructions from file.
   Returns list of (direction distance color) tuples."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect (let* ((parts (split-string line #\Space))
                         (direction (first parts))
                         (distance (parse-integer (second parts)))
                         (color-part (third parts))
                         ;; Remove (# from start and ) from end
                         (color (subseq color-part 2 (1- (length color-part)))))
                    (list direction distance color)))))

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun direction-delta (dir)
  "Return (dr dc) for a direction character/string."
  (cond
    ((string= dir "R") '(0 1))
    ((string= dir "D") '(1 0))
    ((string= dir "L") '(0 -1))
    ((string= dir "U") '(-1 0))
    (t (error "Unknown direction: ~A" dir))))

(defun hex-direction-delta (hex-digit)
  "Return (dr dc) for hex direction digit (0=R, 1=D, 2=L, 3=U)."
  (case (char-code hex-digit)
    ((#.(char-code #\0)) '(0 1))   ; R
    ((#.(char-code #\1)) '(1 0))   ; D
    ((#.(char-code #\2)) '(0 -1))  ; L
    ((#.(char-code #\3)) '(-1 0))  ; U
    (t (error "Unknown hex direction: ~A" hex-digit))))

(defun calculate-area (vertices perimeter)
  "Calculate total area using Shoelace formula and Pick's theorem.
   Shoelace gives us twice the signed area of the polygon.
   Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
   We want: Total = i + b = A + b/2 + 1"
  (let* ((n (length vertices))
         (area 0))
    ;; Shoelace formula for polygon area
    (loop for i from 0 below n
          for j = (mod (1+ i) n)
          for vi = (nth i vertices)
          for vj = (nth j vertices)
          do (incf area (* (first vi) (second vj)))
             (decf area (* (first vj) (second vi))))
    (setf area (floor (abs area) 2))
    ;; Total points = interior + boundary
    ;; From Pick's theorem: interior = area - boundary/2 + 1
    ;; Total = interior + boundary = area + boundary/2 + 1
    (+ area (floor perimeter 2) 1)))

(defun part1 (instructions)
  "Part 1: Follow the dig plan directions."
  (let ((vertices (list (list 0 0)))
        (perimeter 0)
        (r 0)
        (c 0))
    (dolist (instr instructions)
      (let* ((direction (first instr))
             (distance (second instr))
             (delta (direction-delta direction))
             (dr (first delta))
             (dc (second delta)))
        (incf r (* dr distance))
        (incf c (* dc distance))
        (push (list r c) vertices)
        (incf perimeter distance)))
    (calculate-area (nreverse vertices) perimeter)))

(defun parse-hex (hex-str)
  "Parse a hexadecimal string to integer."
  (parse-integer hex-str :radix 16))

(defun part2 (instructions)
  "Part 2: Decode instructions from hex color codes.
   Last digit of hex: 0=R, 1=D, 2=L, 3=U
   First 5 digits: distance in hex"
  (let ((vertices (list (list 0 0)))
        (perimeter 0)
        (r 0)
        (c 0))
    (dolist (instr instructions)
      (let* ((color (third instr))
             (distance (parse-hex (subseq color 0 5)))
             (dir-char (char color 5))
             (delta (hex-direction-delta dir-char))
             (dr (first delta))
             (dc (second delta)))
        (incf r (* dr distance))
        (incf c (* dc distance))
        (push (list r c) vertices)
        (incf perimeter distance)))
    (calculate-area (nreverse vertices) perimeter)))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (instructions (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 instructions))
    (format t "Part 2: ~A~%" (part2 instructions))))

(main)
