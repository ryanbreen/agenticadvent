#!/usr/bin/env sbcl --script

(defun get-script-dir ()
  "Get the directory containing this script."
  (let ((script-path (or *load-pathname* *compile-file-pathname*)))
    (if script-path
        (directory-namestring script-path)
        "./")))

(defun read-input ()
  "Read the input file."
  (let ((input-file (merge-pathnames "../input.txt" (get-script-dir))))
    (with-open-file (stream input-file :direction :input)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(defun parse-move (line)
  "Parse a move line like 'R 4' into (direction . count)."
  (let* ((parts (split-string line #\Space))
         (dir (char (first parts) 0))
         (count (parse-integer (second parts))))
    (cons dir count)))

(defun split-string (string char)
  "Split string by character."
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun direction-delta (dir)
  "Get (dx . dy) for a direction character."
  (case dir
    (#\U (cons 0 1))
    (#\D (cons 0 -1))
    (#\L (cons -1 0))
    (#\R (cons 1 0))))

(defun sign-of (x)
  "Return -1, 0, or 1 based on sign of x."
  (cond ((zerop x) 0)
        ((plusp x) 1)
        (t -1)))

(defun move-tail (head tail)
  "Move tail toward head if not adjacent. Returns new tail position."
  (let* ((hx (car head))
         (hy (cdr head))
         (tx (car tail))
         (ty (cdr tail))
         (dx (- hx tx))
         (dy (- hy ty)))
    ;; If adjacent or overlapping, don't move
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
        tail
        ;; Move toward head
        (cons (+ tx (sign-of dx))
              (+ ty (sign-of dy))))))

(defun simulate-rope (moves rope-length)
  "Simulate rope with given length and return count of positions visited by tail."
  (let ((knots (make-array rope-length :initial-element (cons 0 0)))
        (visited (make-hash-table :test 'equal)))
    ;; Initialize all knots at origin
    (dotimes (i rope-length)
      (setf (aref knots i) (cons 0 0)))
    ;; Mark starting position
    (setf (gethash (aref knots (1- rope-length)) visited) t)
    ;; Process each move
    (dolist (line moves)
      (let* ((move (parse-move line))
             (dir (car move))
             (count (cdr move))
             (delta (direction-delta dir))
             (dx (car delta))
             (dy (cdr delta)))
        ;; Repeat for count steps
        (dotimes (step count)
          ;; Move head
          (let ((head (aref knots 0)))
            (setf (aref knots 0) (cons (+ (car head) dx)
                                       (+ (cdr head) dy))))
          ;; Move each subsequent knot
          (loop for i from 1 below rope-length do
            (setf (aref knots i)
                  (move-tail (aref knots (1- i)) (aref knots i))))
          ;; Record tail position
          (setf (gethash (aref knots (1- rope-length)) visited) t))))
    ;; Return count of unique positions
    (hash-table-count visited)))

(defun part1 (moves)
  "Part 1: Rope length 2."
  (simulate-rope moves 2))

(defun part2 (moves)
  "Part 2: Rope length 10."
  (simulate-rope moves 10))

(defun main ()
  (let ((moves (read-input)))
    (format t "Part 1: ~a~%" (part1 moves))
    (format t "Part 2: ~a~%" (part2 moves))))

(main)
