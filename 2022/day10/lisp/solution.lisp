#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 Day 10: Cathode-Ray Tube

(defun read-input (filename)
  "Read input file and return list of instructions."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun simulate-cpu (instructions)
  "Simulate CPU and return list of (cycle . x) pairs for each cycle."
  (let ((x 1)
        (cycle 0)
        (result nil))
    (dolist (line instructions)
      (cond
        ((string= line "noop")
         (incf cycle)
         (push (cons cycle x) result))
        (t  ; addx V
         (let ((v (parse-integer (subseq line 5))))
           (incf cycle)
           (push (cons cycle x) result)
           (incf cycle)
           (push (cons cycle x) result)
           (incf x v)))))
    (nreverse result)))

(defun part1 (instructions)
  "Sum signal strengths at cycles 20, 60, 100, 140, 180, 220."
  (let ((target-cycles '(20 60 100 140 180 220))
        (total 0))
    (dolist (state (simulate-cpu instructions))
      (let ((cycle (car state))
            (x (cdr state)))
        (when (member cycle target-cycles)
          (incf total (* cycle x)))))
    total))

(defun part2 (instructions)
  "Render CRT display. Sprite is 3 pixels wide centered at X."
  (let ((screen (make-array '(6 40) :element-type 'character :initial-element #\.)))
    (dolist (state (simulate-cpu instructions))
      (let* ((cycle (car state))
             (x (cdr state))
             (pos (mod (1- cycle) 40))
             (row (floor (1- cycle) 40)))
        (when (< row 6)
          (when (<= (abs (- pos x)) 1)
            (setf (aref screen row pos) #\#)))))
    ;; Convert to string
    (with-output-to-string (out)
      (dotimes (row 6)
        (when (> row 0)
          (write-char #\Newline out))
        (dotimes (col 40)
          (write-char (aref screen row col) out))))))

(defun main ()
  (let* ((script-path (or *load-pathname* *default-pathname-defaults*))
         (script-dir (make-pathname :directory (pathname-directory script-path)))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (instructions (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 instructions))
    (format t "Part 2:~%~A~%" (part2 instructions))))

(main)
