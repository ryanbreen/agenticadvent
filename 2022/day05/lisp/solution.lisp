#!/usr/bin/env sbcl --script
;;; Day 5: Supply Stacks - Crane moving crates between stacks

(defun parse-stacks (stack-lines)
  "Parse the visual stack diagram into a vector of lists (bottom to top)."
  ;; Get number of stacks from the last line (the numbers line)
  (let* ((num-line (car (last stack-lines)))
         (num-stacks (length (remove "" (split-string num-line #\Space) :test #'string=)))
         (stacks (make-array num-stacks :initial-element nil)))
    ;; Process stack lines top-down (excluding the number line)
    (dolist (line (butlast stack-lines))
      (dotimes (i num-stacks)
        (let ((pos (1+ (* i 4))))  ; Position of crate letter: 1, 5, 9, ...
          (when (< pos (length line))
            (let ((char (char line pos)))
              (when (and char (alpha-char-p char))
                (push char (aref stacks i))))))))
    ;; After processing top-to-bottom with push, bottom is at front, top is at back
    ;; This is the correct order (first element = bottom, last element = top)
    stacks))

(defun split-string (str delim)
  "Split a string by delimiter character."
  (let ((result nil)
        (current ""))
    (loop for char across str
          do (if (char= char delim)
                 (progn
                   (push current result)
                   (setf current ""))
                 (setf current (concatenate 'string current (string char)))))
    (push current result)
    (nreverse result)))

(defun parse-move (line)
  "Parse a move line like 'move 2 from 4 to 6' into (count from to)."
  (let* ((parts (split-string line #\Space))
         (count (parse-integer (nth 1 parts)))
         (from (1- (parse-integer (nth 3 parts))))   ; 0-indexed
         (to (1- (parse-integer (nth 5 parts)))))    ; 0-indexed
    (list count from to)))

(defun parse-input (filename)
  "Parse input file into (stacks . moves)."
  (with-open-file (stream filename :direction :input)
    (let ((stack-lines nil)
          (moves nil)
          (in-moves nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (cond
                 ;; Empty line separates stacks from moves
                 ((= (length line) 0)
                  (setf in-moves t))
                 ;; Moves section
                 (in-moves
                  (push (parse-move line) moves))
                 ;; Stack diagram section
                 (t
                  (push line stack-lines))))
      (cons (parse-stacks (nreverse stack-lines))
            (nreverse moves)))))

(defun copy-stacks (stacks)
  "Deep copy the stacks array."
  (let ((new-stacks (make-array (length stacks))))
    (dotimes (i (length stacks))
      (setf (aref new-stacks i) (copy-list (aref stacks i))))
    new-stacks))

(defun get-tops (stacks)
  "Get the top crate letter from each stack, concatenated."
  (coerce (loop for i from 0 below (length stacks)
                for stack = (aref stacks i)
                when stack
                collect (car (last stack)))
          'string))

(defun part1 (stacks moves)
  "Move crates one at a time (reverses order)."
  (let ((stacks (copy-stacks stacks)))
    (dolist (move moves)
      (destructuring-bind (count from to) move
        (dotimes (_ count)
          (let ((crate (car (last (aref stacks from)))))
            (setf (aref stacks from) (butlast (aref stacks from)))
            (setf (aref stacks to) (append (aref stacks to) (list crate)))))))
    (get-tops stacks)))

(defun part2 (stacks moves)
  "Move multiple crates at once (preserves order)."
  (let ((stacks (copy-stacks stacks)))
    (dolist (move moves)
      (destructuring-bind (count from to) move
        (let* ((from-stack (aref stacks from))
               (split-point (- (length from-stack) count))
               (crates (nthcdr split-point from-stack)))
          (setf (aref stacks from) (butlast from-stack count))
          (setf (aref stacks to) (append (aref stacks to) crates)))))
    (get-tops stacks)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (input (parse-input input-file))
         (stacks (car input))
         (moves (cdr input)))
    (format t "Part 1: ~A~%" (part1 stacks moves))
    (format t "Part 2: ~A~%" (part2 stacks moves))))

(main)
