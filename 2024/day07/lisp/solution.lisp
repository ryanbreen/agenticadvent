#!/usr/bin/env sbcl --script

;;; Advent of Code 2024 Day 7: Bridge Repair
;;; Common Lisp implementation

(defun split-string (string separator)
  "Split string by separator character or string"
  (let ((sep-char (if (characterp separator) separator (char separator 0)))
        (result nil)
        (current ""))
    (loop for char across string
          do (if (char= char sep-char)
                 (progn
                   (when (> (length current) 0)
                     (push current result))
                   (setf current ""))
                 (setf current (concatenate 'string current (string char)))))
    (when (> (length current) 0)
      (push current result))
    (nreverse result)))

(defun parse-line (line)
  "Parse a line like '190: 10 19' into (target . nums)"
  (let* ((colon-pos (position #\: line))
         (target (parse-integer (subseq line 0 colon-pos)))
         (nums-str (string-trim " " (subseq line (1+ colon-pos))))
         (nums (mapcar #'parse-integer (split-string nums-str #\Space))))
    (cons target nums)))

(defun parse-input (text)
  "Parse input text into list of (target . nums) pairs"
  (mapcar #'parse-line
          (remove-if (lambda (s) (string= s ""))
                     (split-string text #\Newline))))

(defun concat-numbers (a b)
  "Concatenate two numbers: 12 || 345 = 12345"
  (parse-integer (format nil "~A~A" a b)))

(defun evaluate (nums ops)
  "Evaluate numbers with operators left-to-right"
  (let ((result (first nums)))
    (loop for i from 0 below (length ops)
          for op = (nth i ops)
          for num = (nth (1+ i) nums)
          do (cond
               ((eq op '+) (setf result (+ result num)))
               ((eq op '*) (setf result (* result num)))
               ((eq op '||) (setf result (concat-numbers result num)))))
    result))

(defun generate-operator-combinations (operators n)
  "Generate all combinations of operators of length n"
  (if (zerop n)
      '(nil)
      (let ((sub-combinations (generate-operator-combinations operators (1- n))))
        (loop for op in operators
              append (loop for combo in sub-combinations
                          collect (cons op combo))))))

(defun can-make-target (target nums operators)
  "Check if any combination of operators can produce target"
  (let ((n-ops (1- (length nums))))
    (loop for ops in (generate-operator-combinations operators n-ops)
          when (= (evaluate nums ops) target)
          return t)))

(defun part1 (equations)
  "Solve Part 1: sum targets that can be made with + and *"
  (let ((operators '(+ *)))
    (loop for (target . nums) in equations
          when (can-make-target target nums operators)
          sum target)))

(defun part2 (equations)
  "Solve Part 2: sum targets that can be made with +, *, and ||"
  (let ((operators '(+ * ||)))
    (loop for (target . nums) in equations
          when (can-make-target target nums operators)
          sum target)))

(defun read-file (filepath)
  "Read entire file into a string"
  (with-open-file (stream filepath)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun main ()
  "Main entry point"
  (let* ((text (read-file "../input.txt"))
         (equations (parse-input text)))
    (format t "Part 1: ~A~%" (part1 equations))
    (format t "Part 2: ~A~%" (part2 equations))))

(main)
