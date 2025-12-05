#!/usr/bin/env sbcl --script

;;; Advent of Code 2024 - Day 3: Mull It Over
;;; Common Lisp implementation (no external dependencies)

(defun read-input ()
  "Read the input file from ../input.txt"
  (with-open-file (stream "../input.txt"
                          :direction :input
                          :if-does-not-exist :error)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun digit-char-to-int (c)
  "Convert a digit character to an integer."
  (- (char-code c) (char-code #\0)))

(defun parse-number (data start max-len)
  "Parse a number from data starting at position start with max max-len digits.
   Returns (values number end-position) or (values nil nil) if no valid number."
  (let ((num 0)
        (len 0))
    (loop while (and (< start (length data))
                     (< len max-len)
                     (digit-char-p (char data start)))
          do (setf num (+ (* num 10) (digit-char-to-int (char data start))))
             (incf start)
             (incf len))
    (if (> len 0)
        (values num start)
        (values nil nil))))

(defun try-parse-mul (data pos)
  "Try to parse mul(X,Y) at position pos.
   Returns (values x y new-pos) or (values nil nil nil) if not a valid mul."
  (when (and (< (+ pos 4) (length data))
             (string= data "mul(" :start1 pos :end1 (+ pos 4)))
    (let ((pos (+ pos 4)))
      ;; Parse first number (1-3 digits)
      (multiple-value-bind (x new-pos) (parse-number data pos 3)
        (when (and x (< new-pos (length data)) (char= (char data new-pos) #\,))
          (setf pos (1+ new-pos))
          ;; Parse second number (1-3 digits)
          (multiple-value-bind (y new-pos) (parse-number data pos 3)
            (when (and y (< new-pos (length data)) (char= (char data new-pos) #\)))
              (values x y (1+ new-pos)))))))))

(defun part1 (data)
  "Find all valid mul(X,Y) instructions and sum their products."
  (let ((total 0)
        (pos 0))
    (loop while (< pos (length data))
          do (multiple-value-bind (x y new-pos) (try-parse-mul data pos)
               (if x
                   (progn
                     (incf total (* x y))
                     (setf pos new-pos))
                   (incf pos))))
    total))

(defun try-parse-do (data pos)
  "Try to parse do() at position pos. Returns new-pos or nil."
  (when (and (<= (+ pos 4) (length data))
             (string= data "do()" :start1 pos :end1 (+ pos 4)))
    (+ pos 4)))

(defun try-parse-dont (data pos)
  "Try to parse don't() at position pos. Returns new-pos or nil."
  (when (and (<= (+ pos 7) (length data))
             (string= data "don't()" :start1 pos :end1 (+ pos 7)))
    (+ pos 7)))

(defun part2 (data)
  "Like part1, but do() enables and don't() disables mul instructions."
  (let ((total 0)
        (pos 0)
        (enabled t))
    (loop while (< pos (length data))
          do (let ((do-pos (try-parse-do data pos))
                   (dont-pos (try-parse-dont data pos)))
               (cond
                 (do-pos
                  (setf enabled t)
                  (setf pos do-pos))
                 (dont-pos
                  (setf enabled nil)
                  (setf pos dont-pos))
                 (t
                  (multiple-value-bind (x y new-pos) (try-parse-mul data pos)
                    (if x
                        (progn
                          (when enabled
                            (incf total (* x y)))
                          (setf pos new-pos))
                        (incf pos)))))))
    total))

(defun main ()
  "Main entry point"
  (let ((data (read-input)))
    (format t "Part 1: ~a~%" (part1 data))
    (format t "Part 2: ~a~%" (part2 data))))

(main)
