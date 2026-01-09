#!/usr/bin/env sbcl --script
;;; Day 20: Grove Positioning System

(defun parse-input (filename)
  "Parse numbers from input file."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defun mix (numbers times)
  "Mix the list of numbers, maintaining original order references.
   Returns vector of (original-index . value) pairs."
  (let* ((n (length numbers))
         (n-1 (1- n))
         ;; Create vector of (original-index . value) pairs
         (indexed (make-array n :initial-contents
                              (loop for num in numbers
                                    for i from 0
                                    collect (cons i num)))))
    (dotimes (_ times indexed)
      (dotimes (orig-idx n)
        ;; Find current position of element with this original index
        (let ((curr-pos (position orig-idx indexed :key #'car)))
          ;; Get the value
          (let ((val (cdr (aref indexed curr-pos))))
            ;; Remove from current position by shifting elements
            (loop for i from curr-pos below (1- n)
                  do (setf (aref indexed i) (aref indexed (1+ i))))
            ;; Calculate new position (modulo n-1 because we removed the element)
            (let ((new-pos (mod (+ curr-pos val) n-1)))
              ;; Make room by shifting elements right
              (loop for i from (1- n) above new-pos
                    do (setf (aref indexed i) (aref indexed (1- i))))
              ;; Insert at new position
              (setf (aref indexed new-pos) (cons orig-idx val)))))))))

(defun grove-coordinates (mixed)
  "Find sum of 1000th, 2000th, 3000th values after 0."
  (let* ((n (length mixed))
         ;; Find index of element with value 0
         (zero-idx (position 0 mixed :key #'cdr)))
    (+ (cdr (aref mixed (mod (+ zero-idx 1000) n)))
       (cdr (aref mixed (mod (+ zero-idx 2000) n)))
       (cdr (aref mixed (mod (+ zero-idx 3000) n))))))

(defun part1 (numbers)
  "Mix once and find grove coordinates."
  (grove-coordinates (mix numbers 1)))

(defun part2 (numbers)
  "Multiply by decryption key, mix 10 times."
  (let* ((decryption-key 811589153)
         (scaled (mapcar (lambda (n) (* n decryption-key)) numbers)))
    (grove-coordinates (mix scaled 10))))

(defun main ()
  (let* ((script-path (or *load-pathname* *compile-file-pathname*
                          (pathname ".")))
         (dir (directory-namestring script-path))
         (input-file (merge-pathnames "../input.txt" dir))
         (numbers (parse-input input-file)))
    (format t "Part 1: ~A~%" (part1 numbers))
    (format t "Part 2: ~A~%" (part2 numbers))))

(main)
