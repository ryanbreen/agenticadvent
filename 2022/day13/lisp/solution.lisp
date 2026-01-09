#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 Day 13: Distress Signal
;;; Common Lisp solution

(defun parse-packet (line)
  "Parse a packet line by converting JSON-like syntax to Lisp lists.
   [] -> (), comma separated values -> space separated"
  (let ((transformed
         (with-output-to-string (out)
           (loop for char across line do
             (cond
               ((char= char #\[) (write-char #\( out))
               ((char= char #\]) (write-char #\) out))
               ((char= char #\,) (write-char #\Space out))
               (t (write-char char out)))))))
    (read-from-string transformed)))

(defun compare-packets (left right)
  "Compare two packets recursively.
   Returns: -1 if left < right (correct order)
             1 if left > right (wrong order)
             0 if equal (continue)"
  (cond
    ;; Both integers
    ((and (numberp left) (numberp right))
     (cond
       ((< left right) -1)
       ((> left right) 1)
       (t 0)))
    ;; Both lists
    ((and (listp left) (listp right))
     (loop for l-elem in left
           for r-elem in right
           for result = (compare-packets l-elem r-elem)
           when (/= result 0)
             do (return-from compare-packets result))
     ;; All compared elements were equal, check lengths
     (cond
       ((< (length left) (length right)) -1)
       ((> (length left) (length right)) 1)
       (t 0)))
    ;; Mixed types - convert integer to list
    ((numberp left)
     (compare-packets (list left) right))
    (t
     (compare-packets left (list right)))))

(defun read-input (filename)
  "Read all non-empty lines from input file and parse packets."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (and (> (length line) 0)
                    (char= (char line 0) #\[))
            collect (parse-packet line))))

(defun part1 (packets)
  "Sum indices of pairs in correct order."
  (loop for i from 0 by 2
        for pair-index from 1
        while (< (1+ i) (length packets))
        for left = (nth i packets)
        for right = (nth (1+ i) packets)
        when (= (compare-packets left right) -1)
          sum pair-index))

(defun part2 (packets)
  "Sort all packets with divider packets, find decoder key."
  (let* ((divider1 '((2)))
         (divider2 '((6)))
         (all-packets (append packets (list divider1 divider2)))
         (sorted (sort (copy-list all-packets)
                      (lambda (a b) (= (compare-packets a b) -1)))))
    ;; Find positions (1-indexed)
    (let ((pos1 (1+ (position divider1 sorted :test #'equal)))
          (pos2 (1+ (position divider2 sorted :test #'equal))))
      (* pos1 pos2))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (packets (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 packets))
    (format t "Part 2: ~A~%" (part2 packets))))

(main)
