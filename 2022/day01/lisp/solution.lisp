#!/usr/bin/env sbcl --script
;;; Advent of Code 2022 - Day 1: Calorie Counting

(defun read-file-contents (filename)
  "Read entire file contents as a string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-string (string delimiter)
  "Split string by delimiter string."
  (let ((result '())
        (start 0)
        (delim-len (length delimiter)))
    (loop
      (let ((pos (search delimiter string :start2 start)))
        (if pos
            (progn
              (push (subseq string start pos) result)
              (setf start (+ pos delim-len)))
            (progn
              (push (subseq string start) result)
              (return (nreverse result))))))))

(defun trim-whitespace (string)
  "Remove leading and trailing whitespace from string."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-input (filename)
  "Parse input into list of calorie totals per elf."
  (let* ((content (trim-whitespace (read-file-contents filename)))
         (groups (split-string content (format nil "~%~%"))))
    (mapcar (lambda (group)
              (let ((lines (split-string group (format nil "~%"))))
                (reduce #'+
                        (mapcar (lambda (line)
                                  (let ((trimmed (trim-whitespace line)))
                                    (if (string= trimmed "")
                                        0
                                        (parse-integer trimmed))))
                                lines))))
            groups)))

(defun part1 (elves)
  "Find the Elf carrying the most Calories."
  (reduce #'max elves))

(defun part2 (elves)
  "Find total calories carried by top three Elves."
  (let ((sorted-elves (sort (copy-list elves) #'>)))
    (reduce #'+ (subseq sorted-elves 0 3))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (elves (parse-input input-file)))
    (format t "Part 1: ~A~%" (part1 elves))
    (format t "Part 2: ~A~%" (part2 elves))))

(main)
