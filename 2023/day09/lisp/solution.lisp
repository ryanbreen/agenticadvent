;;;; Advent of Code 2023 - Day 9: Mirage Maintenance
;;;; Common Lisp Solution

(defun split-string (string &optional (delimiter #\Space))
  "Split a string by delimiter, returning a list of substrings."
  (let ((result '())
        (start 0)
        (len (length string)))
    (loop for i from 0 to len
          do (when (or (= i len) (char= (char string i) delimiter))
               (when (> i start)
                 (push (subseq string start i) result))
               (setf start (1+ i))))
    (nreverse result)))

(defun parse-line (line)
  "Parse a line of space-separated integers."
  (mapcar #'(lambda (s) (parse-integer s)) (split-string line)))

(defun read-input (filename)
  "Read the input file and parse each line into a list of integers."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (and line (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0))
          collect (parse-line line))))

(defun get-differences (seq)
  "Compute the differences between consecutive elements."
  (loop for (a b) on seq
        while b
        collect (- b a)))

(defun all-zeros-p (seq)
  "Check if all elements in the sequence are zero."
  (every #'zerop seq))

(defun extrapolate-next (seq)
  "Extrapolate the next value in the sequence."
  (let ((sequences (list seq)))
    ;; Build the pyramid of differences
    (loop for current = seq then diffs
          for diffs = (get-differences current)
          do (push diffs sequences)
          until (all-zeros-p diffs))
    ;; Sum up the last elements from bottom to top
    (loop for s in sequences
          sum (car (last s)))))

(defun extrapolate-prev (seq)
  "Extrapolate the previous value in the sequence."
  (let ((sequences (list seq)))
    ;; Build the pyramid of differences
    (loop for current = seq then diffs
          for diffs = (get-differences current)
          do (push diffs sequences)
          until (all-zeros-p diffs))
    ;; Work from bottom to top, subtracting first elements
    (let ((prev-first 0))
      (dolist (s sequences prev-first)
        (setf prev-first (- (first s) prev-first))))))

(defun part1 (histories)
  "Sum of all extrapolated next values."
  (reduce #'+ (mapcar #'extrapolate-next histories)))

(defun part2 (histories)
  "Sum of all extrapolated previous values."
  (reduce #'+ (mapcar #'extrapolate-prev histories)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (histories (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 histories))
    (format t "Part 2: ~A~%" (part2 histories))))

(main)
