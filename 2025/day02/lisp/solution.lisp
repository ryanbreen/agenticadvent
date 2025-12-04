; Run: sbcl --script solution.lisp

(defun read-input ()
  "Read input file and return as a single string."
  (with-open-file (stream "../input.txt")
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (string-trim '(#\Newline #\Space) content))))

(defun parse-ranges (input-text)
  "Parse comma-separated ranges into a list of (start . end) pairs."
  (let ((ranges '()))
    (loop for part in (split-string input-text #\,)
          do (let ((trimmed (string-trim '(#\Space) part)))
               (when (find #\- trimmed)
                 (let* ((dash-pos (position #\- trimmed))
                        (start (parse-integer (subseq trimmed 0 dash-pos)))
                        (end (parse-integer (subseq trimmed (1+ dash-pos)))))
                   (push (cons start end) ranges)))))
    (nreverse ranges)))

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (loop with result = '()
        with start = 0
        for pos = (position delimiter string :start start)
        do (push (subseq string start pos) result)
           (if pos
               (setf start (1+ pos))
               (return (nreverse result)))))

(defun is-invalid-id-part1 (num)
  "Check if a number is invalid (a pattern repeated exactly twice)."
  (let ((s (write-to-string num)))
    (when (evenp (length s))
      (let* ((mid (/ (length s) 2))
             (first-half (subseq s 0 mid))
             (second-half (subseq s mid)))
        (string= first-half second-half)))))

(defun is-invalid-id-part2 (num)
  "Check if a number is invalid (a pattern repeated at least twice)."
  (let ((s (write-to-string num))
        (length (length (write-to-string num))))
    ;; Try all possible pattern lengths from 1 to length/2
    (loop for pattern-length from 1 to (floor length 2)
          when (zerop (mod length pattern-length))
            do (let* ((pattern (subseq s 0 pattern-length))
                      (repetitions (/ length pattern-length))
                      (repeated (apply #'concatenate 'string
                                      (loop repeat repetitions collect pattern))))
                 (when (string= s repeated)
                   (return-from is-invalid-id-part2 t))))
    nil))

(defun part1 ()
  "Sum all invalid IDs (patterns repeated exactly twice)."
  (let* ((input-text (read-input))
         (ranges (parse-ranges input-text))
         (total 0))
    (dolist (range ranges)
      (loop for num from (car range) to (cdr range)
            when (is-invalid-id-part1 num)
              do (incf total num)))
    total))

(defun part2 ()
  "Sum all invalid IDs (patterns repeated at least twice)."
  (let* ((input-text (read-input))
         (ranges (parse-ranges input-text))
         (total 0))
    (dolist (range ranges)
      (loop for num from (car range) to (cdr range)
            when (is-invalid-id-part2 num)
              do (incf total num)))
    total))

(defun main ()
  "Main entry point."
  (format t "Part 1: ~a~%" (part1))
  (format t "Part 2: ~a~%" (part2)))

(main)
