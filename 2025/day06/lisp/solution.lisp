#!/usr/bin/env sbcl --script

;;; Read input file
(defun read-input-file (filename)
  "Read the entire input file as a list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;;; Parse problems for Part 1
(defun parse-problems (lines)
  "Parse the worksheet into a list of (numbers operator) pairs for Part 1."
  (when (null lines)
    (return-from parse-problems nil))

  ;; Find the operator row (last row with only +, *, and spaces)
  (let ((op-row-idx (position-if
                      (lambda (line)
                        (and (> (length (string-trim '(#\Space) line)) 0)
                             (every (lambda (c) (member c '(#\+ #\* #\Space))) line)))
                      lines :from-end t)))

    (when (null op-row-idx)
      (return-from parse-problems nil))

    (let* ((op-row (nth op-row-idx lines))
           (number-rows (subseq lines 0 op-row-idx))
           (max-width (reduce #'max (mapcar #'length lines)))
           ;; Pad all rows to the same width
           (padded-number-rows (mapcar (lambda (line)
                                          (concatenate 'string
                                                      line
                                                      (make-string (- max-width (length line))
                                                                  :initial-element #\Space)))
                                       number-rows))
           (padded-op-row (concatenate 'string
                                      op-row
                                      (make-string (- max-width (length op-row))
                                                  :initial-element #\Space)))
           (problems nil))

      ;; Find problem boundaries
      (let ((col 0))
        (loop while (< col max-width) do
          ;; Skip separator columns (all spaces)
          (loop while (and (< col max-width)
                          (every (lambda (row) (char= (char row col) #\Space))
                                 padded-number-rows)
                          (char= (char padded-op-row col) #\Space))
                do (incf col))

          (when (>= col max-width)
            (return))

          ;; Find the end of this problem
          (let ((start-col col))
            (loop while (< col max-width) do
              (let ((is-separator (and (every (lambda (row) (char= (char row col) #\Space))
                                             padded-number-rows)
                                      (char= (char padded-op-row col) #\Space))))
                (when is-separator
                  (return))
                (incf col)))

            (let ((end-col col)
                  (numbers nil))
              ;; Extract numbers from this problem
              (dolist (row padded-number-rows)
                (let ((num-str (string-trim '(#\Space) (subseq row start-col end-col))))
                  (when (> (length num-str) 0)
                    (push (parse-integer num-str) numbers))))

              (setf numbers (nreverse numbers))

              ;; Extract operator
              (let ((op-str (string-trim '(#\Space) (subseq padded-op-row start-col end-col))))
                (when (and (> (length op-str) 0) numbers)
                  (push (list numbers (char op-str 0)) problems)))))))

      (nreverse problems))))

;;; Parse problems for Part 2
(defun parse-problems-part2 (lines)
  "Parse the worksheet for Part 2 - reading right-to-left columns."
  (when (null lines)
    (return-from parse-problems-part2 nil))

  ;; Find the operator row (last row with only +, *, and spaces)
  (let ((op-row-idx (position-if
                      (lambda (line)
                        (and (> (length (string-trim '(#\Space) line)) 0)
                             (every (lambda (c) (member c '(#\+ #\* #\Space))) line)))
                      lines :from-end t)))

    (when (null op-row-idx)
      (return-from parse-problems-part2 nil))

    (let* ((op-row (nth op-row-idx lines))
           (number-rows (subseq lines 0 op-row-idx))
           (max-width (reduce #'max (mapcar #'length lines)))
           ;; Pad all rows to the same width
           (padded-number-rows (mapcar (lambda (line)
                                          (concatenate 'string
                                                      line
                                                      (make-string (- max-width (length line))
                                                                  :initial-element #\Space)))
                                       number-rows))
           (padded-op-row (concatenate 'string
                                      op-row
                                      (make-string (- max-width (length op-row))
                                                  :initial-element #\Space)))
           (problems nil))

      ;; Find problem boundaries
      (let ((col 0))
        (loop while (< col max-width) do
          ;; Skip separator columns (all spaces)
          (loop while (and (< col max-width)
                          (every (lambda (row) (char= (char row col) #\Space))
                                 padded-number-rows)
                          (char= (char padded-op-row col) #\Space))
                do (incf col))

          (when (>= col max-width)
            (return))

          ;; Find the end of this problem
          (let ((start-col col))
            (loop while (< col max-width) do
              (let ((is-separator (and (every (lambda (row) (char= (char row col) #\Space))
                                             padded-number-rows)
                                      (char= (char padded-op-row col) #\Space))))
                (when is-separator
                  (return))
                (incf col)))

            (let ((end-col col)
                  (numbers nil))
              ;; For Part 2: Read columns right-to-left, each column forms a number
              (loop for c from (1- end-col) downto start-col do
                (let ((digits nil))
                  (dolist (row padded-number-rows)
                    (let ((ch (char row c)))
                      (when (digit-char-p ch)
                        (push ch digits))))
                  (setf digits (nreverse digits))
                  (when digits
                    ;; Join digits to form number (top=most significant, bottom=least)
                    (let ((num-str (coerce digits 'string)))
                      (push (parse-integer num-str) numbers)))))

              ;; Extract operator
              (let ((op-str (string-trim '(#\Space) (subseq padded-op-row start-col end-col))))
                (when (and (> (length op-str) 0) numbers)
                  (push (list numbers (char op-str 0)) problems)))))))

      (nreverse problems))))

;;; Solve a single problem
(defun solve-problem (numbers op)
  "Solve a single problem given numbers and operator."
  (cond
    ((char= op #\+)
     (reduce #'+ numbers))
    ((char= op #\*)
     (reduce #'* numbers))
    (t 0)))

;;; Part 1
(defun part1 (lines)
  "Solve Part 1 of the problem."
  (let ((problems (parse-problems lines)))
    (reduce #'+ (mapcar (lambda (problem)
                          (solve-problem (first problem) (second problem)))
                        problems)
            :initial-value 0)))

;;; Part 2
(defun part2 (lines)
  "Solve Part 2 of the problem."
  (let ((problems (parse-problems-part2 lines)))
    (reduce #'+ (mapcar (lambda (problem)
                          (solve-problem (first problem) (second problem)))
                        problems)
            :initial-value 0)))

;;; Main
(defun main ()
  (let* ((input-path (merge-pathnames "../input.txt"
                                      (make-pathname :directory
                                                    (pathname-directory *load-truename*))))
         (lines (read-input-file input-path)))
    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

(main)
