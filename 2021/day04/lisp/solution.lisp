;;;; Advent of Code 2021 - Day 4: Giant Squid (Bingo)

(defun read-input ()
  "Read and parse the input file."
  (let ((path (merge-pathnames "../input.txt" *load-pathname*)))
    (with-open-file (stream path :direction :input)
      (let* ((first-line (read-line stream))
             (numbers (parse-numbers first-line #\,))
             (boards nil))
        ;; Skip blank line after numbers
        (read-line stream nil nil)
        ;; Read boards
        (loop
          (let ((board (read-board stream)))
            (if board
                (push board boards)
                (return))))
        (values numbers (nreverse boards))))))

(defun parse-numbers (line delimiter)
  "Parse a line of delimiter-separated numbers."
  (let ((numbers nil)
        (start 0))
    (loop for i from 0 to (length line)
          do (when (or (= i (length line))
                       (char= (char line i) delimiter))
               (let ((num-str (string-trim " " (subseq line start i))))
                 (when (> (length num-str) 0)
                   (push (parse-integer num-str) numbers)))
               (setf start (1+ i))))
    (nreverse numbers)))

(defun read-board (stream)
  "Read a 5x5 board from the stream."
  (let ((board (make-array '(5 5) :initial-element 0))
        (row-count 0))
    (loop for line = (read-line stream nil nil)
          while (and line (< row-count 5))
          do (let ((trimmed (string-trim " " line)))
               (when (> (length trimmed) 0)
                 (let ((nums (parse-space-separated trimmed)))
                   (loop for col from 0 below 5
                         for num in nums
                         do (setf (aref board row-count col) num))
                   (incf row-count)))))
    (if (= row-count 5)
        board
        nil)))

(defun parse-space-separated (line)
  "Parse a line of space-separated numbers."
  (let ((numbers nil)
        (start 0)
        (in-number nil))
    (loop for i from 0 to (length line)
          do (if (or (= i (length line))
                     (char= (char line i) #\Space))
                 (when in-number
                   (push (parse-integer (subseq line start i)) numbers)
                   (setf in-number nil))
                 (unless in-number
                   (setf start i)
                   (setf in-number t))))
    (nreverse numbers)))

(defun check-winner (marked)
  "Check if a board has won (complete row or column)."
  (or
   ;; Check rows
   (loop for row from 0 below 5
         thereis (loop for col from 0 below 5
                       always (aref marked row col)))
   ;; Check columns
   (loop for col from 0 below 5
         thereis (loop for row from 0 below 5
                       always (aref marked row col)))))

(defun calculate-score (board marked last-number)
  "Calculate the score of a winning board."
  (let ((unmarked-sum 0))
    (loop for row from 0 below 5
          do (loop for col from 0 below 5
                   do (unless (aref marked row col)
                        (incf unmarked-sum (aref board row col)))))
    (* unmarked-sum last-number)))

(defun mark-number (board marked number)
  "Mark a number on the board if it exists."
  (loop for row from 0 below 5
        do (loop for col from 0 below 5
                 do (when (= (aref board row col) number)
                      (setf (aref marked row col) t)))))

(defun part1 (numbers boards)
  "Find the first winning board and return its score."
  (let ((marked-boards (loop for b in boards
                             collect (make-array '(5 5) :initial-element nil))))
    (loop for number in numbers
          do (loop for board in boards
                   for marked in marked-boards
                   do (mark-number board marked number)
                      (when (check-winner marked)
                        (return-from part1 (calculate-score board marked number)))))
    nil))

(defun part2 (numbers boards)
  "Find the last winning board and return its score."
  (let ((marked-boards (loop for b in boards
                             collect (make-array '(5 5) :initial-element nil)))
        (won (make-array (length boards) :initial-element nil))
        (last-score nil))
    (loop for number in numbers
          do (loop for i from 0
                   for board in boards
                   for marked in marked-boards
                   do (unless (aref won i)
                        (mark-number board marked number)
                        (when (check-winner marked)
                          (setf (aref won i) t)
                          (setf last-score (calculate-score board marked number))))))
    last-score))

(defun main ()
  (multiple-value-bind (numbers boards) (read-input)
    (format t "Part 1: ~A~%" (part1 numbers boards))
    (format t "Part 2: ~A~%" (part2 numbers boards))))

(main)
