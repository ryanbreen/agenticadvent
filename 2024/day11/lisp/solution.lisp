#!/usr/bin/env sbcl --script

;;; Day 11: Plutonian Pebbles
;;; Memoized recursive counting of stones after blinks

(defun split-string (string)
  "Split a string by whitespace into a list of strings."
  (let ((words '())
        (current-word ""))
    (loop for char across string
          do (if (member char '(#\Space #\Tab #\Newline))
                 (when (> (length current-word) 0)
                   (push current-word words)
                   (setf current-word ""))
                 (setf current-word (concatenate 'string current-word (string char)))))
    (when (> (length current-word) 0)
      (push current-word words))
    (nreverse words)))

(defun read-input (filename)
  "Read space-separated numbers from input file."
  (with-open-file (stream filename)
    (let ((line (read-line stream nil)))
      (when line
        (mapcar #'parse-integer (split-string (string-trim '(#\Space #\Tab #\Newline) line)))))))

(defun count-digits (n)
  "Count the number of digits in a positive integer."
  (if (zerop n)
      1
      (1+ (floor (log n 10)))))

(defun split-number (n)
  "Split a number with even digits into left and right halves.
   Returns (values left right)."
  (let* ((s (write-to-string n))
         (len (length s))
         (mid (/ len 2))
         (left-str (subseq s 0 mid))
         (right-str (subseq s mid)))
    (values (parse-integer left-str)
            (parse-integer right-str))))

(let ((memo (make-hash-table :test 'equal)))
  (defun count-stones (value blinks)
    "Count how many stones result from a single stone after N blinks.
     Uses memoization for efficiency."
    (let ((key (cons value blinks)))
      (multiple-value-bind (cached found) (gethash key memo)
        (if found
            cached
            (let ((result
                    (cond
                      ;; Base case: no more blinks
                      ((zerop blinks) 1)

                      ;; Rule 1: 0 becomes 1
                      ((zerop value)
                       (count-stones 1 (1- blinks)))

                      ;; Rule 2: Even number of digits -> split
                      ((evenp (count-digits value))
                       (multiple-value-bind (left right) (split-number value)
                         (+ (count-stones left (1- blinks))
                            (count-stones right (1- blinks)))))

                      ;; Rule 3: Multiply by 2024
                      (t
                       (count-stones (* value 2024) (1- blinks))))))
              (setf (gethash key memo) result)
              result))))))

(defun part1 (stones)
  "Count total stones after 25 blinks."
  (reduce #'+ (mapcar (lambda (stone) (count-stones stone 25)) stones)))

(defun part2 (stones)
  "Count total stones after 75 blinks."
  (reduce #'+ (mapcar (lambda (stone) (count-stones stone 75)) stones)))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (stones (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 stones))
    (format t "Part 2: ~A~%" (part2 stones))))

(main)
