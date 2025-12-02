; Run: sbcl --script solution.lisp

(defun read-input ()
  "Read input file and return lines as a list."
  (with-open-file (stream "../input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun part1 ()
  "Count how many times the dial points at 0 after each rotation."
  (let ((position 50)
        (zero-count 0)
        (lines (read-input)))
    (dolist (line lines)
      (when (> (length line) 0)
        (let* ((direction (char line 0))
               (distance (parse-integer (subseq line 1))))
          ;; Update position
          (setf position
                (mod (if (char= direction #\L)
                         (- position distance)
                         (+ position distance))
                     100))
          ;; Check if we landed on 0
          (when (= position 0)
            (incf zero-count)))))
    zero-count))

(defun part2 ()
  "Count how many times the dial points at 0 during and after rotations."
  (let ((position 50)
        (zero-count 0)
        (lines (read-input)))
    (dolist (line lines)
      (when (> (length line) 0)
        (let* ((direction (char line 0))
               (distance (parse-integer (subseq line 1))))
          ;; Count zeros crossed during rotation
          (cond
            ((char= direction #\L)
             ;; Moving left (toward lower numbers)
             (cond
               ((and (> position 0) (>= distance position))
                ;; We hit 0 after 'position' steps, then every 100 steps
                (incf zero-count (1+ (floor (- distance position) 100))))
               ((and (= position 0) (>= distance 100))
                ;; Starting from 0, hit it again after 100 steps
                (incf zero-count (floor distance 100)))))
            ((char= direction #\R)
             ;; Moving right (toward higher numbers)
             (if (> position 0)
                 (let ((steps-to-zero (- 100 position)))
                   (when (>= distance steps-to-zero)
                     (incf zero-count (1+ (floor (- distance steps-to-zero) 100)))))
                 ;; Starting from 0
                 (when (>= distance 100)
                   (incf zero-count (floor distance 100))))))
          ;; Update position
          (setf position
                (mod (if (char= direction #\L)
                         (- position distance)
                         (+ position distance))
                     100)))))
    zero-count))

(defun main ()
  "Main entry point."
  (format t "Part 1: ~a~%" (part1))
  (format t "Part 2: ~a~%" (part2)))

(main)
