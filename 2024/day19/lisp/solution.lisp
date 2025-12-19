#!/usr/bin/env sbcl --script

;;; Day 19: Linen Layout
;;; Count designs that can be formed from towel patterns and count all ways

(defun split-by-comma (string)
  "Split a string by comma into a list of trimmed strings."
  (let ((result '())
        (current "")
        (in-token nil))
    (loop for char across string
          do (cond
               ((char= char #\,)
                (when in-token
                  (push (string-trim '(#\Space #\Tab) current) result)
                  (setf current "")
                  (setf in-token nil)))
               ((member char '(#\Space #\Tab))
                (when in-token
                  (setf current (concatenate 'string current (string char)))))
               (t
                (setf in-token t)
                (setf current (concatenate 'string current (string char))))))
    (when (and in-token (> (length current) 0))
      (push (string-trim '(#\Space #\Tab) current) result))
    (nreverse result)))

(defun read-input (filename)
  "Read input file, returning (patterns . designs)."
  (with-open-file (stream filename)
    (let* ((patterns-line (read-line stream nil))
           (patterns (split-by-comma patterns-line))
           (designs '()))
      ;; Skip blank line
      (read-line stream nil)
      ;; Read designs
      (loop for line = (read-line stream nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (when (> (length trimmed) 0)
                   (push trimmed designs))))
      (cons patterns (nreverse designs)))))

(defun string-prefix-p (prefix string start)
  "Check if STRING starting at START has PREFIX."
  (let ((prefix-len (length prefix))
        (string-len (length string)))
    (when (<= (+ start prefix-len) string-len)
      (loop for i from 0 below prefix-len
            always (char= (char prefix i) (char string (+ start i)))))))

(defun count-ways (design patterns)
  "Count the number of ways to form DESIGN from PATTERNS using DP with memoization."
  (let ((memo (make-hash-table :test 'eql))
        (design-len (length design)))
    (labels ((dp (pos)
               (cond
                 ((= pos design-len) 1)
                 ((gethash pos memo))
                 (t
                  (let ((total 0))
                    (dolist (pattern patterns)
                      (when (string-prefix-p pattern design pos)
                        (incf total (dp (+ pos (length pattern))))))
                    (setf (gethash pos memo) total)
                    total)))))
      (dp 0))))

(defun part1 (patterns designs)
  "Count how many designs can be formed by patterns."
  (count-if (lambda (design)
              (> (count-ways design patterns) 0))
            designs))

(defun part2 (patterns designs)
  "Sum the number of ways to form each design."
  (reduce #'+ (mapcar (lambda (design)
                        (count-ways design patterns))
                      designs)))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (input (read-input input-file))
         (patterns (car input))
         (designs (cdr input)))
    (format t "Part 1: ~A~%" (part1 patterns designs))
    (format t "Part 2: ~A~%" (part2 patterns designs))))

(main)
