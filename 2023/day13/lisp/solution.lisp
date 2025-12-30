#!/usr/bin/env sbcl --script

(defun read-input-file (filename)
  "Read the entire file into a string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-by-double-newline (text)
  "Split text by double newlines (blank lines)."
  (let ((result nil)
        (current "")
        (i 0)
        (len (length text)))
    (loop while (< i len)
          do (let ((ch (char text i)))
               (cond
                 ;; Check for double newline (or any variation)
                 ((and (char= ch #\Newline)
                       (< (+ i 1) len)
                       (char= (char text (+ i 1)) #\Newline))
                  (when (> (length (string-trim '(#\Space #\Newline #\Return #\Tab) current)) 0)
                    (push current result))
                  (setf current "")
                  ;; Skip all consecutive newlines
                  (loop while (and (< i len)
                                   (or (char= (char text i) #\Newline)
                                       (char= (char text i) #\Return)))
                        do (incf i))
                  (decf i)) ; Back up one since loop will increment
                 ;; Skip carriage returns
                 ((char= ch #\Return)
                  nil)
                 ;; Add normal character
                 (t
                  (setf current (concatenate 'string current (string ch)))))
               (incf i)))
    ;; Add final block if non-empty
    (when (> (length (string-trim '(#\Space #\Newline #\Return #\Tab) current)) 0)
      (push current result))
    (nreverse result)))

(defun split-by-newline (text)
  "Split text by newlines into list of strings."
  (let ((result nil)
        (current ""))
    (loop for ch across text
          do (cond
               ((char= ch #\Newline)
                (push current result)
                (setf current ""))
               ((char= ch #\Return)
                nil) ; Skip carriage returns
               (t
                (setf current (concatenate 'string current (string ch))))))
    ;; Add final line if non-empty
    (when (> (length current) 0)
      (push current result))
    (nreverse result)))

(defun parse-input (text)
  "Parse input into list of patterns (each pattern is a list of strings)."
  (let* ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) text))
         (blocks (split-by-double-newline trimmed)))
    (mapcar #'split-by-newline blocks)))

(defun find-vertical-reflection (pattern)
  "Find vertical line of reflection. Returns columns to the left, or 0 if none."
  (if (null pattern)
      0
      (let ((width (length (first pattern))))
        (loop for col from 1 below width
              do (let ((is-reflection t))
                   (loop for row in pattern
                         do (let* ((left (subseq row 0 col))
                                   (right (subseq row col))
                                   (left-rev (reverse left))
                                   (min-len (min (length left-rev) (length right))))
                              (when (not (string= left-rev right
                                                  :end1 min-len
                                                  :end2 min-len))
                                (setf is-reflection nil)
                                (return))))
                   (when is-reflection
                     (return-from find-vertical-reflection col)))
              finally (return 0)))))

(defun find-horizontal-reflection (pattern)
  "Find horizontal line of reflection. Returns rows above, or 0 if none."
  (if (null pattern)
      0
      (let ((height (length pattern)))
        (loop for row from 1 below height
              do (let* ((top (subseq pattern 0 row))
                        (bottom (subseq pattern row))
                        (top-rev (reverse top))
                        (min-len (min (length top-rev) (length bottom)))
                        (is-reflection t))
                   (loop for i from 0 below min-len
                         do (when (not (string= (nth i top-rev) (nth i bottom)))
                              (setf is-reflection nil)
                              (return)))
                   (when is-reflection
                     (return-from find-horizontal-reflection row)))
              finally (return 0)))))

(defun summarize-pattern (pattern)
  "Get the summary value for a pattern."
  (let ((v (find-vertical-reflection pattern)))
    (if (> v 0)
        v
        (* (find-horizontal-reflection pattern) 100))))

(defun part1 (patterns)
  "Calculate the sum of all pattern summaries."
  (reduce #'+ (mapcar #'summarize-pattern patterns)))

(defun count-differences (s1 s2)
  "Count character differences between two strings."
  (let ((min-len (min (length s1) (length s2))))
    (loop for i from 0 below min-len
          count (not (char= (char s1 i) (char s2 i))))))

(defun find-vertical-reflection-with-smudge (pattern)
  "Find vertical line with exactly one smudge fix needed."
  (if (null pattern)
      0
      (let ((width (length (first pattern))))
        (loop for col from 1 below width
              do (let ((total-diff 0))
                   (loop for row in pattern
                         do (let* ((left (subseq row 0 col))
                                   (right (subseq row col))
                                   (left-rev (reverse left))
                                   (min-len (min (length left-rev) (length right))))
                              (incf total-diff (count-differences left-rev right))
                              (when (> total-diff 1)
                                (return))))
                   (when (= total-diff 1)
                     (return-from find-vertical-reflection-with-smudge col)))
              finally (return 0)))))

(defun find-horizontal-reflection-with-smudge (pattern)
  "Find horizontal line with exactly one smudge fix needed."
  (if (null pattern)
      0
      (let ((height (length pattern)))
        (loop for row from 1 below height
              do (let* ((top (subseq pattern 0 row))
                        (bottom (subseq pattern row))
                        (top-rev (reverse top))
                        (min-len (min (length top-rev) (length bottom)))
                        (total-diff 0))
                   (loop for i from 0 below min-len
                         do (progn
                              (incf total-diff (count-differences (nth i top-rev) (nth i bottom)))
                              (when (> total-diff 1)
                                (return))))
                   (when (= total-diff 1)
                     (return-from find-horizontal-reflection-with-smudge row)))
              finally (return 0)))))

(defun summarize-pattern-with-smudge (pattern)
  "Get the summary value for a pattern with smudge fix."
  (let ((v (find-vertical-reflection-with-smudge pattern)))
    (if (> v 0)
        v
        (* (find-horizontal-reflection-with-smudge pattern) 100))))

(defun part2 (patterns)
  "Calculate the sum with smudge fixes."
  (reduce #'+ (mapcar #'summarize-pattern-with-smudge patterns)))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (text (read-input-file input-file))
         (patterns (parse-input text)))
    (format t "Part 1: ~a~%" (part1 patterns))
    (format t "Part 2: ~a~%" (part2 patterns))))

(main)
