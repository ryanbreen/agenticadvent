#!/usr/bin/env sbcl --script

;;; Day 5: Print Queue - Common Lisp Solution

(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (let ((result '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (progn
               (push (subseq string start i) result)
               (setf start (1+ i)))
          finally (push (subseq string start) result))
    (nreverse result)))

(defun split-by-double-newline (string)
  "Split a string by double newline (blank line)."
  (let ((result '())
        (start 0)
        (len (length string)))
    (loop for i from 0 below (1- len)
          when (and (char= (char string i) #\Newline)
                    (char= (char string (1+ i)) #\Newline))
          do (progn
               (push (subseq string start i) result)
               (setf start (+ i 2)))
          finally (when (< start len)
                    (push (subseq string start) result)))
    (nreverse result)))

(defun read-input (filename)
  "Read the input file and return its contents as a string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-input (input-text)
  "Parse input into rules and updates.
   Returns (values rules updates) where:
   - rules: hash table mapping page -> list of pages that must come after
   - updates: list of lists of page numbers"
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) input-text))
         (sections (split-by-double-newline trimmed))
         (rules-section (first sections))
         (updates-section (second sections))
         (rules (make-hash-table :test 'equal)))

    ;; Parse rules: X|Y means X must come before Y
    (dolist (rule-line (split-string rules-section #\Newline))
      (when (> (length rule-line) 0)
        (let* ((parts (split-string rule-line #\|))
               (before (parse-integer (first parts)))
               (after (parse-integer (second parts))))
          (push after (gethash before rules '())))))

    ;; Parse updates
    (let ((updates nil))
      (dolist (update-line (split-string updates-section #\Newline))
        (when (> (length update-line) 0)
          (let ((pages (mapcar #'parse-integer
                               (split-string update-line #\,))))
            (push pages updates))))

      (values rules (nreverse updates)))))

(defun is-valid-order-p (update rules)
  "Check if an update is in valid order according to rules."
  (let ((page-positions (make-hash-table :test 'equal)))
    ;; Build position map
    (loop for page in update
          for i from 0
          do (setf (gethash page page-positions) i))

    ;; Check each page's constraints
    (loop for page in update
          for i from 0
          do (let ((must-be-after (gethash page rules '())))
               (dolist (after-page must-be-after)
                 (let ((after-pos (gethash after-page page-positions)))
                   (when (and after-pos (< after-pos i))
                     (return-from is-valid-order-p nil))))))
    t))

(defun get-middle-element (lst)
  "Get the middle element of a list."
  (nth (floor (length lst) 2) lst))

(defun part1 (rules updates)
  "Find sum of middle page numbers for correctly-ordered updates."
  (let ((total 0))
    (dolist (update updates)
      (when (is-valid-order-p update rules)
        (incf total (get-middle-element update))))
    total))

(defun fix-order (update rules)
  "Reorder an update to satisfy all rules using a comparator-based sort."
  (flet ((compare (a b)
           ;; If a must come before b, return true
           (cond ((member b (gethash a rules '())) t)
                 ;; If b must come before a, return false
                 ((member a (gethash b rules '())) nil)
                 ;; Otherwise maintain relative order
                 (t nil))))
    (stable-sort (copy-list update) #'compare)))

(defun part2 (rules updates)
  "Find sum of middle page numbers for incorrectly-ordered updates after fixing."
  (let ((total 0))
    (dolist (update updates)
      (unless (is-valid-order-p update rules)
        (let ((fixed (fix-order update rules)))
          (incf total (get-middle-element fixed)))))
    total))

(defun main ()
  "Main entry point."
  (let* ((input-path (merge-pathnames "../input.txt"
                                       (make-pathname :directory
                                                      (pathname-directory *load-truename*))))
         (input-text (read-input input-path)))
    (multiple-value-bind (rules updates) (parse-input input-text)
      (format t "Part 1: ~a~%" (part1 rules updates))
      (format t "Part 2: ~a~%" (part2 rules updates)))))

;; Run the solution
(main)
