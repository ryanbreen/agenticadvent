#!/usr/bin/env sbcl --script
;;; Day 21: Monkey Math
;;; Evaluate expression tree (Part 1) and solve for unknown variable (Part 2)

(defparameter *monkeys* (make-hash-table :test 'equal))
(defparameter *eval-memo* (make-hash-table :test 'equal))
(defparameter *humn-memo* (make-hash-table :test 'equal))

(defun parse-input (filename)
  "Parse monkey definitions from file into *monkeys* hash table."
  (clrhash *monkeys*)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          do (let* ((colon-pos (position #\: line))
                    (name (subseq line 0 colon-pos))
                    (job (string-trim " " (subseq line (+ colon-pos 1)))))
               (if (digit-char-p (char job 0))
                   (setf (gethash name *monkeys*) (parse-integer job))
                   (let* ((parts (split-string job #\Space)))
                     (setf (gethash name *monkeys*)
                           (list (first parts)
                                 (second parts)
                                 (third parts)))))))))

(defun split-string (string delimiter)
  "Split string by delimiter character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun evaluate-monkey (name)
  "Recursively evaluate a monkey's value with memoization."
  (let ((cached (gethash name *eval-memo*)))
    (when cached
      (return-from evaluate-monkey cached)))
  (let ((job (gethash name *monkeys*)))
    (if (numberp job)
        (progn
          (setf (gethash name *eval-memo*) job)
          job)
        (let* ((left-name (first job))
               (op (second job))
               (right-name (third job))
               (left-val (evaluate-monkey left-name))
               (right-val (evaluate-monkey right-name))
               (result (cond ((string= op "+") (+ left-val right-val))
                             ((string= op "-") (- left-val right-val))
                             ((string= op "*") (* left-val right-val))
                             ((string= op "/") (floor left-val right-val)))))
          (setf (gethash name *eval-memo*) result)
          result))))

(defun contains-humn-p (name)
  "Check if the evaluation tree for NAME contains 'humn'."
  (let ((cached (gethash name *humn-memo*)))
    (when cached
      (return-from contains-humn-p (eq cached :yes))))
  (when (string= name "humn")
    (setf (gethash name *humn-memo*) :yes)
    (return-from contains-humn-p t))
  (let ((job (gethash name *monkeys*)))
    (if (numberp job)
        (progn
          (setf (gethash name *humn-memo*) :no)
          nil)
        (let* ((left-name (first job))
               (right-name (third job))
               (result (or (contains-humn-p left-name)
                           (contains-humn-p right-name))))
          (setf (gethash name *humn-memo*) (if result :yes :no))
          result))))

(defun solve-for-humn (name target)
  "Given that NAME should equal TARGET, find what 'humn' should be."
  (when (string= name "humn")
    (return-from solve-for-humn target))
  (let ((job (gethash name *monkeys*)))
    (when (numberp job)
      (return-from solve-for-humn nil))
    (let* ((left-name (first job))
           (op (second job))
           (right-name (third job))
           (left-has-humn (contains-humn-p left-name)))
      (if left-has-humn
          ;; Solve for left side
          (let* ((right-val (evaluate-monkey right-name))
                 (new-target (cond ((string= op "+") (- target right-val))
                                   ((string= op "-") (+ target right-val))
                                   ((string= op "*") (floor target right-val))
                                   ((string= op "/") (* target right-val)))))
            (solve-for-humn left-name new-target))
          ;; Solve for right side
          (let* ((left-val (evaluate-monkey left-name))
                 (new-target (cond ((string= op "+") (- target left-val))
                                   ((string= op "-") (- left-val target))
                                   ((string= op "*") (floor target left-val))
                                   ((string= op "/") (floor left-val target)))))
            (solve-for-humn right-name new-target))))))

(defun part1 ()
  "Evaluate the root monkey."
  (clrhash *eval-memo*)
  (evaluate-monkey "root"))

(defun part2 ()
  "Find what humn needs to yell for root's two values to be equal."
  (clrhash *eval-memo*)
  (clrhash *humn-memo*)
  (let* ((root-job (gethash "root" *monkeys*))
         (left-name (first root-job))
         (right-name (third root-job))
         (left-has-humn (contains-humn-p left-name)))
    (if left-has-humn
        (let ((target (evaluate-monkey right-name)))
          (solve-for-humn left-name target))
        (let ((target (evaluate-monkey left-name)))
          (solve-for-humn right-name target)))))

(defun main ()
  (let* ((script-path (or *load-pathname* *compile-file-pathname* ""))
         (script-dir (if (pathnamep script-path)
                         (directory-namestring script-path)
                         (directory-namestring (pathname script-path))))
         (input-file (merge-pathnames "../input.txt" script-dir)))
    (parse-input input-file)
    (format t "Part 1: ~A~%" (part1))
    (format t "Part 2: ~A~%" (part2))))

(main)
