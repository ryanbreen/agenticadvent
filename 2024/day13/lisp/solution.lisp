#!/usr/bin/env sbcl --script

;;; Day 13: Claw Contraption
;;; Solve system of linear equations using Cramer's rule

(defun read-input-file (filename)
  "Read the entire input file as a string."
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-button-line (line)
  "Parse a button line like 'Button A: X+94, Y+34' and return (94 34)."
  (let ((x-start (+ (search "X+" line) 2))
        (comma-pos (position #\, line)))
    (let ((y-start (+ (search "Y+" line) 2)))
      (list (parse-integer (subseq line x-start comma-pos))
            (parse-integer (subseq line y-start))))))

(defun parse-prize-line (line)
  "Parse a prize line like 'Prize: X=8400, Y=5400' and return (8400 5400)."
  (let ((x-start (+ (search "X=" line) 2))
        (comma-pos (position #\, line)))
    (let ((y-start (+ (search "Y=" line) 2)))
      (list (parse-integer (subseq line x-start comma-pos))
            (parse-integer (subseq line y-start))))))

(defun parse-machines (text)
  "Parse the input text into a list of machine configurations.
   Each machine is (ax ay bx by px py)."
  (let ((machines '())
        (blocks (remove-if (lambda (s) (string= s ""))
                          (split-string text (format nil "~%~%")))))
    (dolist (block blocks)
      (let* ((lines (remove-if (lambda (s) (string= s ""))
                               (split-string block (string #\Newline))))
             (button-a (parse-button-line (first lines)))
             (button-b (parse-button-line (second lines)))
             (prize (parse-prize-line (third lines))))
        (push (list (first button-a)  ; ax
                    (second button-a) ; ay
                    (first button-b)  ; bx
                    (second button-b) ; by
                    (first prize)     ; px
                    (second prize))   ; py
              machines)))
    (nreverse machines)))

(defun split-string (string delimiter)
  "Split a string by delimiter."
  (let ((result '())
        (start 0))
    (loop for pos = (search delimiter string :start2 start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (+ pos (length delimiter)))
          finally (push (subseq string start) result))
    (nreverse result)))

(defun solve-machine (ax ay bx by px py &optional max-presses)
  "Solve for button presses using Cramer's rule.

   System of equations:
     a*ax + b*bx = px
     a*ay + b*by = py

   Solution:
     det = ax*by - ay*bx
     a = (px*by - py*bx) / det
     b = (ax*py - ay*px) / det

   Returns token cost (3*a + b) or NIL if no valid solution."
  (let ((det (- (* ax by) (* ay bx))))
    (when (zerop det)
      (return-from solve-machine nil))  ; No unique solution

    ;; Calculate numerators
    (let ((a-num (- (* px by) (* py bx)))
          (b-num (- (* ax py) (* ay px))))

      ;; Check if solutions are integers
      (when (or (/= (mod a-num det) 0)
                (/= (mod b-num det) 0))
        (return-from solve-machine nil))

      (let ((a (floor a-num det))
            (b (floor b-num det)))

        ;; Check non-negative
        (when (or (< a 0) (< b 0))
          (return-from solve-machine nil))

        ;; Check max presses constraint (Part 1)
        (when (and max-presses
                   (or (> a max-presses) (> b max-presses)))
          (return-from solve-machine nil))

        ;; Return token cost
        (+ (* 3 a) b)))))

(defun part1 (machines)
  "Part 1: Max 100 presses per button."
  (let ((total 0))
    (dolist (machine machines)
      (let ((cost (apply #'solve-machine (append machine (list 100)))))
        (when cost
          (incf total cost))))
    total))

(defun part2 (machines)
  "Part 2: Prize coordinates shifted by 10^13, no press limit."
  (let ((total 0)
        (offset 10000000000000))
    (dolist (machine machines)
      (destructuring-bind (ax ay bx by px py) machine
        ;; Shift prize coordinates
        (let ((cost (solve-machine ax ay bx by (+ px offset) (+ py offset) nil)))
          (when cost
            (incf total cost)))))
    total))

(defun main ()
  "Main entry point."
  (let* ((input-path (merge-pathnames "../input.txt"
                                      (make-pathname :directory
                                                     (pathname-directory *load-pathname*))))
         (input-text (read-input-file input-path))
         (machines (parse-machines input-text)))

    (format t "Part 1: ~A~%" (part1 machines))
    (format t "Part 2: ~A~%" (part2 machines))))

;; Run the main function
(main)
