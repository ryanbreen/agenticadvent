;;;; Day 2: Dive! - Common Lisp Solution

(defun read-commands (filename)
  "Read commands from file, returning list of (direction . value) pairs."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (plusp (length line))
          collect (let* ((space-pos (position #\Space line))
                         (direction (subseq line 0 space-pos))
                         (value (parse-integer (subseq line (1+ space-pos)))))
                    (cons direction value)))))

(defun part1 (commands)
  "Calculate position using simple movement rules."
  (let ((horizontal 0)
        (depth 0))
    (dolist (cmd commands)
      (let ((direction (car cmd))
            (value (cdr cmd)))
        (cond
          ((string= direction "forward") (incf horizontal value))
          ((string= direction "down") (incf depth value))
          ((string= direction "up") (decf depth value)))))
    (* horizontal depth)))

(defun part2 (commands)
  "Calculate position using aim-based movement rules."
  (let ((horizontal 0)
        (depth 0)
        (aim 0))
    (dolist (cmd commands)
      (let ((direction (car cmd))
            (value (cdr cmd)))
        (cond
          ((string= direction "forward")
           (incf horizontal value)
           (incf depth (* aim value)))
          ((string= direction "down") (incf aim value))
          ((string= direction "up") (decf aim value)))))
    (* horizontal depth)))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-path (merge-pathnames "../input.txt" script-dir))
         (commands (read-commands input-path)))
    (format t "Part 1: ~a~%" (part1 commands))
    (format t "Part 2: ~a~%" (part2 commands))))

(main)
