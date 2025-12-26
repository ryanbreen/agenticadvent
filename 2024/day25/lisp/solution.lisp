#!/usr/bin/env sbcl --script
;;;; Day 25: Code Chronicle - Lock and key matching

(defun read-input (filename)
  "Read the entire input file as a string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER."
  (let ((result '())
        (start 0))
    (loop for pos = (search delimiter string :start2 start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (+ pos (length delimiter)))
          finally (push (subseq string start) result))
    (nreverse result)))

(defun parse-schematic (schematic-text)
  "Parse a single schematic into either a lock or key with heights.
Returns (type . heights) where type is :lock or :key."
  (let* ((lines (remove-if (lambda (s) (string= s ""))
                          (split-string schematic-text (string #\Newline))))
         (is-lock (string= (first lines) "#####")))
    (if is-lock
        ;; Lock: count # from top (excluding top row)
        (let ((heights '()))
          (dotimes (col 5)
            (let ((height 0))
              (loop for row from 1 to 6
                    for line = (nth row lines)
                    while (and line (char= (char line col) #\#))
                    do (incf height))
              (push height heights)))
          (cons :lock (nreverse heights)))
        ;; Key: count # from bottom (excluding bottom row)
        (let ((heights '()))
          (dotimes (col 5)
            (let ((height 0))
              (loop for row from 5 downto 0
                    for line = (nth row lines)
                    while (and line (char= (char line col) #\#))
                    do (incf height))
              (push height heights)))
          (cons :key (nreverse heights))))))

(defun parse-input (text)
  "Parse schematics into locks and keys.
Returns (locks . keys) as two lists."
  (let ((schematics (split-string (string-trim '(#\Space #\Newline) text)
                                  (format nil "~%~%")))
        (locks '())
        (keys '()))
    (dolist (schematic schematics)
      (let ((parsed (parse-schematic schematic)))
        (case (car parsed)
          (:lock (push (cdr parsed) locks))
          (:key (push (cdr parsed) keys)))))
    (cons (nreverse locks) (nreverse keys))))

(defun fits-p (lock key)
  "Check if a key fits a lock (no column sum exceeds 5)."
  (every (lambda (l k) (<= (+ l k) 5))
         lock key))

(defun part1 (locks keys)
  "Count unique lock/key pairs that fit together."
  (let ((count 0))
    (dolist (lock locks)
      (dolist (key keys)
        (when (fits-p lock key)
          (incf count))))
    count))

(defun main ()
  (let* ((input-file (merge-pathnames "../input.txt"
                                     (make-pathname :name nil :type nil
                                                   :defaults *load-truename*)))
         (text (read-input input-file))
         (parsed (parse-input text))
         (locks (car parsed))
         (keys (cdr parsed)))

    (format t "Part 1: ~A~%" (part1 locks keys))
    (format t "Part 2: Merry Christmas!~%")))

(main)
