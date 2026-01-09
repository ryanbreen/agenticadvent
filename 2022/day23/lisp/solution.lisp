#!/usr/bin/env sbcl --script
;;; Day 23: Unstable Diffusion

(defun make-pos (r c)
  "Create a position as a cons cell."
  (cons r c))

(defun pos-r (pos) (car pos))
(defun pos-c (pos) (cdr pos))

(defun split-string-by-newline (string)
  "Split a string by newline characters."
  (let ((result nil)
        (start 0)
        (len (length string)))
    (loop for i from 0 below len do
      (when (char= (char string i) #\Newline)
        (push (subseq string start i) result)
        (setf start (1+ i))))
    (when (< start len)
      (push (subseq string start) result))
    (nreverse result)))

(defun read-file-to-string (pathname)
  "Read entire file contents into a string."
  (with-open-file (stream pathname :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-input (text)
  "Parse elf positions from grid text."
  (let ((elves (make-hash-table :test 'equal))
        (r 0))
    (dolist (line (split-string-by-newline text))
      (when (> (length line) 0)
        (loop for c from 0 below (length line)
              when (char= (char line c) #\#)
                do (setf (gethash (make-pos r c) elves) t))
        (incf r)))
    elves))

(defun has-neighbor-p (elves r c)
  "Check if position has any of 8 neighbors occupied."
  (loop for dr from -1 to 1
        thereis (loop for dc from -1 to 1
                      thereis (and (not (and (= dr 0) (= dc 0)))
                                   (gethash (make-pos (+ r dr) (+ c dc)) elves)))))

(defun direction-clear-p (elves r c direction)
  "Check if the given direction is clear for the elf at (r,c)."
  (case direction
    (:n (and (not (gethash (make-pos (1- r) (1- c)) elves))
             (not (gethash (make-pos (1- r) c) elves))
             (not (gethash (make-pos (1- r) (1+ c)) elves))))
    (:s (and (not (gethash (make-pos (1+ r) (1- c)) elves))
             (not (gethash (make-pos (1+ r) c) elves))
             (not (gethash (make-pos (1+ r) (1+ c)) elves))))
    (:w (and (not (gethash (make-pos (1- r) (1- c)) elves))
             (not (gethash (make-pos r (1- c)) elves))
             (not (gethash (make-pos (1+ r) (1- c)) elves))))
    (:e (and (not (gethash (make-pos (1- r) (1+ c)) elves))
             (not (gethash (make-pos r (1+ c)) elves))
             (not (gethash (make-pos (1+ r) (1+ c)) elves))))))

(defun move-delta (direction)
  "Return (dr dc) for the given direction."
  (case direction
    (:n (values -1 0))
    (:s (values 1 0))
    (:w (values 0 -1))
    (:e (values 0 1))))

(defun simulate-round (elves directions)
  "Run one round of simulation. Returns (new-elves moved-p)."
  (let ((proposals (make-hash-table :test 'equal))     ; elf-pos -> proposed-pos
        (proposal-counts (make-hash-table :test 'equal))) ; pos -> count

    ;; Phase 1: Each elf proposes a move
    (maphash (lambda (pos ignored)
               (declare (ignore ignored))
               (let ((r (pos-r pos))
                     (c (pos-c pos)))
                 ;; Only consider moving if has any neighbor
                 (when (has-neighbor-p elves r c)
                   ;; Try each direction in order
                   (dolist (dir directions)
                     (when (direction-clear-p elves r c dir)
                       (multiple-value-bind (dr dc) (move-delta dir)
                         (let ((new-pos (make-pos (+ r dr) (+ c dc))))
                           (setf (gethash pos proposals) new-pos)
                           (incf (gethash new-pos proposal-counts 0))))
                       (return))))))
             elves)

    ;; Phase 2: Execute moves (only if unique proposal)
    (let ((new-elves (make-hash-table :test 'equal))
          (moved-p nil))
      (maphash (lambda (pos ignored)
                 (declare (ignore ignored))
                 (let ((proposed (gethash pos proposals)))
                   (if (and proposed (= 1 (gethash proposed proposal-counts 0)))
                       (progn
                         (setf (gethash proposed new-elves) t)
                         (setf moved-p t))
                       (setf (gethash pos new-elves) t))))
               elves)
      (values new-elves moved-p))))

(defun bounding-rect-empty (elves)
  "Count empty tiles in bounding rectangle."
  (let ((min-r most-positive-fixnum)
        (max-r most-negative-fixnum)
        (min-c most-positive-fixnum)
        (max-c most-negative-fixnum)
        (count 0))
    (maphash (lambda (pos ignored)
               (declare (ignore ignored))
               (let ((r (pos-r pos))
                     (c (pos-c pos)))
                 (setf min-r (min min-r r))
                 (setf max-r (max max-r r))
                 (setf min-c (min min-c c))
                 (setf max-c (max max-c c))
                 (incf count)))
             elves)
    (- (* (1+ (- max-r min-r))
          (1+ (- max-c min-c)))
       count)))

(defun rotate-directions (dirs)
  "Move first direction to the end."
  (append (rest dirs) (list (first dirs))))

(defun part1 (text)
  "Count empty tiles after 10 rounds."
  (let ((elves (parse-input text))
        (directions '(:n :s :w :e)))
    (dotimes (i 10)
      (declare (ignore i))
      (setf elves (simulate-round elves directions))
      (setf directions (rotate-directions directions)))
    (bounding-rect-empty elves)))

(defun part2 (text)
  "Find first round where no elf moves."
  (let ((elves (parse-input text))
        (directions '(:n :s :w :e))
        (round-num 0))
    (loop
      (incf round-num)
      (multiple-value-bind (new-elves moved-p) (simulate-round elves directions)
        (unless moved-p
          (return round-num))
        (setf elves new-elves)
        (setf directions (rotate-directions directions))))))

(defun main ()
  (let* ((script-path (truename *load-pathname*))
         (dir (make-pathname :directory (pathname-directory script-path)))
         (input-file (merge-pathnames "../input.txt" dir))
         (text (read-file-to-string input-file)))
    (format t "Part 1: ~a~%" (part1 text))
    (format t "Part 2: ~a~%" (part2 text))))

(main)
