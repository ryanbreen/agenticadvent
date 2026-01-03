#!/usr/bin/env sbcl --script
;;; Day 22: Sand Slabs - 3D falling bricks simulation

(defstruct brick
  x1 y1 z1 x2 y2 z2)

(defun split-string (string separator)
  "Split a string by a separator character."
  (let ((result '())
        (current ""))
    (loop for char across string do
      (if (char= char separator)
          (progn
            (push current result)
            (setf current ""))
          (setf current (concatenate 'string current (string char)))))
    (push current result)
    (nreverse result)))

(defun parse-brick (line)
  "Parse a brick specification from a line like '1,0,1~1,2,1'."
  (let* ((tilde-pos (position #\~ line))
         (left (subseq line 0 tilde-pos))
         (right (subseq line (1+ tilde-pos)))
         (left-parts (mapcar #'parse-integer (split-string left #\,)))
         (right-parts (mapcar #'parse-integer (split-string right #\,)))
         (x1 (first left-parts))
         (y1 (second left-parts))
         (z1 (third left-parts))
         (x2 (first right-parts))
         (y2 (second right-parts))
         (z2 (third right-parts)))
    ;; Ensure z1 <= z2
    (if (> z1 z2)
        (make-brick :x1 x2 :y1 y2 :z1 z2 :x2 x1 :y2 y1 :z2 z1)
        (make-brick :x1 x1 :y1 y1 :z1 z1 :x2 x2 :y2 y2 :z2 z2))))

(defun read-input (filename)
  "Read and parse all bricks from input file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          when (> (length line) 0)
          collect (parse-brick line))))

(defun settle-bricks (bricks)
  "Simulate bricks falling and settling.
   Returns: (values settled-bricks supports supporters)
   where supports[i] = list of bricks that brick i supports (above)
         supporters[i] = list of bricks that support brick i (below)"
  (let* ((n (length bricks))
         (indexed-bricks (loop for b in bricks
                               for i from 0
                               collect (cons i b)))
         ;; Sort by minimum z
         (sorted-bricks (sort indexed-bricks
                              (lambda (a b)
                                (< (brick-z1 (cdr a)) (brick-z1 (cdr b))))))
         (occupied (make-hash-table :test 'equal))
         (settled (make-array n :initial-element nil))
         (supports (make-hash-table))    ;; i -> set of bricks i supports
         (supporters (make-hash-table))) ;; i -> set of bricks supporting i

    ;; Initialize empty sets
    (dotimes (i n)
      (setf (gethash i supports) (make-hash-table))
      (setf (gethash i supporters) (make-hash-table)))

    (dolist (pair sorted-bricks)
      (let* ((orig-idx (car pair))
             (brick (cdr pair))
             (x1 (brick-x1 brick))
             (y1 (brick-y1 brick))
             (z1 (brick-z1 brick))
             (x2 (brick-x2 brick))
             (y2 (brick-y2 brick))
             (z2 (brick-z2 brick))
             (min-x (min x1 x2))
             (max-x (max x1 x2))
             (min-y (min y1 y2))
             (max-y (max y1 y2))
             (drop (1- z1))) ;; Maximum drop to z=1

        ;; Find how far this brick can drop
        (loop for x from min-x to max-x do
          (loop for y from min-y to max-y do
            (loop for z from (1- z1) downto 1 do
              (when (gethash (list x y z) occupied)
                (setf drop (min drop (- z1 z 1)))
                (return)))))

        ;; Calculate new positions
        (let ((new-z1 (- z1 drop))
              (new-z2 (- z2 drop)))
          (setf (aref settled orig-idx)
                (make-brick :x1 x1 :y1 y1 :z1 new-z1
                            :x2 x2 :y2 y2 :z2 new-z2))

          ;; Find supporters and mark cells occupied
          (loop for x from min-x to max-x do
            (loop for y from min-y to max-y do
              ;; Check for supporter directly below
              (let ((below (gethash (list x y (1- new-z1)) occupied)))
                (when below
                  (setf (gethash below (gethash orig-idx supporters)) t)
                  (setf (gethash orig-idx (gethash below supports)) t)))
              ;; Mark all cells of this brick
              (loop for z from new-z1 to new-z2 do
                (setf (gethash (list x y z) occupied) orig-idx)))))))

    (values settled supports supporters)))

(defun hash-subset-p (subset superset)
  "Check if all keys in subset are in superset."
  (loop for k being the hash-keys of subset
        always (gethash k superset)))

(defun part1 (bricks)
  "Count bricks that can be safely disintegrated."
  (multiple-value-bind (settled supports supporters)
      (settle-bricks bricks)
    (declare (ignore settled))
    (let ((n (length bricks))
          (safe-count 0))
      (dotimes (i n)
        (let ((can-remove t))
          ;; Check all bricks that brick i supports
          (loop for supported being the hash-keys of (gethash i supports) do
            (when (= (hash-table-count (gethash supported supporters)) 1)
              (setf can-remove nil)
              (return)))
          (when can-remove
            (incf safe-count))))
      safe-count)))

(defun part2 (bricks)
  "Count total bricks that would fall for each disintegration."
  (multiple-value-bind (settled supports supporters)
      (settle-bricks bricks)
    (declare (ignore settled))
    (let ((n (length bricks))
          (total-falls 0))
      (dotimes (i n)
        ;; BFS to find all bricks that would fall
        (let ((falling (make-hash-table))
              (queue (list i)))
          (setf (gethash i falling) t)

          (loop while queue do
            (let ((brick (pop queue)))
              ;; Check all bricks that this brick supports
              (loop for supported being the hash-keys of (gethash brick supports) do
                (unless (gethash supported falling)
                  ;; This brick falls if all its supporters have fallen
                  (when (hash-subset-p (gethash supported supporters) falling)
                    (setf (gethash supported falling) t)
                    (push supported queue))))))

          ;; Don't count the initial brick we removed
          (incf total-falls (1- (hash-table-count falling)))))
      total-falls)))

(defun main ()
  (let* ((input-path (merge-pathnames "../input.txt" *load-truename*))
         (bricks (read-input input-path)))
    (format t "Part 1: ~a~%" (part1 bricks))
    (format t "Part 2: ~a~%" (part2 bricks))))

(main)
