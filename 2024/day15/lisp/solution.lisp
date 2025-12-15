#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read and parse input file."
  (with-open-file (stream filename)
    (let ((lines '())
          (line nil))
      (loop while (setf line (read-line stream nil))
            do (push line lines))
      (nreverse lines))))

(defun parse-input (lines)
  "Parse input into grid and moves."
  (let ((grid-lines '())
        (move-lines '())
        (in-moves nil))
    (dolist (line lines)
      (if (string= line "")
          (setf in-moves t)
          (if in-moves
              (push line move-lines)
              (push line grid-lines))))
    (let* ((grid-lines (nreverse grid-lines))
           (grid (make-array (list (length grid-lines)
                                  (length (first grid-lines)))
                            :initial-element #\.))
           (moves (apply #'concatenate 'string (nreverse move-lines))))
      ;; Fill grid
      (loop for row from 0
            for line in grid-lines
            do (loop for col from 0
                     for ch across line
                     do (setf (aref grid row col) ch)))
      (values grid moves))))

(defun find-robot (grid)
  "Find the robot position in the grid."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1)))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (char= (aref grid r c) #\@)
                   do (return-from find-robot (cons r c))))))

(defun move-robot (grid robot-pos direction)
  "Move robot in the given direction, pushing boxes if needed."
  (let* ((dr-dc (cond ((char= direction #\<) '(0 . -1))
                      ((char= direction #\>) '(0 . 1))
                      ((char= direction #\^) '(-1 . 0))
                      ((char= direction #\v) '(1 . 0))))
         (dr (car dr-dc))
         (dc (cdr dr-dc))
         (r (car robot-pos))
         (c (cdr robot-pos))
         (nr (+ r dr))
         (nc (+ c dc)))

    ;; Check if blocked by wall
    (when (char= (aref grid nr nc) #\#)
      (return-from move-robot robot-pos))

    ;; Empty space - just move
    (when (char= (aref grid nr nc) #\.)
      (setf (aref grid r c) #\.)
      (setf (aref grid nr nc) #\@)
      (return-from move-robot (cons nr nc)))

    ;; Box - try to push
    (when (char= (aref grid nr nc) #\O)
      (let ((check-r nr)
            (check-c nc))
        ;; Find end of box chain
        (loop while (char= (aref grid check-r check-c) #\O)
              do (incf check-r dr)
                 (incf check-c dc))

        ;; If end is wall, can't push
        (when (char= (aref grid check-r check-c) #\#)
          (return-from move-robot robot-pos))

        ;; Move boxes
        (setf (aref grid check-r check-c) #\O)
        (setf (aref grid r c) #\.)
        (setf (aref grid nr nc) #\@)
        (return-from move-robot (cons nr nc))))

    robot-pos))

(defun calculate-gps (grid &optional (box-char #\O))
  "Calculate GPS sum for all boxes."
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (total 0))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (char= (aref grid r c) box-char)
                   do (incf total (+ (* 100 r) c))))
    total))

(defun part1 (filename)
  "Solve part 1."
  (let ((lines (read-input filename)))
    (multiple-value-bind (grid moves) (parse-input lines)
      (let ((robot-pos (find-robot grid)))
        (loop for move across moves
              do (setf robot-pos (move-robot grid robot-pos move)))
        (calculate-gps grid)))))

(defun scale-grid (grid)
  "Scale grid 2x wide for part 2."
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (new-grid (make-array (list rows (* 2 cols))
                              :initial-element #\.)))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   for ch = (aref grid r c)
                   do (cond ((char= ch #\#)
                            (setf (aref new-grid r (* 2 c)) #\#)
                            (setf (aref new-grid r (1+ (* 2 c))) #\#))
                           ((char= ch #\O)
                            (setf (aref new-grid r (* 2 c)) #\[)
                            (setf (aref new-grid r (1+ (* 2 c))) #\]))
                           ((char= ch #\.)
                            (setf (aref new-grid r (* 2 c)) #\.)
                            (setf (aref new-grid r (1+ (* 2 c))) #\.))
                           ((char= ch #\@)
                            (setf (aref new-grid r (* 2 c)) #\@)
                            (setf (aref new-grid r (1+ (* 2 c))) #\.)))))
    new-grid))

(defun can-move-box-vertical (grid box-left-c r dr)
  "Check if a wide box can move vertically."
  (let* ((nr (+ r dr))
         (left-c box-left-c)
         (right-c (1+ box-left-c))
         (left-target (aref grid nr left-c))
         (right-target (aref grid nr right-c)))

    ;; Wall blocks
    (when (or (char= left-target #\#) (char= right-target #\#))
      (return-from can-move-box-vertical nil))

    ;; Collect boxes to check
    (let ((boxes-to-check '()))
      (when (char= left-target #\[)
        (push (cons nr left-c) boxes-to-check))
      (when (char= left-target #\])
        (push (cons nr (1- left-c)) boxes-to-check))
      (when (char= right-target #\[)
        (push (cons nr right-c) boxes-to-check))
      (when (char= right-target #\])
        (push (cons nr (1- right-c)) boxes-to-check))

      ;; Remove duplicates
      (setf boxes-to-check (remove-duplicates boxes-to-check :test #'equal))

      ;; Recursively check
      (dolist (box boxes-to-check)
        (unless (can-move-box-vertical grid (cdr box) (car box) dr)
          (return-from can-move-box-vertical nil)))

      t)))

(defun collect-boxes-vertical (grid box-left-c r dr collected)
  "Collect all boxes that need to move."
  (let ((key (cons r box-left-c)))
    (unless (gethash key collected)
      (setf (gethash key collected) t)
      (let* ((nr (+ r dr))
             (left-c box-left-c)
             (right-c (1+ box-left-c))
             (left-target (aref grid nr left-c))
             (right-target (aref grid nr right-c))
             (boxes-to-check '()))

        (when (char= left-target #\[)
          (push (cons nr left-c) boxes-to-check))
        (when (char= left-target #\])
          (push (cons nr (1- left-c)) boxes-to-check))
        (when (char= right-target #\[)
          (push (cons nr right-c) boxes-to-check))
        (when (char= right-target #\])
          (push (cons nr (1- right-c)) boxes-to-check))

        ;; Remove duplicates
        (setf boxes-to-check (remove-duplicates boxes-to-check :test #'equal))

        (dolist (box boxes-to-check)
          (collect-boxes-vertical grid (cdr box) (car box) dr collected))))))

(defun move-robot-wide (grid robot-pos direction)
  "Move robot in wide grid."
  (let* ((dr-dc (cond ((char= direction #\<) '(0 . -1))
                      ((char= direction #\>) '(0 . 1))
                      ((char= direction #\^) '(-1 . 0))
                      ((char= direction #\v) '(1 . 0))))
         (dr (car dr-dc))
         (dc (cdr dr-dc))
         (r (car robot-pos))
         (c (cdr robot-pos))
         (nr (+ r dr))
         (nc (+ c dc))
         (target (aref grid nr nc)))

    ;; Wall blocks
    (when (char= target #\#)
      (return-from move-robot-wide robot-pos))

    ;; Empty space
    (when (char= target #\.)
      (setf (aref grid r c) #\.)
      (setf (aref grid nr nc) #\@)
      (return-from move-robot-wide (cons nr nc)))

    ;; Box pushing
    (when (or (char= target #\[) (char= target #\]))
      (if (/= dc 0)
          ;; Horizontal movement
          (let ((check-c nc))
            ;; Find end of box chain
            (loop while (or (char= (aref grid r check-c) #\[)
                           (char= (aref grid r check-c) #\]))
                  do (incf check-c dc))

            ;; Wall blocks
            (when (char= (aref grid r check-c) #\#)
              (return-from move-robot-wide robot-pos))

            ;; Shift boxes
            (if (> dc 0)
                ;; Moving right
                (loop for col from check-c downto (1+ nc)
                      do (setf (aref grid r col) (aref grid r (1- col))))
                ;; Moving left
                (loop for col from check-c below nc
                      do (setf (aref grid r col) (aref grid r (1+ col)))))

            (setf (aref grid r c) #\.)
            (setf (aref grid nr nc) #\@)
            (return-from move-robot-wide (cons nr nc)))

          ;; Vertical movement
          (let ((box-left-c (if (char= target #\[) nc (1- nc))))
            ;; Check if can move
            (unless (can-move-box-vertical grid box-left-c nr dr)
              (return-from move-robot-wide robot-pos))

            ;; Collect boxes to move
            (let ((boxes-to-move (make-hash-table :test #'equal)))
              (collect-boxes-vertical grid box-left-c nr dr boxes-to-move)

              ;; Sort boxes by row
              (let ((sorted-boxes '()))
                (maphash (lambda (k v)
                          (declare (ignore v))
                          (push k sorted-boxes))
                        boxes-to-move)
                (setf sorted-boxes
                      (sort sorted-boxes
                            (if (> dr 0) #'> #'<)
                            :key #'car))

                ;; Move all boxes
                (dolist (box sorted-boxes)
                  (let ((box-r (car box))
                        (box-c (cdr box)))
                    (setf (aref grid box-r box-c) #\.)
                    (setf (aref grid box-r (1+ box-c)) #\.)
                    (setf (aref grid (+ box-r dr) box-c) #\[)
                    (setf (aref grid (+ box-r dr) (1+ box-c)) #\])))

                ;; Move robot
                (setf (aref grid r c) #\.)
                (setf (aref grid nr nc) #\@)
                (return-from move-robot-wide (cons nr nc)))))))

    robot-pos))

(defun part2 (filename)
  "Solve part 2."
  (let ((lines (read-input filename)))
    (multiple-value-bind (grid moves) (parse-input lines)
      (setf grid (scale-grid grid))
      (let ((robot-pos (find-robot grid)))
        (loop for move across moves
              do (setf robot-pos (move-robot-wide grid robot-pos move)))
        (calculate-gps grid #\[)))))

(let ((input-file (merge-pathnames "../input.txt" *load-truename*)))
  (format t "Part 1: ~a~%" (part1 input-file))
  (format t "Part 2: ~a~%" (part2 input-file)))
