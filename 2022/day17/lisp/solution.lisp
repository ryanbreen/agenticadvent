#!/usr/bin/env sbcl --script

;;; Day 17: Pyroclastic Flow - Falling rock simulation with cycle detection

;; Rock shapes as lists of (dx . dy) offsets from bottom-left
(defparameter *rocks*
  #(((0 . 0) (1 . 0) (2 . 0) (3 . 0))           ; Horizontal line
    ((1 . 0) (0 . 1) (1 . 1) (2 . 1) (1 . 2))   ; Plus
    ((0 . 0) (1 . 0) (2 . 0) (2 . 1) (2 . 2))   ; L shape
    ((0 . 0) (0 . 1) (0 . 2) (0 . 3))           ; Vertical line
    ((0 . 0) (1 . 0) (0 . 1) (1 . 1))))         ; Square

(defparameter *width* 7)

(defun read-input (filename)
  "Read jet pattern from file."
  (with-open-file (stream filename :direction :input)
    (string-trim '(#\Space #\Newline #\Return) (read-line stream))))

(defun make-coord-key (x y)
  "Create a unique key for coordinates."
  (+ x (* y 10)))

(defun simulate (jets num-rocks)
  "Simulate falling rocks and return final height."
  (let ((occupied (make-hash-table :test 'eql))
        (height 0)
        (jet-idx 0)
        (jet-len (length jets))
        (states (make-hash-table :test 'equal))
        (heights (make-array 0 :adjustable t :fill-pointer 0)))

    (loop for rock-num from 0 below num-rocks do
      (let* ((rock-type (mod rock-num 5))
             (rock (aref *rocks* rock-type))
             (x 2)
             (y (+ height 3)))

        ;; Rock falling loop
        (loop
          ;; Jet push
          (let* ((jet (char jets jet-idx))
                 (dx (if (char= jet #\>) 1 -1))
                 (can-move t))

            (setf jet-idx (mod (1+ jet-idx) jet-len))

            ;; Check horizontal movement
            (loop for (rx . ry) in rock
                  for nx = (+ x rx dx)
                  for ny = (+ y ry)
                  when (or (< nx 0)
                           (>= nx *width*)
                           (gethash (make-coord-key nx ny) occupied))
                  do (setf can-move nil) (return))

            (when can-move
              (incf x dx)))

          ;; Fall down
          (let ((can-fall t))
            (loop for (rx . ry) in rock
                  for nx = (+ x rx)
                  for ny = (+ y ry -1)
                  when (or (< ny 0)
                           (gethash (make-coord-key nx ny) occupied))
                  do (setf can-fall nil) (return))

            (if can-fall
                (decf y)
                ;; Rock stops
                (progn
                  (loop for (rx . ry) in rock
                        for nx = (+ x rx)
                        for ny = (+ y ry)
                        do (setf (gethash (make-coord-key nx ny) occupied) t)
                           (setf height (max height (1+ ny))))
                  (return)))))

        (vector-push-extend height heights)

        ;; Cycle detection for Part 2 (only when needed)
        (when (> num-rocks 10000)
          (let* ((profile-depth 30)
                 (profile (loop for col from 0 below *width*
                               collect (loop for row from 0 below profile-depth
                                            when (gethash (make-coord-key col (- height 1 row)) occupied)
                                            return row
                                            finally (return profile-depth)))))

            (let ((state (list rock-type jet-idx profile)))
              (let ((prev (gethash state states)))
                (when prev
                  ;; Found cycle
                  (let* ((cycle-start prev)
                         (cycle-len (- rock-num cycle-start))
                         (cycle-height (- height (aref heights cycle-start)))
                         (remaining (- num-rocks rock-num 1))
                         (full-cycles (floor remaining cycle-len))
                         (leftover (mod remaining cycle-len))
                         (final-height (+ height (* full-cycles cycle-height))))

                    (when (> leftover 0)
                      (incf final-height (- (aref heights (+ cycle-start leftover))
                                            (aref heights cycle-start))))

                    (return-from simulate final-height))))

              (setf (gethash state states) rock-num))))))

    height))

(defun part1 (jets)
  "Simulate 2022 rocks."
  (simulate jets 2022))

(defun part2 (jets)
  "Simulate 1000000000000 rocks with cycle detection."
  (simulate jets 1000000000000))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (jets (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 jets))
    (format t "Part 2: ~A~%" (part2 jets))))

(main)
