#!/usr/bin/env sbcl --script

(defun read-input (filename)
  "Read input file and return list of lines"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun find-start-col (lines)
  "Find the column where 'S' appears in the first row"
  (when lines
    (position #\S (first lines))))

(defun part1 (lines)
  "Count how many times the beam is split (Part 1)"
  (let* ((rows (length lines))
         (cols (if (> rows 0) (length (first lines)) 0))
         (start-col (find-start-col lines)))

    (if (null start-col)
        0
        (let ((active-beams (list start-col))
              (split-count 0))

          ;; Process row by row starting from row 1 (below S)
          (loop for row from 1 below rows do
            (let ((new-beams nil))

              ;; Process each active beam
              (dolist (col active-beams)
                (when (and (>= col 0) (< col cols))
                  (let ((cell (char (nth row lines) col)))
                    (cond
                      ;; Beam hits splitter
                      ((char= cell #\^)
                       (incf split-count)
                       ;; Emit left and right beams
                       (when (>= (1- col) 0)
                         (push (1- col) new-beams))
                       (when (< (1+ col) cols)
                         (push (1+ col) new-beams)))

                      ;; Beam continues straight down through empty space or other cells
                      (t
                       (push col new-beams))))))

              ;; Remove duplicates (beam merging)
              (setf active-beams (remove-duplicates new-beams))

              ;; If no more beams, stop
              (when (null active-beams)
                (return))))

          split-count))))

(defun part2 (lines)
  "Count the number of distinct timelines (Part 2)"
  (let* ((rows (length lines))
         (cols (if (> rows 0) (length (first lines)) 0))
         (start-col (find-start-col lines)))

    (if (null start-col)
        0
        (let ((timelines (make-hash-table)))
          ;; Start with 1 timeline at the starting column
          (setf (gethash start-col timelines) 1)

          ;; Process row by row starting from row 1 (below S)
          (loop for row from 1 below rows do
            (let ((new-timelines (make-hash-table)))

              ;; Process each column with active timelines
              (maphash
               (lambda (col count)
                 (when (and (>= col 0) (< col cols))
                   (let ((cell (char (nth row lines) col)))
                     (cond
                       ;; Each timeline splits into 2 (left and right)
                       ((char= cell #\^)
                        (when (>= (1- col) 0)
                          (incf (gethash (1- col) new-timelines 0) count))
                        (when (< (1+ col) cols)
                          (incf (gethash (1+ col) new-timelines 0) count)))

                       ;; Timelines continue straight down
                       (t
                        (incf (gethash col new-timelines 0) count))))))
               timelines)

              (setf timelines new-timelines)

              ;; If no more timelines, stop
              (when (zerop (hash-table-count timelines))
                (return))))

          ;; Sum all timeline counts
          (let ((total 0))
            (maphash (lambda (col count)
                      (declare (ignore col))
                      (incf total count))
                    timelines)
            total)))))

(defun main ()
  (let* ((input-path (merge-pathnames "../input.txt"
                                      (make-pathname :defaults *load-truename*)))
         (lines (read-input input-path)))

    (format t "Part 1: ~a~%" (part1 lines))
    (format t "Part 2: ~a~%" (part2 lines))))

(main)
