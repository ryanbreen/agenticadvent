#!/usr/bin/env sbcl --script

;;;; Advent of Code 2024 Day 9: Disk Fragmenter
;;;;
;;;; Compact a fragmented disk by moving file blocks to fill gaps.
;;;; Part 1: Move blocks one at a time from end to leftmost free space
;;;; Part 2: Move whole files to leftmost span that fits

(defun parse-disk-map (filename)
  "Parse disk map into expanded block representation.
   Returns vector where each element is file ID or -1 for free space."
  (let* ((disk-map (with-open-file (stream filename)
                     (read-line stream nil)))
         ;; Calculate total size needed
         (total-size (loop for char across disk-map
                           sum (digit-char-p char)))
         (blocks (make-array total-size :element-type 'fixnum :initial-element -1))
         (file-id 0)
         (is-file t)
         (pos 0))
    (loop for char across disk-map
          for length = (digit-char-p char)
          do (if is-file
                 (progn
                   (loop repeat length do
                     (setf (aref blocks pos) file-id)
                     (incf pos))
                   (incf file-id))
                 (progn
                   (incf pos length)))
             (setf is-file (not is-file)))
    blocks))

(defun compact-blocks (blocks)
  "Compact disk by moving blocks one at a time from end to leftmost free space."
  (let ((blocks (copy-seq blocks))
        (left 0)
        (right (1- (length blocks))))

    (loop while (< left right)
          do (progn
               ;; Find leftmost free space
               (loop while (and (< left right) (/= (aref blocks left) -1))
                     do (incf left))
               ;; Find rightmost file block
               (loop while (and (< left right) (= (aref blocks right) -1))
                     do (decf right))

               ;; Swap if we have valid positions
               (when (< left right)
                 (setf (aref blocks left) (aref blocks right))
                 (setf (aref blocks right) -1)
                 (incf left)
                 (decf right))))
    blocks))

(defun calculate-checksum (blocks)
  "Calculate filesystem checksum: sum of position * file_id for each block."
  (loop for pos from 0 below (length blocks)
        for file-id = (aref blocks pos)
        when (/= file-id -1)
        sum (* pos file-id)))

(defun part1 ()
  "Compact by moving individual blocks, return checksum."
  (let* ((blocks (parse-disk-map "../input.txt"))
         (compacted (compact-blocks blocks)))
    (calculate-checksum compacted)))

(defun find-files (blocks)
  "Find all files: returns hash table mapping file_id -> (start_pos . length)"
  (let ((files (make-hash-table))
        (i 0)
        (len (length blocks)))
    (loop while (< i len)
          do (if (/= (aref blocks i) -1)
                 (let ((file-id (aref blocks i))
                       (start i))
                   (loop while (and (< i len)
                                    (= (aref blocks i) file-id))
                         do (incf i))
                   (setf (gethash file-id files) (cons start (- i start))))
                 (incf i)))
    files))

(defun find-free-span (blocks start-pos length)
  "Find leftmost span of free space of given length before start-pos.
   Returns position or nil if not found."
  (let ((i 0))
    (loop while (< i start-pos)
          do (if (= (aref blocks i) -1)
                 (let ((span-start i)
                       (span-length 0))
                   (loop while (and (< i start-pos) (= (aref blocks i) -1))
                         do (incf span-length)
                            (incf i))
                   (when (>= span-length length)
                     (return-from find-free-span span-start)))
                 (incf i)))
    nil))

(defun set-range (blocks start length value)
  "Set a range of blocks to a given value (mutating operation)."
  (loop for i from start below (+ start length)
        do (setf (aref blocks i) value)))

(defun part2 ()
  "Compact by moving whole files (highest ID first), return checksum."
  (let* ((blocks (parse-disk-map "../input.txt"))
         (files (find-files blocks))
         (max-file-id (loop for key being the hash-keys of files maximize key)))

    ;; Process files in decreasing order of file ID
    (loop for file-id from max-file-id downto 0
          do (let* ((file-info (gethash file-id files))
                    (start (car file-info))
                    (length (cdr file-info))
                    (free-start (find-free-span blocks start length)))
               ;; Move file if we found a suitable span
               (when free-start
                 ;; Clear old position
                 (set-range blocks start length -1)
                 ;; Write to new position
                 (set-range blocks free-start length file-id)
                 ;; Update file position in hash table
                 (setf (gethash file-id files) (cons free-start length)))))

    (calculate-checksum blocks)))

(defun main ()
  (format t "Part 1: ~a~%" (part1))
  (format t "Part 2: ~a~%" (part2)))

(main)
