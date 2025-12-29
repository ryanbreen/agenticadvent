;;;; Advent of Code 2023 Day 8: Haunted Wasteland
;;;; Common Lisp solution

(defun read-input (filename)
  "Read and parse the input file into instructions and a network hash table."
  (with-open-file (stream filename)
    (let* ((instructions (read-line stream))
           (network (make-hash-table :test 'equal)))
      ;; Skip blank line
      (read-line stream)
      ;; Parse node definitions
      (loop for line = (read-line stream nil nil)
            while line
            when (> (length line) 0)
              do (let* ((node (subseq line 0 3))
                        (left (subseq line 7 10))
                        (right (subseq line 12 15)))
                   (setf (gethash node network) (cons left right))))
      (values instructions network))))

(defun navigate (instructions network start end-test)
  "Navigate from START until END-TEST returns true for the current node.
   Returns the number of steps taken."
  (let ((current start)
        (steps 0)
        (instruction-len (length instructions)))
    (loop until (funcall end-test current)
          do (let ((instruction (char instructions (mod steps instruction-len)))
                   (neighbors (gethash current network)))
               (setf current (if (char= instruction #\L)
                                 (car neighbors)
                                 (cdr neighbors)))
               (incf steps)))
    steps))

(defun part1 (instructions network)
  "Navigate from AAA to ZZZ."
  (navigate instructions network "AAA" (lambda (node) (string= node "ZZZ"))))

(defun part2 (instructions network)
  "Navigate all nodes ending in A simultaneously to nodes ending in Z."
  ;; Find all starting nodes (ending in A) using loop collect
  (let ((starting-nodes
          (loop for key being the hash-keys of network
                when (char= (char key 2) #\A)
                collect key)))
    ;; Find cycle length for each starting node and compute LCM
    (reduce #'lcm
            (mapcar (lambda (node)
                      (navigate instructions network node
                                (lambda (n) (char= (char n 2) #\Z))))
                    starting-nodes))))

(defun main ()
  "Main entry point."
  (multiple-value-bind (instructions network)
      (read-input (merge-pathnames "../input.txt" *load-truename*))
    (format t "Part 1: ~A~%" (part1 instructions network))
    (format t "Part 2: ~A~%" (part2 instructions network))))

(main)
