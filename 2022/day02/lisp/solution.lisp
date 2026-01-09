;;;; Advent of Code 2022 - Day 2: Rock Paper Scissors

(defun read-input (filename)
  "Read the input file and return a list of (opponent you) pairs."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length line) 0)
          collect (list (char line 0) (char line 2)))))

(defun outcome-score-part1 (opponent you)
  "Calculate the outcome score for Part 1.
   A=Rock, B=Paper, C=Scissors
   X=Rock, Y=Paper, Z=Scissors
   Returns 0=loss, 3=draw, 6=win"
  (case opponent
    (#\A (case you (#\X 3) (#\Y 6) (#\Z 0)))  ; Rock vs ...
    (#\B (case you (#\X 0) (#\Y 3) (#\Z 6)))  ; Paper vs ...
    (#\C (case you (#\X 6) (#\Y 0) (#\Z 3))))) ; Scissors vs ...

(defun shape-score (you)
  "Score for the shape: X/Rock=1, Y/Paper=2, Z/Scissors=3"
  (case you (#\X 1) (#\Y 2) (#\Z 3)))

(defun part1 (rounds)
  "Calculate total score for Part 1.
   X=Rock, Y=Paper, Z=Scissors"
  (loop for (opponent you) in rounds
        sum (+ (shape-score you) (outcome-score-part1 opponent you))))

(defun choose-shape-part2 (opponent outcome)
  "Determine what shape to play for Part 2.
   Returns the shape score (1=Rock, 2=Paper, 3=Scissors)
   X=lose, Y=draw, Z=win"
  (case opponent
    (#\A (case outcome (#\X 3) (#\Y 1) (#\Z 2)))  ; vs Rock: lose=Scissors, draw=Rock, win=Paper
    (#\B (case outcome (#\X 1) (#\Y 2) (#\Z 3)))  ; vs Paper: lose=Rock, draw=Paper, win=Scissors
    (#\C (case outcome (#\X 2) (#\Y 3) (#\Z 1))))) ; vs Scissors: lose=Paper, draw=Scissors, win=Rock

(defun outcome-score-part2 (outcome)
  "Score for the outcome: X=lose=0, Y=draw=3, Z=win=6"
  (case outcome (#\X 0) (#\Y 3) (#\Z 6)))

(defun part2 (rounds)
  "Calculate total score for Part 2.
   X=lose, Y=draw, Z=win"
  (loop for (opponent outcome) in rounds
        sum (+ (choose-shape-part2 opponent outcome)
               (outcome-score-part2 outcome))))

(defun main ()
  (let* ((script-dir (directory-namestring *load-truename*))
         (input-file (merge-pathnames "../input.txt" script-dir))
         (rounds (read-input input-file)))
    (format t "Part 1: ~A~%" (part1 rounds))
    (format t "Part 2: ~A~%" (part2 rounds))))

(main)
