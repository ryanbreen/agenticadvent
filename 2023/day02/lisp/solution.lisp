#!/usr/bin/env sbcl --script

;;; Advent of Code 2023 Day 2: Cube Conundrum

(defun read-input-file (filename)
  "Read input file and return list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


(defun split-string (string delimiter)
  "Split a string by delimiter character."
  (loop with result = nil
        with start = 0
        for pos = (position delimiter string :start start)
        do (push (subseq string start pos) result)
           (if pos
               (setf start (1+ pos))
               (return (nreverse result)))))

(defun trim-string (string)
  "Remove leading and trailing whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-draw (draw-str)
  "Parse a draw string like '3 red, 5 green' and return an alist of (color . count)."
  (let ((cubes (split-string draw-str #\,)))
    (loop for cube-str in cubes
          collect (let* ((trimmed (trim-string cube-str))
                        (parts (split-string trimmed #\Space))
                        (count (parse-integer (first parts)))
                        (color (intern (string-upcase (second parts)) :keyword)))
                   (cons color count)))))

(defun parse-game (line)
  "Parse a game line and return (game-id . draws)."
  (let* ((colon-pos (position #\: line))
         (game-part (subseq line 0 colon-pos))
         (draws-part (subseq line (+ colon-pos 2)))
         (game-id (parse-integer (subseq game-part 5)))
         (draw-strings (split-string draws-part #\;))
         (draws (mapcar #'parse-draw draw-strings)))
    (cons game-id draws)))

(defun game-possible-p (draws max-cubes)
  "Check if a game is possible with the given maximum cubes."
  (loop for draw in draws
        always (loop for (color . count) in draw
                     always (<= count (cdr (assoc color max-cubes))))))

(defun part1 (lines)
  "Part 1: Sum IDs of games possible with 12 red, 13 green, 14 blue."
  (let ((max-cubes '((:red . 12) (:green . 13) (:blue . 14))))
    (loop for line in lines
          for (game-id . draws) = (parse-game line)
          when (game-possible-p draws max-cubes)
          sum game-id)))

(defun find-minimum-cubes (draws)
  "Find the minimum cubes needed for a game."
  (let ((minimums (list (cons :red 0) (cons :green 0) (cons :blue 0))))
    (loop for draw in draws
          do (loop for (color . count) in draw
                   do (let ((current (cdr (assoc color minimums))))
                        (when (> count current)
                          (setf (cdr (assoc color minimums)) count)))))
    minimums))

(defun calculate-power (min-cubes)
  "Calculate the power (product) of minimum cubes."
  (* (cdr (assoc :red min-cubes))
     (cdr (assoc :green min-cubes))
     (cdr (assoc :blue min-cubes))))

(defun part2 (lines)
  "Part 2: Sum the power of minimum cube sets for each game."
  (loop for line in lines
        for (game-id . draws) = (parse-game line)
        for min-cubes = (find-minimum-cubes draws)
        sum (calculate-power min-cubes)))

(defun main ()
  "Main entry point."
  (let* ((input-file (merge-pathnames "../input.txt" *load-truename*))
         (lines (read-input-file input-file)))
    (format t "Part 1: ~A~%" (part1 lines))
    (format t "Part 2: ~A~%" (part2 lines))))

(main)
