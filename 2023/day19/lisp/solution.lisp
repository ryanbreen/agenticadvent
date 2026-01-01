#!/usr/bin/env sbcl --script
;;; Day 19: Aplenty - Workflow processing and range analysis

(defstruct rule
  "A workflow rule with optional condition."
  attr       ; Character: #\x #\m #\a #\s, or NIL for default
  op         ; Character: #\< or #\>, or NIL
  value      ; Integer threshold, or NIL
  dest)      ; String: destination workflow or "A"/"R"

(defstruct part
  "A machine part with four ratings."
  (x 0 :type integer)
  (m 0 :type integer)
  (a 0 :type integer)
  (s 0 :type integer))

(defun parse-rule (rule-str)
  "Parse a rule string like 'x>10:one' or 'A'."
  (let ((colon-pos (position #\: rule-str)))
    (if colon-pos
        (let* ((condition (subseq rule-str 0 colon-pos))
               (dest (subseq rule-str (1+ colon-pos)))
               (attr (char condition 0))
               (op (char condition 1))
               (value (parse-integer (subseq condition 2))))
          (make-rule :attr attr :op op :value value :dest dest))
        (make-rule :dest rule-str))))

(defun parse-workflow (line)
  "Parse a workflow line like 'px{a<2006:qkq,m>2090:A,rfg}'."
  (let* ((brace-pos (position #\{ line))
         (name (subseq line 0 brace-pos))
         (rules-str (subseq line (1+ brace-pos) (1- (length line))))
         (rules-list nil)
         (start 0))
    ;; Split by comma and parse each rule
    (loop for i from 0 to (length rules-str)
          do (when (or (= i (length rules-str))
                       (char= (char rules-str i) #\,))
               (push (parse-rule (subseq rules-str start i)) rules-list)
               (setf start (1+ i))))
    (cons name (nreverse rules-list))))

(defun parse-part (line)
  "Parse a part line like '{x=787,m=2655,a=1222,s=2876}'."
  (let ((part (make-part)))
    (loop for i from 1 below (1- (length line))
          for c = (char line i)
          when (member c '(#\x #\m #\a #\s))
            do (let* ((eq-pos (position #\= line :start i))
                      (end-pos (or (position #\, line :start eq-pos)
                                   (position #\} line :start eq-pos)))
                      (val (parse-integer (subseq line (1+ eq-pos) end-pos))))
                 (case c
                   (#\x (setf (part-x part) val))
                   (#\m (setf (part-m part) val))
                   (#\a (setf (part-a part) val))
                   (#\s (setf (part-s part) val)))))
    part))

(defun read-input (filename)
  "Read and parse the input file. Returns (workflows . parts)."
  (with-open-file (stream filename :direction :input)
    (let ((workflows (make-hash-table :test 'equal))
          (parts nil)
          (reading-parts nil))
      (loop for line = (read-line stream nil nil)
            while line
            do (cond
                 ((string= line "") (setf reading-parts t))
                 (reading-parts (push (parse-part line) parts))
                 (t (let ((wf (parse-workflow line)))
                      (setf (gethash (car wf) workflows) (cdr wf))))))
      (cons workflows (nreverse parts)))))

(defun get-part-attr (part attr)
  "Get a part's attribute value by character."
  (case attr
    (#\x (part-x part))
    (#\m (part-m part))
    (#\a (part-a part))
    (#\s (part-s part))))

(defun rule-matches-p (rule part)
  "Check if a rule's condition matches the part."
  (if (null (rule-attr rule))
      t  ; Default rule always matches
      (let ((val (get-part-attr part (rule-attr rule))))
        (if (char= (rule-op rule) #\<)
            (< val (rule-value rule))
            (> val (rule-value rule))))))

(defun process-part (workflows part)
  "Process a part through workflows, return T if accepted."
  (let ((current "in"))
    (loop while (and (not (string= current "A"))
                     (not (string= current "R")))
          do (loop for rule in (gethash current workflows)
                   when (rule-matches-p rule part)
                     do (setf current (rule-dest rule))
                        (return)))
    (string= current "A")))

(defun part1 (workflows parts)
  "Sum ratings of all accepted parts."
  (loop for part in parts
        when (process-part workflows part)
          sum (+ (part-x part) (part-m part) (part-a part) (part-s part))))

;;; Part 2: Range-based analysis

(defstruct ranges
  "Ranges for each attribute, each is (min . max) inclusive."
  (x (cons 1 4000))
  (m (cons 1 4000))
  (a (cons 1 4000))
  (s (cons 1 4000)))

(defun clone-ranges (r)
  "Deep copy a ranges structure."
  (make-ranges
   :x (cons (car (ranges-x r)) (cdr (ranges-x r)))
   :m (cons (car (ranges-m r)) (cdr (ranges-m r)))
   :a (cons (car (ranges-a r)) (cdr (ranges-a r)))
   :s (cons (car (ranges-s r)) (cdr (ranges-s r)))))

(defun get-range (ranges attr)
  "Get the range for an attribute."
  (case attr
    (#\x (ranges-x ranges))
    (#\m (ranges-m ranges))
    (#\a (ranges-a ranges))
    (#\s (ranges-s ranges))))

(defun set-range (ranges attr new-range)
  "Set the range for an attribute."
  (case attr
    (#\x (setf (ranges-x ranges) new-range))
    (#\m (setf (ranges-m ranges) new-range))
    (#\a (setf (ranges-a ranges) new-range))
    (#\s (setf (ranges-s ranges) new-range))))

(defun count-combinations (ranges)
  "Count all combinations in the given ranges."
  (* (max 0 (1+ (- (cdr (ranges-x ranges)) (car (ranges-x ranges)))))
     (max 0 (1+ (- (cdr (ranges-m ranges)) (car (ranges-m ranges)))))
     (max 0 (1+ (- (cdr (ranges-a ranges)) (car (ranges-a ranges)))))
     (max 0 (1+ (- (cdr (ranges-s ranges)) (car (ranges-s ranges)))))))

(defun count-accepted (workflows workflow ranges)
  "Count combinations that lead to acceptance using range splitting."
  (cond
    ((string= workflow "R") 0)
    ((string= workflow "A") (count-combinations ranges))
    (t
     (let ((total 0)
           (current-ranges (clone-ranges ranges)))
       (loop for rule in (gethash workflow workflows)
             do (if (null (rule-attr rule))
                    ;; Default rule: all remaining goes to destination
                    (incf total (count-accepted workflows (rule-dest rule) current-ranges))
                    ;; Conditional rule: split ranges
                    (let* ((attr (rule-attr rule))
                           (op (rule-op rule))
                           (value (rule-value rule))
                           (range (get-range current-ranges attr))
                           (lo (car range))
                           (hi (cdr range)))
                      (if (char= op #\<)
                          ;; x < value: [lo, value-1] matches, [value, hi] continues
                          (progn
                            (when (< lo value)
                              (let ((new-ranges (clone-ranges current-ranges)))
                                (set-range new-ranges attr (cons lo (min hi (1- value))))
                                (incf total (count-accepted workflows (rule-dest rule) new-ranges))))
                            (if (>= hi value)
                                (set-range current-ranges attr (cons (max lo value) hi))
                                (return)))  ; No remaining range
                          ;; x > value: [value+1, hi] matches, [lo, value] continues
                          (progn
                            (when (> hi value)
                              (let ((new-ranges (clone-ranges current-ranges)))
                                (set-range new-ranges attr (cons (max lo (1+ value)) hi))
                                (incf total (count-accepted workflows (rule-dest rule) new-ranges))))
                            (if (<= lo value)
                                (set-range current-ranges attr (cons lo (min hi value)))
                                (return)))))))  ; No remaining range
       total))))

(defun part2 (workflows)
  "Count all possible accepted combinations (1-4000 for each rating)."
  (count-accepted workflows "in" (make-ranges)))

(defun main ()
  (let* ((input (read-input (merge-pathnames "../input.txt" *load-pathname*)))
         (workflows (car input))
         (parts (cdr input)))
    (format t "Part 1: ~A~%" (part1 workflows parts))
    (format t "Part 2: ~A~%" (part2 workflows))))

(main)
