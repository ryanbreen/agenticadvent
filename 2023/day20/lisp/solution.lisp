#!/usr/bin/env sbcl --script
;;;; Day 20: Pulse Propagation - Module communication simulation

(defstruct module
  (type nil)
  (destinations nil)
  (state nil)      ; for flip-flops
  (memory nil))    ; for conjunctions (hash table)

(defun parse-input (filename)
  "Parse module configuration from input file."
  (let ((modules (make-hash-table :test 'equal)))
    ;; First pass: parse all modules
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((parts (split-arrow line))
                      (name-part (first parts))
                      (dest-part (second parts))
                      (destinations (split-comma dest-part)))
                 (cond
                   ((string= name-part "broadcaster")
                    (setf (gethash "broadcaster" modules)
                          (make-module :type :broadcaster
                                       :destinations destinations)))
                   ((char= (char name-part 0) #\%)
                    (let ((name (subseq name-part 1)))
                      (setf (gethash name modules)
                            (make-module :type :flip-flop
                                         :destinations destinations
                                         :state nil))))
                   ((char= (char name-part 0) #\&)
                    (let ((name (subseq name-part 1)))
                      (setf (gethash name modules)
                            (make-module :type :conjunction
                                         :destinations destinations
                                         :memory (make-hash-table :test 'equal)))))))))
    ;; Second pass: initialize conjunction memory
    (maphash (lambda (name module)
               (dolist (dest (module-destinations module))
                 (let ((dest-module (gethash dest modules)))
                   (when (and dest-module
                              (eq (module-type dest-module) :conjunction))
                     (setf (gethash name (module-memory dest-module)) nil)))))
             modules)
    modules))

(defun split-arrow (line)
  "Split line by ' -> '."
  (let ((pos (search " -> " line)))
    (list (subseq line 0 pos)
          (subseq line (+ pos 4)))))

(defun split-comma (str)
  "Split string by ', '."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length str)
          do (when (and (char= (char str i) #\,)
                        (< (1+ i) (length str))
                        (char= (char str (1+ i)) #\Space))
               (push (subseq str start i) result)
               (setf start (+ i 2))))
    (push (subseq str start) result)
    (nreverse result)))

(defun reset-modules (modules)
  "Reset all module states."
  (maphash (lambda (name module)
             (declare (ignore name))
             (when (eq (module-type module) :flip-flop)
               (setf (module-state module) nil))
             (when (eq (module-type module) :conjunction)
               (maphash (lambda (k v)
                          (declare (ignore v))
                          (setf (gethash k (module-memory module)) nil))
                        (module-memory module))))
           modules))

(defun simulate-button-press (modules &optional watch-nodes)
  "Simulate a single button press.
   Returns (low-count high-count high-senders)."
  (let ((low-count 0)
        (high-count 0)
        (high-senders (make-hash-table :test 'equal))
        (queue nil))
    ;; Queue: (source dest pulse) where pulse is nil for low, t for high
    (push (list "button" "broadcaster" nil) queue)
    (setf queue (nreverse queue))

    (loop while queue
          do (let* ((item (pop queue))
                    (source (first item))
                    (dest (second item))
                    (pulse (third item))
                    (module (gethash dest modules)))

               (if pulse
                   (incf high-count)
                   (incf low-count))

               ;; Track if watched nodes send high pulses
               (when (and watch-nodes pulse (gethash source watch-nodes))
                 (setf (gethash source high-senders) t))

               (when module
                 (case (module-type module)
                   (:broadcaster
                    (dolist (next-dest (module-destinations module))
                      (setf queue (nconc queue (list (list dest next-dest pulse))))))

                   (:flip-flop
                    (when (not pulse) ; Only react to low pulses
                      (setf (module-state module) (not (module-state module)))
                      (dolist (next-dest (module-destinations module))
                        (setf queue (nconc queue (list (list dest next-dest (module-state module))))))))

                   (:conjunction
                    (setf (gethash source (module-memory module)) pulse)
                    ;; Send low if all inputs are high, otherwise send high
                    (let ((all-high t))
                      (maphash (lambda (k v)
                                 (declare (ignore k))
                                 (when (not v)
                                   (setf all-high nil)))
                               (module-memory module))
                      (let ((output (not all-high)))
                        (dolist (next-dest (module-destinations module))
                          (setf queue (nconc queue (list (list dest next-dest output))))))))))))

    (list low-count high-count high-senders)))

(defun part1 (modules)
  "Part 1: Count pulses after 1000 button presses."
  (reset-modules modules)
  (let ((total-low 0)
        (total-high 0))
    (dotimes (i 1000)
      (let ((result (simulate-button-press modules)))
        (incf total-low (first result))
        (incf total-high (second result))))
    (* total-low total-high)))

(defun part2 (modules)
  "Part 2: Find minimum button presses for rx to receive a low pulse."
  (reset-modules modules)

  ;; Find the module that feeds into rx
  (let ((rx-input nil))
    (maphash (lambda (name module)
               (when (member "rx" (module-destinations module) :test #'string=)
                 (setf rx-input name)))
             modules)

    (if (null rx-input)
        0
        (let ((watch-nodes (make-hash-table :test 'equal))
              (cycle-lengths (make-hash-table :test 'equal))
              (num-watch 0))

          ;; Find all modules that feed into rx-input
          (let ((rx-input-module (gethash rx-input modules)))
            (when (and rx-input-module (module-memory rx-input-module))
              (maphash (lambda (k v)
                         (declare (ignore v))
                         (setf (gethash k watch-nodes) t)
                         (incf num-watch))
                       (module-memory rx-input-module))))

          ;; Simulate until we find all cycle lengths
          (let ((button-press 0))
            (loop while (< (hash-table-count cycle-lengths) num-watch)
                  do (incf button-press)
                     (let ((high-senders (third (simulate-button-press modules watch-nodes))))
                       (maphash (lambda (node val)
                                  (declare (ignore val))
                                  (when (not (gethash node cycle-lengths))
                                    (setf (gethash node cycle-lengths) button-press)))
                                high-senders)))

            ;; LCM of all cycle lengths
            (let ((result 1))
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (setf result (lcm result v)))
                       cycle-lengths)
              result))))))

(defun main ()
  (let* ((script-path (or *load-pathname* *compile-file-pathname*
                          (make-pathname :directory '(:relative))))
         (input-path (merge-pathnames "../input.txt" script-path)))
    (let ((modules (parse-input input-path)))
      (format t "Part 1: ~A~%" (part1 modules)))
    ;; Re-parse for Part 2 (fresh state)
    (let ((modules (parse-input input-path)))
      (format t "Part 2: ~A~%" (part2 modules)))))

(main)
