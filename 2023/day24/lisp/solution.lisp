;;;; Day 24: Never Tell Me The Odds
;;;; Common Lisp solution using built-in ratio type for exact arithmetic

(defstruct hailstone
  px py pz vx vy vz)

(defun parse-line (line)
  "Parse a hailstone line into a hailstone struct."
  (let* ((at-pos (position #\@ line))
         (pos-part (subseq line 0 at-pos))
         (vel-part (subseq line (1+ at-pos))))
    (flet ((parse-triple (s)
             (let ((nums nil)
                   (start 0))
               (loop for i from 0 to (length s)
                     for c = (if (< i (length s)) (char s i) #\,)
                     when (eql c #\,)
                       do (let ((num-str (string-trim '(#\Space #\Tab) (subseq s start i))))
                            (push (parse-integer num-str) nums)
                            (setf start (1+ i))))
               (nreverse nums))))
      (let ((pos (parse-triple pos-part))
            (vel (parse-triple vel-part)))
        (make-hailstone :px (first pos) :py (second pos) :pz (third pos)
                        :vx (first vel) :vy (second vel) :vz (third vel))))))

(defun read-input (filename)
  "Read and parse all hailstones from input file."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length (string-trim '(#\Space #\Tab #\Return) line)) 0)
            collect (parse-line line))))

(defun find-intersection-2d (h1 h2)
  "Find intersection of two hailstone paths in XY plane.
   Returns (x y t1 t2) as rationals, or NIL if parallel."
  (let* ((px1 (hailstone-px h1)) (py1 (hailstone-py h1))
         (vx1 (hailstone-vx h1)) (vy1 (hailstone-vy h1))
         (px2 (hailstone-px h2)) (py2 (hailstone-py h2))
         (vx2 (hailstone-vx h2)) (vy2 (hailstone-vy h2))
         ;; Solve: vx1*t1 - vx2*t2 = px2 - px1
         ;;        vy1*t1 - vy2*t2 = py2 - py1
         ;; Using Cramer's rule: det = vx1*(-vy2) - (-vx2)*vy1
         (det (- (* vx1 (- vy2)) (* (- vx2) vy1))))
    (when (/= det 0)
      (let* ((dx (- px2 px1))
             (dy (- py2 py1))
             ;; t1 = (dx*(-vy2) - (-vx2)*dy) / det
             (t1 (/ (- (* dx (- vy2)) (* (- vx2) dy)) det))
             ;; t2 = (vx1*dy - dx*vy1) / det
             (t2 (/ (- (* vx1 dy) (* dx vy1)) det))
             ;; Intersection point
             (x (+ px1 (* vx1 t1)))
             (y (+ py1 (* vy1 t1))))
        (list x y t1 t2)))))

(defun part1 (hailstones &optional (min-coord 200000000000000) (max-coord 400000000000000))
  "Count intersections within test area, in the future for both hailstones."
  (let ((count 0)
        (n (length hailstones)))
    (loop for i from 0 below (1- n)
          do (loop for j from (1+ i) below n
                   for h1 = (nth i hailstones)
                   for h2 = (nth j hailstones)
                   for result = (find-intersection-2d h1 h2)
                   when result
                     do (destructuring-bind (x y t1 t2) result
                          (when (and (>= t1 0) (>= t2 0)
                                     (<= min-coord x max-coord)
                                     (<= min-coord y max-coord))
                            (incf count)))))
    count))

(defun solve-system (matrix rhs)
  "Solve 4x4 system using Gaussian elimination with exact rational arithmetic.
   Matrix is a list of 4 lists, rhs is a list of 4 values."
  (let* ((n 4)
         ;; Create augmented matrix with rationals
         (aug (make-array (list n (1+ n))))
         (solution (make-array n)))
    ;; Initialize augmented matrix
    (loop for i from 0 below n
          do (loop for j from 0 below n
                   do (setf (aref aug i j) (elt (elt matrix i) j)))
             (setf (aref aug i n) (elt rhs i)))

    ;; Forward elimination with partial pivoting
    (loop for col from 0 below n
          do (let ((max-row col))
               ;; Find pivot
               (loop for row from (1+ col) below n
                     when (> (abs (aref aug row col)) (abs (aref aug max-row col)))
                       do (setf max-row row))
               ;; Swap rows
               (when (/= max-row col)
                 (loop for j from col to n
                       do (rotatef (aref aug col j) (aref aug max-row j))))
               ;; Eliminate column
               (when (/= (aref aug col col) 0)
                 (loop for row from (1+ col) below n
                       when (/= (aref aug row col) 0)
                         do (let ((factor (/ (aref aug row col) (aref aug col col))))
                              (loop for j from col to n
                                    do (decf (aref aug row j) (* factor (aref aug col j)))))))))

    ;; Back substitution
    (loop for i from (1- n) downto 0
          do (setf (aref solution i) (aref aug i n))
             (loop for j from (1+ i) below n
                   do (decf (aref solution i) (* (aref aug i j) (aref solution j))))
             (setf (aref solution i) (/ (aref solution i) (aref aug i i))))

    (coerce solution 'list)))

(defun part2 (hailstones)
  "Find rock position and velocity that hits all hailstones.
   Returns sum of rx + ry + rz."
  ;; Take first 5 hailstones to build 4 linear equations
  (let ((h (subseq hailstones 0 5))
        (matrix-xy nil)
        (rhs-xy nil)
        (matrix-xz nil)
        (rhs-xz nil))

    ;; Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    (loop for i from 0 to 3
          for h1 = (nth i h)
          for h2 = (nth (1+ i) h)
          for px1 = (hailstone-px h1) for py1 = (hailstone-py h1)
          for vx1 = (hailstone-vx h1) for vy1 = (hailstone-vy h1)
          for px2 = (hailstone-px h2) for py2 = (hailstone-py h2)
          for vx2 = (hailstone-vx h2) for vy2 = (hailstone-vy h2)
          ;; Coefficients for rx, ry, rvx, rvy
          for a = (- vy1 vy2)
          for b = (- vx2 vx1)
          for c = (- py2 py1)
          for d = (- px1 px2)
          for e = (- (- (* px1 vy1) (* py1 vx1))
                     (- (* px2 vy2) (* py2 vx2)))
          do (push (list a b c d) matrix-xy)
             (push e rhs-xy))

    (setf matrix-xy (nreverse matrix-xy))
    (setf rhs-xy (nreverse rhs-xy))

    (let ((xy-solution (solve-system matrix-xy rhs-xy)))
      (destructuring-bind (rx ry rvx rvy) xy-solution
        (declare (ignore rvx rvy))

        ;; Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
        (loop for i from 0 to 3
              for h1 = (nth i h)
              for h2 = (nth (1+ i) h)
              for px1 = (hailstone-px h1) for pz1 = (hailstone-pz h1)
              for vx1 = (hailstone-vx h1) for vz1 = (hailstone-vz h1)
              for px2 = (hailstone-px h2) for pz2 = (hailstone-pz h2)
              for vx2 = (hailstone-vx h2) for vz2 = (hailstone-vz h2)
              ;; Same structure as XY but with Z instead of Y
              for a = (- vz1 vz2)
              for b = (- vx2 vx1)
              for c = (- pz2 pz1)
              for d = (- px1 px2)
              for e = (- (- (* px1 vz1) (* pz1 vx1))
                         (- (* px2 vz2) (* pz2 vx2)))
              do (push (list a b c d) matrix-xz)
                 (push e rhs-xz))

        (setf matrix-xz (nreverse matrix-xz))
        (setf rhs-xz (nreverse rhs-xz))

        (let ((xz-solution (solve-system matrix-xz rhs-xz)))
          (destructuring-bind (rx2 rz rvx2 rvz) xz-solution
            (declare (ignore rx2 rvx2 rvz))
            ;; Return sum of position coordinates as integer
            (values (truncate (+ rx ry rz)))))))))

(defun main ()
  (let* ((input-file (if (> (length sb-ext:*posix-argv*) 1)
                         (second sb-ext:*posix-argv*)
                         "../input.txt"))
         (hailstones (read-input input-file)))
    (format t "Part 1: ~a~%" (part1 hailstones))
    (format t "Part 2: ~a~%" (part2 hailstones))))

(main)
