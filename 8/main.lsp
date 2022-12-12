(defun parse-input (&optional (str t))
  (let ((lines) (grid))
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
      ((eql line 'eof))
      (push line lines))
    (setf grid (make-array (list (length lines) (length (car lines)))))
    (let ((i 0)
          (code0 (char-code #\0)))
      (dolist (line (reverse lines))
        (dotimes (j (length line))
          (setf (aref grid i j) (- (char-code (char line j)) code0)))
        (incf i)))
    grid))

; Count visible trees by iterating a row / col, and counting how many times
; a max value is updated
(defun count-visible-trees (grid)
  (let* ((dimensions (array-dimensions grid))
         (visible-grid (make-array dimensions :initial-element 0)))
    (destructuring-bind (rows cols) dimensions
      (dotimes (i rows)
        (let ((rmax)   ; row max
              (rrmax)) ; reverse row max
          (dotimes (j cols)
              (if (or (not rmax) (> (aref grid i j) rmax))
                (progn 
                  (setf (aref visible-grid i j) 1)
                  (setf rmax (aref grid i j))))
              (let ((rj (- cols j 1))) ; reverse j
                (if (or (not rrmax) (> (aref grid i rj) rrmax))
                  (progn
                    (setf (aref visible-grid i rj) 1)
                    (setf rrmax (aref grid i rj))))))))
      (dotimes (j cols)
        (let ((cmax)   ; col max
              (rcmax)) ; reverse col max
          (dotimes (i rows)
            (if (or (not cmax) (> (aref grid i j) cmax))
              (progn 
                  (setf (aref visible-grid i j) 1)
                  (setf cmax (aref grid i j))))
            (let ((ri (- rows i 1))) ; reverse i
              (if (or (not rcmax) (> (aref grid ri j) rcmax))
                (progn
                    (setf (aref visible-grid ri j) 1)
                    (setf rcmax (aref grid ri j)))))))))
    (reduce #'+ (make-array (array-total-size visible-grid) :displaced-to visible-grid))))

(defconstant grid (parse-input))

; Part one
(format t "solution: ~A~%" (count-visible-trees grid))
