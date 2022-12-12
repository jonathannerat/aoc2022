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

(defconstant input-grid (parse-input))

; Part one
;; (format t "solution: ~A~%" (count-visible-trees input-grid))

(defun next-pos (grid x y dir &optional (steps 1))
  (destructuring-bind (rows cols) (array-dimensions grid)
    (let ((max-rows (1- rows))
          (max-cols (1- cols)))
      (case dir
        ('up    (values (max (- x steps) 0) y        (< x steps)))
        ('left  (values x (max (- y steps) 0)        (< y steps)))
        ('down  (values (min (+ x steps) max-rows) y (>= (+ x steps) rows)))
        ('right (values x (min (+ y steps) max-cols) (>= (+ y steps) cols)))))))

(defstruct entry up left down right)

(defun entry-dir (entry dir &optional default)
  (or (case dir
        ('up    (entry-up entry))
        ('left  (entry-left entry))
        ('down  (entry-down entry))
        ('right (entry-right entry)))
    default))

(defun entry-tostr (entry)
  (format nil "(~A,~A,~A,~A)"
          (entry-dir entry 'up "")
          (entry-dir entry 'left "")
          (entry-dir entry 'down "")
          (entry-dir entry 'right "")))

(defun (setf entry-dir) (value entry dir)
  (case dir
    ('up    (setf (entry-up entry) value))
    ('left  (setf (entry-left entry) value))
    ('down  (setf (entry-down entry) value))
    ('right (setf (entry-right entry) value))))

(defun count-trees-dir (grid x y dir
                             &optional (cache (make-array (array-dimensions grid)
                                                          :initial-element nil)))
  (let ((entry (aref cache x y)))
    (or 
      (and entry (entry-dir entry dir))
      (multiple-value-bind (next-x next-y overflow) (next-pos grid x y dir)
        (when (not entry)
          (setf entry (make-entry))
          (setf (aref cache x y) entry))
        (let* ((distance (if overflow 0 1))
               (height (aref grid x y))
               (next-height (aref grid next-x next-y)))
          (do nil ((or overflow (>= next-height height)))
            (let* ((next-distance (count-trees-dir grid next-x next-y dir cache))
                   (next-move (multiple-value-list
                                (next-pos grid next-x next-y dir next-distance))))
              (incf distance next-distance)
              (setf next-x (first next-move)
                    next-y (second next-move)
                    overflow (or (third next-move) (zerop next-distance))
                    next-height (aref grid next-x next-y))))
          (setf (entry-dir entry dir) distance))))))

(defun print-cache (cache)
  (destructuring-bind (rows cols) (array-dimensions cache)
    (do ((i 0 (1+ i)))
        ((= i rows))
      (do ((j 0 (1+ j)))
          ((= j cols) (format t "~%"))
        (let ((entry (aref cache i j)))
             (format t "~A" (if entry (entry-tostr entry) "()")))))))

(defun max-scenic-score (grid)
  (let* ((dimensions (array-dimensions grid))
         (cache (make-array dimensions :initial-element nil))
         (max-score))
    (destructuring-bind (rows cols) dimensions
      (do ((i 1 (1+ i)))
          ((= (1- rows) i) max-score)
            (do ((j 1 (1+ j)))
                ((= (1- cols) j))
                (let* ((distance-up (count-trees-dir grid i j 'up cache))
                       (distance-left (count-trees-dir grid i j 'left cache))
                       (distance-down (count-trees-dir grid i j 'down cache))
                       (distance-right (count-trees-dir grid i j 'right cache))
                       (score (* distance-up distance-left distance-down distance-right)))
                  (when (or (not max-score) (> score max-score))
                    (setf max-score score))))))
    max-score))

; Part two
(format t "solution: ~A~%" (max-scenic-score input-grid))
