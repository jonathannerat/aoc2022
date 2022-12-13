(defun parse-input (&optional (str t))
  (let ((ops))
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof) (nreverse ops))
        (multiple-value-bind (op start) (read-from-string line)
          (let ((steps (parse-integer line :start start)))
            (push (cons op steps) ops))))))

(defun next-coord (coord dir)
  (destructuring-bind (x . y) coord
    (case dir
      (u (cons x (1+ y)))
      (l (cons (1- x) y))
      (d (cons x (1- y)))
      (r (cons (1+ x) y)))))

(defun diff (a b)
  (+ (abs (-  (min a b)))))

(defun distance (head tail)
  (destructuring-bind
    ((head-x . head-y) . (tail-x . tail-y))
    (cons head tail)
    (let ((diffx (- head-x tail-x))
          (diffy (- head-y tail-y)))
      (values diffx diffy
              (+ (abs diffx) (abs diffy)) ; distancia
              (and (/= diffx 0) (/= diffy 0)))))) ; estan en diagonal?

(defmacro implies (l r)
  `(or (not ,l) ,r))

(defun follow-head (head tail)
  (multiple-value-bind
    (diffx diffy dist diag)
    (distance head tail)
    (if (or (and (not diag) (>= dist 2))
            (and diag (>= dist 3)))
      (cond
        ((= diffx 0) (next-coord tail (if (> diffy 0) 'u 'd)))
        ((= diffy 0) (next-coord tail (if (> diffx 0) 'r 'l)))
        (t (next-coord
             (next-coord tail (if (> diffx 0) 'r 'l))
             (if (> diffy 0) 'u 'd))))
      tail)))

(defun count-tail-visits (ops &optional (rope-size 2))
  (let ((visited (make-hash-table :test #'equal))
        (rope (make-array rope-size :initial-element '(0 . 0))))
    (dolist (dir-steps ops)
      (destructuring-bind (dir . steps) dir-steps
        (dotimes (i steps)
          (dotimes (j rope-size)
            (let ((head (svref rope (max 0 (1- j)))))
              (if (zerop j)
                (setf (svref rope 0) (next-coord head dir))
                (let ((coord (svref rope j))
                      (new-coord (follow-head head (svref rope j))))
                  (if (equal coord new-coord)
                    (return)
                    (setf (svref rope j) new-coord))))))
          (setf (gethash (svref rope (1- rope-size)) visited) t))))
    (hash-table-count visited)))

; Part one
;; (format t "solution: ~A~%" (count-tail-visits (parse-input)))

;Part two
(format t "solution: ~A~%" (count-tail-visits (parse-input) 10))
