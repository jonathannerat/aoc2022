(defun at-coord (grid coord)
  (destructuring-bind (i . j) coord (aref grid i j)))

(defun (setf at-coord) (val grid coord)
  (destructuring-bind (i . j) coord
     (setf (aref grid i j) val)))

(defun close-enough (grid cur next)
  (<= (- (at-coord grid next) (at-coord grid cur)) 1))

(defun neighbours (grid coord &optional (is-close? #'close-enough))
  (destructuring-bind
    ((i . j) . (rows cols)) (cons coord (array-dimensions grid))
    (let (N)
      (if (and (plusp i) (funcall is-close? grid coord (cons (1- i) j)))
        (push (cons (1- i) j) N))
      (if (and (plusp j) (funcall is-close? grid coord (cons i (1- j))))
        (push (cons i (1- j)) N))
      (if (and (plusp (- rows i 1)) (funcall is-close? grid coord (cons (1+ i) j)))
        (push (cons (1+ i) j) N))
      (if (and (plusp (- cols j 1)) (funcall is-close? grid coord (cons i (1+ j))))
        (push (cons i (1+ j)) N))
      N)))

(defun parse-input (&optional (str t))
  (let (lines grid start end (i 0))
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof) (setf lines (nreverse lines)))
        (push line lines))
    (setf grid (make-array (list (length lines) (length (car lines)))))
    (dolist (line lines)
      (dotimes (j (length line))
        (let ((c (char line j)))
          (case c
            (#\S
             (setf start (cons i j))
             (setf c #\a))
            (#\E
             (setf end (cons i j))
             (setf c #\z)))
          (setf (aref grid i j) (- (char-code c) (char-code #\a)))))
      (incf i))
    (list grid start end)))

(load "queue.lsp")

(defun grid-bfs (grid start end)
  (let ((distances (make-array (array-dimensions grid)
                               :initial-element nil))
        (q (make-queue)))
    (setf (at-coord distances start) 0)
    (enqueue start q)
    (do () ((is-queue-empty? q) (at-coord distances end))
      (let ((u (dequeue q)))
        (dolist (v (neighbours grid u))
          (when (not (at-coord distances v))
            (setf (at-coord distances v) (1+ (at-coord distances u)))
            (enqueue v q)))))))

; Part one
;; (format t "solution: ~A~%" (apply #'grid-bfs (parse-input)))

(defun grid-inv-bfs (grid start end)
  (let* ((dimensions (array-dimensions grid))
         (distances (make-array dimensions :initial-element nil))
         (q (make-queue))
         start-lst)
    (setf (at-coord distances end) 0)
    (enqueue end q)
    (do () ((is-queue-empty? q))
      (let ((u (dequeue q)))
        (dolist (v (neighbours grid u #'(lambda (g c n) (close-enough g n c))))
          (when (not (at-coord distances v))
            (setf (at-coord distances v) (1+ (at-coord distances u)))
            (enqueue v q)))))
    (destructuring-bind (rows cols) dimensions
      (dotimes (i rows)
        (dotimes (j cols)
          (if (and (zerop (aref grid i j)) (aref distances i j)) 
            (push (cons i j) start-lst))))
      (reduce #'(lambda (m s)
                  (if m (min m (at-coord distances s))
                        (at-coord distances s)))
              start-lst
              :initial-value nil))))

; Part two
(format t "solution: ~A~%" (apply #'grid-inv-bfs (parse-input)))
