; Part one
(defun parse-line (line)
  (let ((last -1) a b c d)
    (dotimes (i (length line))
      (case (char line i)
        (#\- (progn
               (if (< last 0)
                     (setf a (read-from-string line t nil :end i))
                     (setf c (read-from-string line t nil :start last :end i)))
               (setf last (1+ i))))
        (#\, (progn
               (setf b (read-from-string line t nil :start last :end i))
               (setf last (1+ i))))))
    (setf d (read-from-string line t nil :start last))
    (values a b c d)))

(defun contained? (a b c d)
  (or (and (<= a c) (<= d b))
      (and (<= c a) (<= b d))))

(defun fully-contained ()
  (let ((cant 0))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
        ((eql line 'eof) (format t "solution: ~A~%" cant))
        (if (multiple-value-call #'contained? (parse-line line))
          (incf cant)))))

; Part two
(defun overlap? (a b c d)
  (or (contained? a b c d)
      (and (<= a c) (<= c b))
      (and (<= c a) (<= a d))))

(defun pairs-overlap ()
  (let ((cant 0))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
        ((eql line 'eof) (format t "solution: ~A~%" cant))
        (if (multiple-value-call #'overlap? (parse-line line))
          (incf cant)))))

(pairs-overlap)
