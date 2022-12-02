; Part one
(defun string-empty-p (str)
  "Returns True iff the string is empty"
  (= (length str) 0))

(defun calories-max-elf ()
  "Returns the calories of the elf carrying the max amount"
  (let ((max-cal 0)
        (cur-sum 0))
       (do ((line (read-line t nil 'eof) (read-line t nil 'eof))) ; read each line
           ((or (eql line 'eof))
            (format t "solution: ~A~%" (max max-cal cur-sum))) ; stop at eof
           (if (string-empty-p line) ; stop accumulating calories
               (setf max-cal (max max-cal cur-sum) cur-sum 0) ; update max-cal if needed
               (let ((cal (read-from-string line)))
                    (setf cur-sum (+ cur-sum cal)))))))

; Part two
(defun first-n (n lst)
  "Returns the first n elements in the lst list"
  (cond ((or (zerop n) (null lst)) nil)
        (t (cons (car lst) (first-n (- n 1) (cdr lst))))))

(defun insert-top3 (x lst)
  "Returns a list with the top 3 elements in (cons x lst)"
  (let ((top3 (cons x lst)))
       (first-n 3 (sort top3 #'>))))

(defun calories-top3-elfs ()
  "Returns the sum of the calories of the top 3 elfs"
  (let ((top3 ())
        (cur-sum 0))
       (do ((line (read-line t nil 'eof)
                  (read-line t nil 'eof))) ; read each line
           ((or (eql line 'eof)) ; stop at eof
            (setf top3 (insert-top3 cur-sum top3))
            (format t "solution: ~A~%" (apply #'+ top3)))
           (if (string-empty-p line) ; stop accumulating calories
               (progn
                   (setf top3 (insert-top3 cur-sum top3))
                   (setf cur-sum 0))
               (let ((cal (read-from-string line)))
                    (setf cur-sum (+ cur-sum cal)))))))
