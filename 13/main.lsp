; Feed the input file piped through `sed 's/,/ /g;s/\[/(/g;s/]/)/g'`
; for easier parsing
(defun parse-input (&optional (str t))
  (let (pairs last-pair)
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof)
         (push (nreverse last-pair) pairs)
         (nreverse pairs))
      (let ((comp (read-from-string line nil 'eol)))
        (cond
          ((eql comp 'eol)
           (push (nreverse last-pair) pairs)
           (setf last-pair nil))
          (t (push comp last-pair)))))))

; returns the order ('lt, 'eq, 'gt) of elements left and right
(defun pair-order (left right) 
  (cond
    ((and (typep left 'number) (typep right 'number))
     (cond ((< left right) 'lt) ((= left right) 'eq) (t 'gt)))
    ((and (typep left 'list) (typep right 'list))
     (do* (res
           (lrest left (cdr lrest))
           (rrest right (cdr rrest))
           (l (car lrest) (car lrest))
           (r (car rrest) (car rrest)))
          ((or res ; order decided
               (not lrest) ; left ran out of items
               (not rrest)) ; right ran out of items
            (or res 
                (cond
                  ((and (null lrest) (null rrest)) 'eq)
                  ((null lrest) 'lt)
                  (t 'gt))))
          (let ((order (pair-order l r)))
            (if (not (eql order 'eq))
              (setf res order)))))
    ((typep left 'number) (pair-order (list left) right))
    ((typep right 'number) (pair-order left (list right)))))

(defun right-order-indices (pairs)
  (let ((sum 0)
        (vpairs (coerce pairs 'vector)))
    (dotimes (i (length vpairs))
      (let* ((pair  (svref vpairs i))
             (left  (first pair))
             (right (second pair)))
        (if (eql (pair-order left right) 'lt)
          (incf sum (1+ i)))))
    sum))

; Part one
;; (format t "solution: ~A~%" (right-order-indices (parse-input)))

(defconstant first-key '((2)))
(defconstant second-key '((6)))

(defun decoder-key (pairs)
  (let* ((items (append (list first-key second-key)
                       (reduce #'append pairs)))
         (sorted-items (sort items
                             #'(lambda (a b) (eql (pair-order a b) 'lt))))
        first-index second-index (i 1))
    (dolist (item sorted-items)
      (if (equal item first-key)
        (setf first-index i))
      (when (equal item second-key)
        (setf second-index i)
        (return))
      (incf i))
    (* first-index second-index)))

; Part two
(format t "solution: ~A~%" (decoder-key (parse-input)))
