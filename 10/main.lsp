(defun parse-input (&optional (str t))
  (let ((ops))
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof) (nreverse ops))
        (multiple-value-bind (op start) (read-from-string line)
          (case op
            (noop (push nil ops))
            (addx (push (parse-integer line :start start) ops)))))))


(defun sum-strength-at-cycles (ops cycles)
  (let ((cycle 0) (sum 0) (X 1) (notify-cycle (pop cycles)))
    (dolist (n ops)
      (incf cycle (if n 2 1)) ; simulate Y cycles
      (when (>= cycle notify-cycle) ; if we reached one of the cycles required
        (incf sum (* notify-cycle X)) ; increment the sum
        (setf notify-cycle (pop cycles))) ; get the next required cycle
      (if n (incf X n)) ; increment X after (this would be the end of cycle)
      (if (not notify-cycle) (return)))
    sum))

; Part one
(format t "solution: ~A~%" (sum-strength-at-cycles (parse-input) '(20 60 100 140 180 220)))

(defparameter crt-width 40)
(defparameter crt-height 6)
(defparameter crt-pixels (* crt-width crt-height))

(defun draw-crt (ops)
  (let ((X 1) (cycles 0) (n (pop ops)))
    (dotimes (i crt-pixels)
      (if (and (zerop (mod i crt-width)) (not (zerop i)))
        (princ #\lf))
      (if (<= (abs (- (mod i crt-width) X)) 1)
        (princ #\#)
        (princ #\.))
      (if n (incf cycles) (setf n (pop ops)))
      (when (= cycles 2)
        (incf X n)
        (setf n (pop ops))
        (setf cycles 0)))))
