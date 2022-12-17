(load "queue.lsp")

(defstruct monkey items op divider if-true if-false (checks 0))

(defun parse-integer-list (str &optional (offset 0))
  (let ((ints (make-queue))
        (last-start offset))
    (do nil ((>= last-start (length str)) ints)
      (multiple-value-bind
        (n start) (parse-integer str :start last-start :junk-allowed t)
        (if n (enqueue n ints))
        (setf last-start (1+ start))))))

(defun parse-operation (str &optional (offset 0))
  (let (tokens (last-start offset))
    (do nil ((>= last-start (length str)) (setf tokens (nreverse tokens)))
      (multiple-value-bind
        (token start) (read-from-string str nil 'eof :start last-start)
        (push token tokens)
        (setf last-start start)))
    (let ((f (fourth tokens))
          (op2 (fifth tokens)))
      (case f
        (* (if (eql op2 'old)
             #'(lambda (x) (* x x))
             #'(lambda (x) (* x op2))))
        (+ (if (eql op2 'old)
             #'(lambda (x) (+ x x))
             #'(lambda (x) (+ x op2))))))))

(defconstant divider-offset 21)
(defconstant if-true-offset 29)
(defconstant if-false-offset 30)

(defun parse-input (&optional (str t))
  (let (monkeys
        next-params
        (last-monkey (make-monkey)))
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
        ((eql line 'eof)
         (push last-monkey monkeys)
         (nreverse monkeys))
        (let ((colon-pos (position #\: line)))
          (multiple-value-bind
            (key start) (read-from-string line nil 'eof :end colon-pos)
            (case key
              (eof (push last-monkey monkeys)
                   (setf last-monkey (make-monkey)))
              (starting
                (let ((items (parse-integer-list line (+ colon-pos 2))))
                  (setf (monkey-items last-monkey) items)))
              (operation
                (let ((op (parse-operation line (+ colon-pos 2))))
                  (setf (monkey-op last-monkey) op)))
              (test
                (setf (monkey-divider last-monkey)
                      (parse-integer line :start divider-offset)))
              (if
                (if (eql 'true (read-from-string line nil 'eof
                                      :start start
                                      :end colon-pos))
                  (setf (monkey-if-true last-monkey)
                        (parse-integer line :start if-true-offset))
                  (setf (monkey-if-false last-monkey)
                        (parse-integer line :start if-false-offset))))))))))

(defun monkey-business (monkeys &key (rounds 20) relief)
  (let ((common-divider (reduce #'(lambda (acc m) ; optimally, this should be gcd, but since the input has all primes ٩(◕ ‿◕｡)۶
                                    (* acc (monkey-divider m)))
                                monkeys
                                :initial-value 1))
        (svmonkeys (coerce monkeys 'vector)))
    (dotimes (i rounds)
      (dotimes (j (length svmonkeys))
        (let* ((cur-monkey (svref svmonkeys j))
               (items (monkey-items cur-monkey))
               (op (monkey-op cur-monkey))
               (divider (monkey-divider cur-monkey))
               (if-true (monkey-if-true cur-monkey))
               (if-false (monkey-if-false cur-monkey)))
          (do () ((is-queue-empty? items))
            (let* ((item (dequeue items))
                   (worry-level (funcall relief (funcall op item)))
                   (k (if (zerop (mod worry-level divider)) if-true if-false))
                   (next-monkey (svref svmonkeys k)))
              (incf (monkey-checks cur-monkey))
              (let ((next-worry (mod worry-level common-divider)))
                (enqueue
                  ;; worry-level ; part one
                  next-worry  ; part two
                  (monkey-items next-monkey))))))))
    (let ((times-checked (map 'vector #'(lambda (m) (monkey-checks m)) svmonkeys)))
      (sort times-checked #'>)
      (* (svref times-checked 0)
         (svref times-checked 1)))))
; Part one
;; (format t "solution: ~A~%" (monkey-business
;;                              (parse-input)
;;                              :relief #'(lambda (level) (floor (/ level 3)))))

; Part two
; Usar modulo para reducir los productos
(format t "solution: ~A~%" (monkey-business
                             (parse-input)
                             :rounds 10000
                             :relief #'identity))
