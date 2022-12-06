(defun char-to-digit (c)
  (- (char-code c) (char-code #\0)))

(defun parse-stack-lines (lines)
  (let ((stack-indices)
        (stacks)
        (first-line? t))
    (dolist (line lines)
      (if first-line?
        (progn ; parse line with stack ids (and count)
          (do* ((parsed '(1 2) (multiple-value-list
                                (parse-integer line :start start :junk-allowed t)))
                (id (car parsed) (car parsed))
                (start (cadr parsed) (cadr parsed)))
            ((null id)) ; id is null if we reaced the end
            (push (1- start) stack-indices)) ; assume center of each stack is aligned with lsd of the stack id ( [1] [8] 1[0] 21[0] )
          (setf stack-indices (apply #'vector stack-indices)
                stacks (make-array (length stack-indices) :initial-element nil)
                first-line? nil))
        (dotimes (i (length stacks))
          (let ((item (char line (svref stack-indices i))))
            (if (char/= #\  item) ; stack i has an element
              (push item (svref stacks i)))))))
    (reverse stacks)))

(defun parse-input ()
  (let ((parsing-stack? t)
        (lines)
        (instructions))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
      ((eql line 'eof)
       (values (parse-stack-lines lines) (reverse instructions)))
      (if parsing-stack? ; first we parse the stack
        (if (zerop (length line)) ; reached the end of the stack
          (setf parsing-stack? nil) ; next loop, start parsing instructions
          (push line lines)) ; save lines to parse later
        (let ((start 0) inst)
          (dotimes (i 6) ; instructions have 6 words
            (multiple-value-bind
              (value newstart)
              (read-from-string line nil nil :start start)
              (setf start newstart)
              (if (oddp i)
                (push value inst))))
          (push inst instructions))))))

(defun cm9000 (stacks moves from to)
  (dotimes (i moves)
      (push (pop (svref stacks (1- from))) (svref stacks (1- to)))))

(defun execute (inst stacks crane)
  (destructuring-bind (to from moves) inst
    (funcall crane stacks moves from to)))

(defun vm (stacks instructions crane)
  (dolist (inst instructions)
    (execute inst stacks crane))
  stacks)

(defun get-solution (crane)
  (let* ((stacks (multiple-value-bind (stacks instructions) (parse-input)
                   (vm stacks instructions crane)))
         (len (length stacks))
         (output (make-string len)))
    (dotimes (i len)
      (setf (char output i) (car (svref stacks i))))
    (format t "solution: ~A~%" output)))

; Part one
;; (get-solution #'cm9000)

(defun cm9001 (stacks moves from to)
  (let* ((stack-from (svref stacks (1- from)))
         (stack-to   (svref stacks (1- to)))
         (items-top  (subseq stack-from 0 moves))
         (items-left (subseq stack-from moves)))
    (setf (svref stacks (1- to))   (append items-top stack-to)
          (svref stacks (1- from)) items-left)))

; Part two
(get-solution #'cm9001)
