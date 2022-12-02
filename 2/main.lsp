; Part one
(defun wins? (a b)
  (or (and (eql a 'rock) (eql b 'scissors))
      (and (eql a 'paper) (eql b 'rock))
      (and (eql a 'scissors) (eql b 'paper))))

(defun match-rps (id)
  (cond ((or (eql id 'a) (eql id 'x)) 'rock)
        ((or (eql id 'b) (eql id 'y)) 'paper)
        ((or (eql id 'c) (eql id 'z)) 'scissors)))

(defun outcome-points (theirs ours)
  (let ((rps-theirs (match-rps theirs))
        (rps-ours   (match-rps ours)))
    (cond ((wins? rps-ours rps-theirs) 6) ; win
          ((eql rps-ours rps-theirs) 3)   ; draw
          (t 0))))

(defun play-points (ours)
  (let ((rps-ours (match-rps ours)))
    (cond ((eql rps-ours 'rock) 1)
          ((eql rps-ours 'paper) 2)
          (t 3))))

(defun elf-rps ()
  (let ((point-sum 0))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
        ((eql line 'eof) (format t "solution: ~A~%" point-sum))
      (multiple-value-bind (elf-play idx) (read-from-string line)
        (let* ((our-play (read-from-string line nil 'eof :start idx))
               (points (play-points our-play)))
          (setf point-sum (+ point-sum
                             points
                             (outcome-points elf-play our-play))))))))

; Part two
(defun play-for-result (them result)
  (cond ((eql result 'x) ; lose
         (cond ((eql them 'a) 'c)
               ((eql them 'b) 'a)
               ((eql them 'c) 'b)))
        ((eql result 'y) them) ; draw
        (t (cond ((eql them 'a) 'b) ; win
                 ((eql them 'b) 'c)
                 ((eql them 'c) 'a)))))

(defun elf-rps-follow ()
  (let ((point-sum 0))
    (do ((line (read-line *standard-input* nil 'eof) (read-line t nil 'eof)))
        ((eql line 'eof) (format t "solution: ~A~%" point-sum))
      (multiple-value-bind (elf-play idx) (read-from-string line)
        (let* ((result (read-from-string line nil 'eof :start idx))
               (our-play (play-for-result elf-play result))
               (points (play-points our-play)))
          (setf point-sum (+ point-sum
                             points
                             (outcome-points elf-play our-play))))))))

(elf-rps-follow)
