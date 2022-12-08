(defun all-different (v)
  (dotimes (i (length v))
    (if (find (svref v i) (subseq v (1+ i)))
      (return-from all-different nil)))
  t)

(defun detect-loop (size)
  (let ((ring-buf (make-array size)))
    (do ((c (read-char t nil 'eof) (read-char t nil 'eof))
         (i 0 (1+ i)))
        ((or (eql c 'eof)
             (and (>= i 4) (all-different ring-buf)))
         (if (not (eql c 'eof)) i))
        (setf (svref ring-buf (mod i size)) c))))

; Part one
;; (format t "solution: ~A~%" (detect-loop 4))

; Part two
(format t "solution: ~A~%" (detect-loop 14))
