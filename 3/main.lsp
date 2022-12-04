; Part one
(defconstant lowercase-start 97)

(defconstant uppercase-start 65)

(defun item-priority (c)
  (let ((code (char-code c)))
    (if (>= code lowercase-start)
      (+ 1 (- code lowercase-start))     ; char is lowercase
      (+ 27 (- code uppercase-start))))) ; char is uppercase

(defun item-priorities-sum ()
  (let ((sum 0))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof))
         (bag (make-hash-table) (clrhash bag)))
        ((eql line 'eof) (format t "solution: ~A~%" sum))
        (let* ((len (length line))
               (half (/ len 2)))
          (dotimes (i len)
            (let ((c (char line i)))
              (if (< i half)
                (setf (gethash c bag) T) ; (caching present keys)
                (if (gethash c bag)
                  (progn
                   (incf sum (item-priority c))
                   (return)))))))))) ; repeatted key found, exit dotimes loop

; Part two
(defun group-priorities-sum ()
  (let ((sum 0))
    (do* ((line (read-line t nil 'eof) (read-line t nil 'eof))
          (lineno 0 (1+ lineno))
          (bag (make-hash-table)
               (if (zerop (mod lineno 3)) ; if it's a new group
                   (clrhash bag) ; empty bag
                   bag)))        ; else, keep the last one
        ((eql line 'eof) (format t "solution: ~A~%" sum))
        (let ((len (length line))
              (id (mod lineno 3))) ; elf id in it's group
          (dotimes (i len)
            (let* ((c (char line i))
                   (is-defined (gethash c bag)))
              ; either it's not defined and we're the first (so we have to defined it)
              ; or is defined and the previous elf had it (because is-defined has our id)
              (if (or (and (not is-defined) (zerop id))
                      (and is-defined (= is-defined id)))
                (setf (gethash c bag) (1+ id)))))
          (if (= id 2) ; we're last of group
              (maphash
                #'(lambda (key value)
                    ; find the key that appears in all of the group's bags
                    (if (and value (= value 3))
                      (incf sum (item-priority key))))
                bag))))))

(group-priorities-sum)
