(defstruct dir
  parent (entries (make-hash-table)))

(defun parse-input ()
  (let* ((root (make-dir :parent nil))
         (current nil)
         (parsing 'command))
    (do ((line (read-line t nil 'eof) (read-line t nil 'eof)))
        ((eql line 'eof) root)
        (if (char= (char line 0) #\$) ; line starts with $
          (setf parsing 'command))
        (if (eql parsing 'command)
          (let ((command (subseq line 2 4))) ; "$ (cd|ls)"
            (cond
              ((string= command "cd")
               (let ((name (subseq line 5)))
                 (cond
                   ((string= name "/")
                      (setf current root))
                   ((string= name "..")
                      (setf current (dir-parent current)))
                   (t (setf current (gethash (read-from-string name) (dir-entries current)))))))
              ((string= command "ls") (setf parsing 'entries))))
          (progn
              (multiple-value-bind (size start) (parse-integer line :junk-allowed t)
                (let ((name (subseq line
                                    (if size (1+ start) 4)))) ; read name after size or "dir"
                  (setf (gethash (read-from-string name) (dir-entries current))
                        (or size (make-dir :parent current))))))))))

(defun mapdirs (f root &optional (path '(/)))
  (funcall f root path)
  (maphash #'(lambda (name value)
               (if (typep value 'dir)
                 (mapdirs f value (cons name path))))
           (dir-entries root)))

(defun mapfiles (f root)
  (mapdirs
    #'(lambda (dir path)
        (maphash
          #'(lambda (name value)
              (if (typep value 'number)
                (funcall f value (cons name path))))
          (dir-entries dir)))
    root))

(defun dir-size (root)
  (let ((size 0))
    (mapfiles
      #'(lambda (fsize path) (incf size fsize))
      root)
    size))

(defconstant max-dir-size 100000)

(defun sum-of-dirsize-under (root size)
  (let ((sum 0))
    (mapdirs
      #'(lambda (dir path)
          (let ((dsize (dir-size dir)))
            (if (<= dsize size)
              (incf sum dsize))))
      (parse-input))
    sum))

; Part one
;; (format t "solution: ~A~%" (sum-of-dirsize-under (parse-input) max-dir-size))

(defconstant available-space 70000000)
(defconstant needed-space 30000000)

; Part two
(let* ((root (parse-input))
       (root-size (dir-size root))
       (unused-space (- available-space root-size))
       (min-dir-size root-size))
  (mapdirs
    #'(lambda (dir path)
        (let ((dsize (dir-size dir)))
          (if (>= (+ unused-space dsize) needed-space)
            (setf min-dir-size (min min-dir-size dsize)))))
    root)
  (format t "solution: ~A~%" min-dir-size))
