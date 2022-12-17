(defun make-queue() (cons nil nil))

(defun is-queue-empty? (q) (null (car q)))

(defun queue-length (q) (length (car q)))

(defun enqueue (obj q)
  (if (is-queue-empty? q)
    (setf (cdr q) (setf (car q) (list obj)))
    (setf (cdr (cdr q)) (list obj)
          (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q) (pop (car q)))
