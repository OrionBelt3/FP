(defun remove-every-second (w)
  (cond ((null  w) nil)
        (t (cons (car w) (remove-every-second (cddr w))))))

