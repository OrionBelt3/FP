
(defun copy-array (arr)
  (let* ((dimensions (array-dimensions arr)) (new-arr (make-array dimensions)))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref new-arr i)
            (row-major-aref arr i)))
    new-arr))

(defun fun (mat)
  (let ((size (array-dimensions mat)))
    (let ((lines (first size)) (columns (second size)) (ans (copy-array mat)))
      (do ((i (- lines 1) (- i 1))) ((< i 0) ans)
        (do ((j 0 (+ j 1))) ((>= j columns) 'done_str)
          (if (and (> j 0) (> (aref ans i (- j 1)) (aref ans i j)))
            (setf (aref ans i j) (aref ans i (- j 1))))
          (if (and (< i (- lines 1)) (> (aref ans (+ i 1) j) (aref ans i j)))
            (setf (aref ans i j) (aref ans (+ i 1) j))
            ))))))

(print (fun (make-array '(3 3) :initial-contents '((0 0 0) (0 1 0) (0 0 0)))))

