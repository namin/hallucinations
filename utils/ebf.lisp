(make-module exponential-backoff (utils)
	     (:nicknames "EBF"))
(in-package :ebf)

(defunx make-ebf (&optional v &key (alpha 0.1)) (cons v alpha))

(defunx value (ebf) (car ebf))
(defunx (setf value) (v ebf) (setf (car ebf) v))
(defunx alpha (ebf) (cdr ebf))
(defunx (setf alpha) (a ebf) (setf (cdr ebf) a))

(defun update (ebf v)
  (let ((a (alpha ebf)))
    (setf (value ebf)
      (+ (* a v)
	 (* (- 1 a) (value ebf))))))
