(defun compile-and-load (file)
  (let ((in-file (concatenate 'string file ".lisp"))
	(out-file (concatenate 'string file ".fasl")))
    (compile-file in-file :OUTPUT-FILE out-file)
    (load out-file)))

(setq ? '?) ;; TODO

(defun ppf (f &rest args)
  (lambda (p) (apply f (cons p (remove-if (lambda (x) (eq x '?)) args)))))

(defun fold (func list init-val)
  (reduce func list :initial-value init-val))

(defun mapset (func set)
  (if (null (cdr set))
      (car set)
    (funcall func (car set) (cdr set)))) ;; TODO

(defun eqv (x y)
  (equalp x y))

(defmacro defregression (name &rest body)
  `(progn
     (defun ,name ()
       ,@body)
     (,name)
     ))

(defmacro with-set (bindings &rest body)
  `(let ,bindings
     ,@body))

(defmacro with-temps (bindings &rest body)
  `(let ,bindings
     ,@body))

(compile-and-load "pgm2stream")
(compile-and-load "shiftregister")
(compile-and-load "vision")
(compile-and-load "representation")
(compile-and-load "finger")

(defun process-img-sequence (sr stem number &optional print-period)
  (let ((ln 0))
    (dotimes (i number)
      (let ((img (pgm-to-row-stream (format nil "~A~A.pgm" stem i))))
	(tick)
	(while (more-rows img)
	  (update-shift-register sr (bits-to-intervals (next-row img)))
	  (incf ln)
	  (when (and print-period (zerop (mod ln print-period)))
	    (format t "Line ~A:~%" ln) (pprint-representation sr)))))))
