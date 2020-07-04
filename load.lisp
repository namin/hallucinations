(defun compile-and-load (file)
  (let ((in-file (concatenate 'string file ".lisp"))
	(out-file (concatenate 'string file ".fasl")))
    (compile-file in-file :OUTPUT-FILE out-file)
    (load out-file)))

(defun eqv (x y)
  (equalp x y))

(defmacro defregression (name &rest body)
  `(progn
     (defun ,name ()
       ,@body)
     ;;(,name)
     ))

(defmacro with-set (bindings &rest body)
  `(let ,bindings
     ,@body))

(defmacro with-temps (bindings &rest body)
  `(let ,bindings
     ,@body))

(compile-and-load "pgm2stream")
(compile-and-load "shiftregister")
(load "vision.lisp")
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
