(make-module shadowvariable (:utils)
	     (:nicknames "SHADOWVBL"))
(in-package shadowvbl)

;;;; SHADOW-VARIABLE
;;;; a shadow-variable remembers its previous value as well as the current one

(defconstant unbound (gensym "UNBOUND"))

(defclassx shadow-variable ()
  ((old :initarg :old :initform unbound)
   (new :initarg :new :initform nil)
   (changed :initform nil)))

(defmethodx changed ((x shadow-variable))
  (with-slots (changed) x changed))

(defmethodx pushv ((x shadow-variable) v)
  "Push changes the new value, but not the old value"
  (with-slots (new old changed) x 
    (setf new v)
    (when (eq unbound old) (setf old v))
    (setf changed (not (eqv new old)))))

(defmethodx pullv ((x shadow-variable))
  "Pull returns the current value and the old value and sets the old to be the new"
  (with-slots (new old changed) x
    (let ((save old))
      (setf old (copy new))
      (setf changed nil)
      (values new save))))

(defmethodx peekv ((x shadow-variable))
  "Returns the new value without modifying the old value"
  (with-slots (new) x new))

(defmethod print-object ((v shadow-variable) stream)
  (format stream "~A ~2I~:_[~A]" (slot-value v 'new) (slot-value v 'old)))

