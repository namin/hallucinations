(make-module :observer (:utils))
(in-package :observer)

;;; OBSERVER/OBSERVABLE mechanism 
;; add-observer attaches observers to particular objects
;; observe-method wraps an around-method on a generic method, such
;;   that observers of one of its parameters are tickled after the call
;; Note that only one parameter can be observed for a given method

(defclassx observer () ())

(public get-observers)
(defclassx observable () 
  ((observers :initform '() :accessor get-observers)))

(defmethodx add-observer ((x observable) (obs observer))
  (pushnew obs (get-observers x))
  (notify obs x 'observer::init))	; immediately poke the observer for first update

(defmethodx remove-observers ((x observable))
  (setf (get-observers x) nil))
(defmethodx remove-observer ((x observable) (obs observer))
  (setf (get-observers x) (delete obs (get-observers x))))


(defmethod extract-variables (sll)
  (labels ((name (x) (if (consp x) (car x) x)))
    (let ((l (delete "&REST" (mapcar #'name sll) :key #'symbol-name :test #'equal)))
      (if (eql (length l) (length sll))
	  (append l '(()))
	l))))

;; must contain all required and optional parameters, and a &rest if appropriate
;; may not contain &key or &aux
(defmacrox observe-method (name target variables)
  (let ((callvbls (extract-variables variables))
	(v2 (gensym)))
    `(defmethod ,name :around ,variables
       (let ((,v2 (apply #'call-next-method ,@callvbls))) ; do the update
	 (when (typep ,target 'observable)
	   (mapcar (ppf notify ?v ,target ',name ,@(remove nil callvbls)) 
		   (get-observers ,target))) ; notify observers
	 ,v2))))


(defmethodx notify ((obs observer) (x observable) method &rest parameters)
  (declare (ignore method parameters))
  (format t "default notify triggered"))