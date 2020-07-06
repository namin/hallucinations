(make-module :btree (:utils))
(in-package btree)

;;; EXTERNAL INTERFACE
(defclassx btree ()
  ((root :initform nil :accessor root)))

(defmethodx tree-remove ((x btree) k)
  (itree-remove (root x) k (place (root x))))
(defmethodx tree-add ((x btree) k v)
  (itree-add (root x) k v (place (root x))))
(defmethodx tree-getnext ((x btree) k)
  (itree-getnext (root x) k nil))
(defmethodx tree-least ((x btree))
  (if (root x) 
      (itree-least (root x))
    nil))
(defmethodx emptyp ((x btree))
  (null (root x)))

(defmethod print-object ((x btree) stream)
  (print-object (root x) stream))

;;; INTERNALS
(defclass tree-elt ()
  ((value :initarg :v :accessor value)
   (object :initarg :o :accessor object)
   (left :initform nil :accessor left)
   (right :initform nil :accessor right)))

(defmethod print-object ((x tree-elt) stream)
  (format stream "(")
  (when (left x) (print-object (left x) stream) (format stream " "))
  (format stream "~A,~A" (value x) (object x))
  (when (right x) (format stream " ") (print-object (right x) stream))
  (format stream ")"))
		
(defun join (lower upper)
  (cond ((and lower upper)
	 (if (= 0 (random 2))
	     (let ((new (join lower (left upper)))) ; choose upper as head
	       (setf (left upper) new)
	       upper)
	   (let ((new (join (right lower) upper))) ; choose lower as head
	     (setf (right lower) new)
	     lower)))
	(lower lower)			; if only one, return it
	(upper upper) 
	(t nil)))			; if neither, return nil

(defmethod itree-remove (x k parlink)
  (cond ((null x) 'done)		; if unfound, just return
	((= k (value x))		; if found, remove
	 (setf (placeof parlink) (join (left x) (right x))))
	((< k (value x))		; if less, search left
	 (itree-remove (left x) k (place (left x))))
	(t				; otherwise, search right
	 (itree-remove (right x) k (place (right x))))))

(defmethod itree-add (x k v parlink)
  (cond ((null x)			; at leaf, add
	 (setf (placeof parlink) 
	   (make-instance 'tree-elt :v k :o v)))
	((= k (value x))
	 (setf (object x) v))
	((< k (value x))
	 (itree-add (left x) k v (place (left x))))
	(t
	 (itree-add (right x) k v (place (right x))))))

(defmethod itree-getnext (x k >val)
  (cond ((null x)			; if at leaf, give best value
	 (values (car >val) (cdr >val)))
	((= k (value x))		; if equal, return it
	 (values (object x) k))
	((< k (value x))		; if less, go left
	 (itree-getnext (left x) k (cons (object x) (value x))))
	(t				; else go right
	 (itree-getnext (right x) k >val))))

(defmethod itree-least (x)
  (if (left x) 
      (itree-least (left x)) 
    (values (object x) (value x))))

;;;; TESTING CODE
#|
(setf x (make-instance 'btree))
(setf alpha '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(dotimes (n 10000)
  (if (< (random 1.0) 0.5) (tree-add x (random 50) (nth (random 26) alpha))
    (tree-remove x (random 50))))
|#