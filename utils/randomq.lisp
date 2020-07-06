(make-module :randomq (:utils :btree))
(in-package randomq)

;;;; RANDOM QUEUE
;; - adding objects to the queue is idempotent
;; - objects are retrieved from the queue uniformly randomly

(defclassx random-queue ()
  ((contents :initform (make-hash-table))
   (tree :initform (make-instance 'btree))))

(defmethodx pushv ((q random-queue) v) 
  (when (not (contains q v))
    (with-slots (contents tree) q
      (let ((index (random 1.0)))
	(setf (gethash v contents) t)
	(tree-add tree index v)))))

(defmethodx popv ((q random-queue))
  (when (not (emptyp q))
    (with-slots (contents tree) q
      (multiple-value-bind (value index) (tree-getnext tree (random 1.0))
	(when (not value)
	  (multiple-value-bind (v i) (tree-least tree)
	    (setf index i) (setf value v)))
	(remhash value contents)
	(tree-remove tree index)
	value))))

(defmethodx contains ((q random-queue) v)
  (with-slots (contents) q
    (gethash v contents)))

(defmethodx emptyp ((q random-queue))
  (with-slots (tree) q 
    (emptyp tree)))

(defmethod print-object ((q random-queue) stream)
  (with-slots (tree) q
    (print-object tree stream)))


;;;; TESTING CODE
#|
(setf x (make-instance 'random-queue))
(setf alpha '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(dotimes (n 10000)
  (if (< (random 1.0) 0.5) (pushv x (nth (random 26) alpha))
    (popv x)))
|#