;;;; -*- Mode: LISP; Package: PUZZLE; Syntax: Common-lisp; -*-

;;;; Rubik's cube solving in Common Lisp
;;;; Copyright (C) 2003 Kenneth P. Turvey <kt@squeakydolphin.com> 
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;; 
;;;; 
;;;; 9/28/2004: added modulization & emptyp -JSB

;;;; Implements a priority queue.

(make-module priority-queue (:utils))
(in-package :priority-queue)

(defclass priority-queue ()
  ((queue-data :documentation
  "Contains an array implementation of heap for use as a priority queue.")
   (growth :initarg :growth
	   :initform 1000
	   :documentation
	   "The number of elements to add to the heap when it needs to
be increased in size.")
   (priority :initarg :priority
	     :documentation
  "The function used to determine the priority of a given element on
the queue.  This function defaults to the identity function."))
   (:documentation "Implements a priority queue."))


;; Constructor

(defunx make-priority-queue (&key (priority #'(lambda (x) x)) (size 100)
			 (growth 1000) (element-type t))
  "Returns a new priority queue.  The key priority is a function that
takes one argument of the type that will be stored in the priority queue
and returns its priority as a number.  The size key indicates an estimate
of the size of the priority queue.  The growth is a number indicating
the number of elements by which to grow the heap when it is running out
of space.  The element type is the kind of elements that will be stored
in the priority queue."
  (check-type growth (integer 1 *))
  (let ((tmp-pq (make-instance 'priority-queue :priority priority
			       :growth growth)))
    (setf (slot-value tmp-pq 'queue-data)
	  (make-array size :element-type element-type :adjustable t
		      :fill-pointer 0))
    tmp-pq))

;; Generic functions
  
(defgeneric heapify (priority-queue index)
  (:documentation "This generic function forces the sub-tree rooted at index
into a heap.  It returns t if any swaps were performed and nil if not."))

;; Methods

(defmethod heapify ((q priority-queue) index)
  (let* ((left-index (+ (* 2 index) 1))
	 (right-index (+ 1 left-index)))
    (cond
     ;; Two children
     ((< right-index (fill-pointer (slot-value q 'queue-data)))
      (let ((left-priority
	     (funcall (slot-value q 'priority)
		      (aref (slot-value q 'queue-data) left-index)))
	    (right-priority
	     (funcall (slot-value q 'priority)
		      (aref (slot-value q 'queue-data) right-index)))
	    (index-priority
	     (funcall (slot-value q 'priority)
		      (aref (slot-value q 'queue-data) index))))
	(cond ((> left-priority right-priority)
	       (when (> left-priority index-priority)
		 (rotatef
		  (aref (slot-value q 'queue-data) index)
		  (aref (slot-value q 'queue-data) left-index))
		 (heapify q left-index)
		 t))
	      ((>= right-priority left-priority)
	       (when (> right-priority index-priority)
		 (rotatef
		  (aref (slot-value q 'queue-data) index)
		  (aref (slot-value q 'queue-data) right-index))
		 (heapify q right-index)
		 t))
	      (t nil))))
     ;; One child
     ((< left-index (fill-pointer (slot-value q 'queue-data)))
      (cond ((> (funcall (slot-value q 'priority)
			 (aref (slot-value q 'queue-data) left-index))
		(funcall (slot-value q 'priority)
			 (aref (slot-value q 'queue-data) index)))
	     (rotatef
	      (aref (slot-value q 'queue-data) index)
	      (aref (slot-value q 'queue-data) left-index))
	     ;; No heapify is necessary since index cannot have any
	     ;; grandchildren. 
	     t) 
	    (t nil)))
     ;; No children
     (t nil))))
     
(defmethodx enqueue ((q priority-queue) value)
  (vector-push-extend 
   value
   (slot-value q 'queue-data) (slot-value q 'growth))
  (fix-priority-queue
   q (floor (/ (- (fill-pointer (slot-value q 'queue-data)) 2) 2)))
  (values value (fill-pointer (slot-value q 'queue-data))))

(defmethodx dequeue ((q priority-queue))
  (cond ((= (fill-pointer (slot-value q 'queue-data)) 0) (values nil nil))
	(t (let ((tmp (aref (slot-value q 'queue-data) 0)))
	     (setf (aref (slot-value q 'queue-data) 0)
		   (aref (slot-value q 'queue-data)
			 (setf (fill-pointer
				(slot-value q 'queue-data))
			       (1- (fill-pointer
				    (slot-value q 'queue-data))))))
	     (when (> (fill-pointer (slot-value q 'queue-data)) 1)
	       (heapify q 0))
	     (values tmp t)))))

(defmethodx emptyp ((q priority-queue))
  "Returns t if the queue is empty, nil if there are elements"
  (= (fill-pointer (slot-value q 'queue-data)) 0))

(defgeneric fix-priority-queue (priority-queue index)
  (:documentation "This generic function is much like heapify, but it avoids
performing any extra work.  It assumes that the element with the given index
is the only one that is not in its correct position in the tree, and works
to correct this.")) 

(defmethod fix-priority-queue ((q priority-queue) index)
  (when (>= index 0)
					; We are not at the root of the tree. 
    (when (> (fill-pointer (slot-value q 'queue-data)) (+ (* index 2) 1))
					; This node has at least one child
      (cond ((> (fill-pointer (slot-value q 'queue-data)) (+ (* index 2) 2))
					; This node has two children
	     (cond ((>= (funcall (slot-value q 'priority)
				 (aref (slot-value q 'queue-data)
				       (+ (* index 2) 1)))
			(funcall (slot-value q 'priority)
				 (aref (slot-value q 'queue-data)
				       (+ (* index 2) 2))))
					; The left child is larger (or equal)
		    (when (> (funcall (slot-value q 'priority)
				      (aref (slot-value q 'queue-data)
					    (+ (* index 2) 1)))
			     (funcall (slot-value q 'priority)
				      (aref (slot-value q 'queue-data)
					    index)))
					; A swap is necessary.
		      (rotatef (aref (slot-value q 'queue-data)
				     (+ (* index 2) 1))
			       (aref (slot-value q 'queue-data) index))
		      (fix-priority-queue q (floor (/ (- index 1) 2)))))
		     (t
					; The right child must be greater
		      (when (> (funcall (slot-value q 'priority)
					(aref (slot-value q 'queue-data)
					      (+ (* index 2) 2)))
			       (funcall (slot-value q 'priority)
					(aref (slot-value q 'queue-data)
					      index)))
					; A swap is necessary
			(rotatef (aref (slot-value q 'queue-data)
				       (+ (* index 2) 2))
				 (aref (slot-value q 'queue-data) index))
			(fix-priority-queue q (floor (/ (- index 1) 2)))))))
					; The two child case has been handled
					; this node must only have one child
	    (t (when (> (funcall (slot-value q 'priority)
				 (aref (slot-value q 'queue-data)
				       (+ (* index 2) 1)))
			(funcall (slot-value q 'priority)
				 (aref (slot-value q 'queue-data)
				       index)))
		 (rotatef (aref (slot-value q 'queue-data) (+ (* index 2) 1))
			  (aref (slot-value q 'queue-data) index))
		 (fix-priority-queue q (floor (/ (- index 1) 2)))))))))
  
