;;;;;; STROKES ;;;;;;
;; A stroke is a set of intervals that have been collected together into
;; a single "visual object", possibly through the use of coercion

;; Possible stroke types: small, medium, large, rise, fall, horizontal

(defun stroke-type-from-angle (intervals)
  (let ((dx (1- (length intervals)))
	(dy (- (interval-middle (first intervals))
	       (interval-middle (car (last intervals))))))
    (let ((angle (atan dy dx)))
      (cond ((or (> angle (* 3/8 pi)) (< angle (* -3/8 pi))) ; vertical
	     (qualitative-height 
	      (apply #'min (mapcar #'interval-min intervals))
	      (apply #'max (mapcar #'interval-max intervals))))
	    ((> angle (* 1/8 pi)) 'rise)
	    ((< angle (* -1/8 pi)) 'fall)
	    (T 'horizontal)))))

(defun calculate-stroke-type (intervals)
  (cond ((find 'large intervals :key #'interval-type) 'large)
	((find 'medium intervals :key #'interval-type) 'medium)
	((<= (length intervals) (* n-width 1/5)) 'small)
	(t (stroke-type-from-angle intervals))))
  
;; constructor and accessors
(defvar last-stroke-uid 0)
(defun make-stroke (intervals &optional (uid (incf last-stroke-uid)))
  (append (list (calculate-stroke-type intervals) nil uid) intervals))
(defun stroke-type (stroke) (first stroke))
(defun stroke-uid (stroke) (third stroke))
(defun stroke-coerced? (stroke) (second stroke))
(defun stroke-intervals (stroke) (cdddr stroke))
(defun stroke-eq (stroke1 stroke2)	; is identity equal?
  (equal (stroke-uid stroke1) (stroke-uid stroke2)))
(defun stroke-equal (stroke1 stroke2)	; are contents equal?
  (and (eq (stroke-type stroke1) (stroke-type stroke2))
       (eq (stroke-coerced? stroke1) (stroke-coerced? stroke2))
       (eqv (stroke-intervals stroke1) (stroke-intervals stroke2))))
;; measurements
(defun stroke-min (stroke) 
  (apply #'min (mapcar #'interval-min (stroke-intervals stroke))))
(defun stroke-max (stroke)
  (apply #'max (mapcar #'interval-max (stroke-intervals stroke))))
(defun most-recent-interval-> (stroke1 stroke2)
  (let ((i1 (first (stroke-intervals stroke1)))
	(i2 (first (stroke-intervals stroke2))))
    (assert (and (eq (> (interval-min i1) (interval-min i2))
		     (> (interval-max i1) (interval-max i2)))
		 (eq (> (interval-max i1) (interval-min i2))
		     (> (interval-min i1) (interval-max i2)))))
    (> (interval-min i1) (interval-min i2))))
;; a stroke can be coerced into an adjacent category
(defparameter stroke-coercion-table
    '((large medium) (medium small large horizontal rise fall) 
      (small medium rise fall horizontal)
      (rise small horizontal) (horizontal small rise fall) 
      (fall small horizontal)))
(defun coerce-stroke (stroke type)
  (let ((real (calculate-stroke-type (stroke-intervals stroke))))
    (if (find type (assoc real stroke-coercion-table))
	(append (list type T (stroke-uid stroke)) (stroke-intervals stroke))
      (error 'coercion-failure :type type :obj stroke))))
(defun ensure-stroke-type (stroke type)
  (if (eq (stroke-type stroke) type) stroke (coerce-stroke stroke type)))
(defun ensure-stroke-angled (stroke)
  (assert (not (find (stroke-type stroke) '(medium large))))
  (if (eq (stroke-type stroke) 'small)
      (coerce-stroke stroke (stroke-type-from-angle (stroke-intervals stroke)))
    stroke))
;; adding new intervals to a stroke may change its type
(defun add-to-stroke (stroke interval)
  (let ((new (make-stroke (cons interval (stroke-intervals stroke))
			  (stroke-uid stroke))))
    (if (stroke-coerced? stroke) 
	(coerce-stroke new (stroke-type stroke))
      new)))

(defregression test-stroke
  (with-temps ((fullheight 7) (n-width 5))
    (let ((st (make-stroke (list (make-interval 1 1) (make-interval 2 2)))))
      (assert (eq (stroke-type st) 'fall))
      (assert (not (stroke-coerced? st)))
      (assert (stroke-coerced? (coerce-stroke st 'horizontal)))
      (assert (eq (stroke-type (coerce-stroke st 'horizontal)) 'horizontal))
      (assert (eq (stroke-type (add-to-stroke st (make-interval 0 8))) 'large))
      (assert (stroke-eq st (add-to-stroke st (make-interval 2 3))))
      'ok)))
