;;;;;; INTERVALS ;;;;;;
;; an interval is a triple: (type min max)
;; type is implied, but may be coerced to an adjacent type

;; constructor
(defun make-interval (min max)
  (list (qualitative-height min max) min max))
;; accessors
(defun interval-type (interval) (first interval))
(defun interval-min (interval) (second interval))
(defun interval-max (interval) (third interval))
;; measures
(defun qualitative-height (min max)
  (let ((h (1+ (- max min))))
    (cond ((>= h (* fullheight 3/4)) 'large)
	  ((<= h (* fullheight 1/4)) 'small)
	  (T 'medium))))
(defun interval-height (interval) 
  (1+ (- (interval-max interval) (interval-min interval))))
(defun interval-middle (interval) 
  (/ (+ (interval-max interval) (interval-min interval)) 2))
(defun interval-adjacent (a b)
  (and (not (> (interval-min a) (1+ (interval-max b))))
       (not (> (interval-min b) (1+ (interval-max a))))))

;; An interval can be coerced by one category
(defun coerce-interval (interval type)
  (let ((min (interval-min interval)) (max (interval-max interval)))
    (let ((real (qualitative-height min max)))
      (if (and (find type '(large medium small)) ; sane
	       (not (or (and (eq real 'small) (eq type 'large))
			(and (eq real 'large) (eq type 'small)))))
	  (list type min max)
	(error 'coercion-failure :type type :obj interval)))))
(defun ensure-interval-type (interval type)
  (if (eq (interval-type interval) type) 
      interval 
    (coerce-interval interval type)))

;; Turns an array of booleans into a list of intervals in rising order
(defun bits-to-intervals (slice)
  (labels ((from-i (i)
	     (let ((next-true (position 1 slice :start i)))
	       (if next-true
		   (let ((next-false (position 0 slice :start next-true)))
		     (if next-false
			 (cons (make-interval next-true (1- next-false))
			       (from-i next-false))
		       (list (make-interval next-true (1- (length slice))))))
		 nil))))
    (from-i 0)))


(defregression test-interval
  (with-temps ((fullheight 7) (n-width 5))
    (let ((i1 (make-interval 1 3)) (i2 (make-interval 4 10))
	  (i3 (make-interval 5 6)) (i4 (make-interval 2 2)))
      (assert (interval-adjacent i1 i2))
      (assert (interval-adjacent i2 i1))
      (assert (not (interval-adjacent i1 i3)))
      (assert (not (interval-adjacent i3 i1)))
      (assert (eq (interval-type i1) 'medium))
      (assert (eq (interval-type i2) 'large))
      (assert (eq (interval-type i4) 'small))
      (assert (eq (interval-type (coerce-interval i1 'large)) 'large))
      (assert (eqv (bits-to-intervals '(1 1 0 0 1 0 0 1 0))
		   '((medium 0 1) (small 4 4) (small 7 7))))
      'ok)))
