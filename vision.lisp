;; The code in this file takes a sequence of pixel rows and turns them into
;; a sequence of changes in lines, suitable for representing letters.

(define-condition coercion-failure (error)
  ((type :initarg :type) (object :initarg :obj)))
(defmethod print-object ((e coercion-failure) out)
  (with-slots (object type) e
    (format out "~A cannot be coerced to ~A" object type)))

;; There are ten features:
;; (R)ise: an ascending diagonal line
;; (F)all: a descending diagonal line
;; (H)orizontal: a horizontal line
;; (L)arge: a full-height vertical line
;; (M)edium: a half-height vertical line
;; (S)mall: a not non-vertical line less than half-height
;; no feature: (.) observed, (_) predicted
;; ( ) default
(defun alist-to-hash-table (alist)
  (let ((table (make-hash-table)))
    (dolist (elt alist) (setf (gethash (car elt) table) (cdr elt)))
    table))
(defparameter feature-characters
    (alist-to-hash-table 
     '((rise #\R #\r) (fall #\F #\f) (horizontal #\H #\h)
       (large #\L #\l) (medium #\M #\m) (small #\S #\s) 
       (nothing #\_ #\-) (blank #\. #\.))))
(defun feature-character (feature observed)
  (nth (if observed 0 1) (gethash feature feature-characters)))

;; This all needs to be not-very-sensitive to small perturbations in
;; height, width, or orientation.
;; To do this, we'll give a vertical scale "fullheight" and a horizontal
;; scale "n-width"
;; Large is 3/4 - 1 fullheight  x  0 - 1/5 n-width
;; Medium is >1/4 - <3/4 fullheight  x  0 - 1/5 n-width
;; Small is 0 - 1/4  fullheight  x  0 - 1/5 n-width
;; Rise, Fall, Horizontal are all 0 - 1/4 fullheight, >1/5 n-width
;;   (or lower width if a medium or small is involved)
;;
;; Example: in sample.pgm, fullheight = 7, n-width = 5
;;   large = 6-7, medium = 2-5, small = 1, R/F/H = 1 x 2+
;; Example: in sample2.pgm, fullheight = 20, n-width = 12
;;   large = 16-20, medium = 5-15, small = 1-4, R/F/H = 1-4 x 3+
;; Example: in austen.txt0.pgm, fullheight = 10, n-width = 6
;;   large = 8-10, medium = 3-7, small = 1-2, R/F/H = 1-2 x 2+
;;
;; Notice that there's an undefined region:
;; >1/4 fullheight and >1/5 n-width.
;; this will be interpreted as larges and mediums.
(defvar fullheight 0)
(defvar n-width 0)

(compile-and-load "interval")
(compile-and-load "stroke")
(compile-and-load "components")

(defun connected-to-stroke (stroke new-interval)
  (interval-adjacent (first (stroke-intervals stroke)) new-interval))

;; Return an event:
;; side indicates where the vertical is: left or right
;; if all lines attach at the same point, left is split & right is merge
;; if all lines attach at different points, it's left-attach or right-attach
;; otherwise it's a problem
(defun attachment-location (vertical line)
  (assert (find (stroke-type vertical) '(medium large)))
  (let ((bottom (stroke-min vertical)) (top (stroke-max vertical))
	(line-location (interval-middle (first (stroke-intervals line)))))
    ;;(if (eq (stroke-type vertical) 'medium) ;; 2 locations for mediums
    ;;	(if (> line-location (/ (+ bottom top) 2)) 'top 'bottom)
      ;; 3 locations for larges
      (cond ((> line-location (+ (* 3/4 top) (* 1/4 bottom))) 'top)
	    ((< line-location (+ (* 1/4 top) (* 3/4 bottom))) 'bottom)
	    (t 'middle))))
   ;; )

(defun contains-duplicate (list)
  (and list (or (find (car list) (cdr list)) (contains-duplicate (cdr list)))))
(defun homogeneous (list) (not (find (first list) list :test-not #'equal)))

(defun force-separation (locations)
  (when (>= verbosity 2) (format t "a hack used!~%"))
  (when (eq (first locations) 'middle) (setf (first locations) 'bottom))
  (when (eq (third locations) 'middle) (setf (third locations) 'top)))
(defun attach (vertical lines left?)
  (when (and (> (length lines) 2) (eq (stroke-type vertical) 'medium))
    (setf vertical (coerce-stroke vertical 'large)))
  (let ((locations (mapcar (pcf attachment-location vertical) lines)))
    (when (contains-duplicate locations) (force-separation locations))
    (assert (if (contains-duplicate locations) (homogeneous locations) T))
    (let ((paired (mapcar #'cons locations lines)))
    (if (contains-duplicate locations)
	(cons (if left? 'split 'merge) (cons vertical paired))
      (progn			; attachment coercion is reconsidered	
	;;(dolist (p paired) ; l- & r-attach coerce sm. to horiz
	;;  (when (eq (stroke-type (cdr p)) 'small)
	;;    (rplacd p (coerce-stroke (cdr p) 'horizontal))))
	(cons (if left? 'left-attach 'right-attach) (cons vertical paired)))))))

(define-condition confusion (error)
  ((subject :initarg :subject)))
(defmethod print-object ((e confusion) out)
  (with-slots (subject) e (format out "Confused about ~A" subject)))


;; The basic update routine will return a list of (event . strokes)
;; where the events are: 
;; update, appear, disappear
;; inflect: small-to-small
;; left-attach, right-attach: 1st stroke is vertical
;; split: the first stroke is the pre-split stroke
;; merge: the first stroke is the post-merge stroke

;; consider an interval and a set of strokes:
;; Number of connections?
;; 0:  new stroke

;; 1:  stroke and interval both L/M: add interval to stroke
;;     stroke is L/M, interval is S: attach new stroke
;;     stroke is S, interval is L/M: attach new stroke, make old stroke R/F/H
;;     stroke is R/F/H, interval is L/M: attach new stroke
;;     stroke is S, interval is S: add interval to stroke
;;     stroke is R/F/H, interval is S: would interval change type?
;;          If so, attach new stroke
;;          If not, add interval to stroke
;; returns a pair: (old-strokes . new-strokes)
(defun resolve-one-to-one (stroke new-interval)
  (let ((s-type (stroke-type stroke)) (i-type (interval-type new-interval)))
    (cond ((and (find s-type '(medium large))
		(find i-type '(medium large)))
	   (list 'update (add-to-stroke stroke new-interval)))
	  ((find s-type '(medium large)) ; vertical attaches a new small
	   (attach stroke (list (make-stroke (list new-interval))) T))
	  ((find i-type '(medium large)) ; small attaches to new vertical
	   (attach (make-stroke (list new-interval)) 
		   (list (ensure-stroke-angled stroke))
		   nil))
	  ((equal s-type 'small)
	   (list 'update (add-to-stroke stroke new-interval)))
	  ;; Note: inflection might want to be handled differently
	  (T				; if type changes, inflection
	   (let ((extended (add-to-stroke stroke new-interval)))
	     (if (eq (stroke-type stroke) (stroke-type extended))
		 (list 'update extended)
	       (list 'inflect stroke (make-stroke (list new-interval)))))))))

;; 2+: either stroke is L: unknown! error!
;;     either stroke is M: pretend the stroke is S
;;       Actually, though we'd like to do this, asymmetry prevents us
;;     interval is S: merge
;;     interval is L/M: attach new stroke, make old stroke R/F/H
(defun resolve-many-to-one (strokes new-interval)
  (assert (not (find 'large strokes :key #'stroke-type)))
  (when (find 'medium strokes :key #'stroke-type)
    (setf strokes (mapcar (lambda (s) (if (eq (stroke-type s) 'medium) 
					  (coerce-stroke s 'small) 
					s)) 
			  strokes)))
  (if (eq (interval-type new-interval) 'small)
      (append (list 'merge (make-stroke (list new-interval))) strokes)
    (attach (make-stroke (list new-interval))
	    (mapcar #'ensure-stroke-angled strokes)
	    nil)))

;; If two+ intervals connect to the same stroke:
;;     if either interval is L: unknown! error!
;;     if either interval is M: pretend the interval is S
;;     if stroke is L/M, intervals are S: attach new strokes; 
;;                                 if same attachment, is split
;;     if stroke is S, intervals are S: split, make stroke R/F/H
;;     if stroke is R/F/H, intervals are S: split
(defun resolve-one-to-many (stroke new-intervals)
  (labels ((to-stroke (interval) (make-stroke (list interval)))) 
    (let ((coerced (mapcar (ppf ensure-interval-type ? 'small) new-intervals)))
      (let ((new-strokes (mapcar #'to-stroke coerced)))
	(if (find (stroke-type stroke) '(medium large))
	    (attach stroke new-strokes T)
	  (append (list 'split (ensure-stroke-angled stroke)) 
		  new-strokes))))))

(defun resolve-connected-set (strokes new-intervals)
  (let ((num-strokes (length strokes)) (num-intervals (length new-intervals)))
    (cond ((= num-strokes 0)		; disconnected: make new intervals
	   (assert (= num-intervals 1))
	   (list 'appear (make-stroke new-intervals)))
	  ((= num-intervals 0)		; a termination
	   (assert (= num-strokes 1))
	   (cons 'disappear strokes))
	  ((and (= num-strokes 1) (= num-intervals 1))
	   (resolve-one-to-one (first strokes) (first new-intervals)))
	  ((= num-intervals 1)
	   (resolve-many-to-one strokes (first new-intervals)))
	  ((= num-strokes 1)
	   (resolve-one-to-many (first strokes) new-intervals))
	  (t				; many-to-many shouldn't happen!
	   (error 'confusion :subject (list strokes new-intervals))
	   ))))

;; The main stroke-incorporation routine
;; Returns a list of changes to strokes
(defun remove-R-ambiguity (set)
  (if set
      (let ((cur (car set)))
	(if (and (> (length (first cur)) 1) (> (length (second cur)) 1))
	    (progn
	      (when (>= verbosity 2) (format t "R hack used!~%"))
	      (cons (list nil (list (first (second cur))))
		    (cons (list (first cur) (cdr (second cur)))
			  (remove-R-ambiguity (cdr set)))))
	  (cons cur (remove-R-ambiguity (cdr set)))))
    '()))
(defun incorporate-new-intervals (strokes new-intervals)
  (let ((set (bigraph-components strokes new-intervals #'connected-to-stroke)))
    (setf set (remove-R-ambiguity set))	; hack for realing w. capital R
    (mapset #'resolve-connected-set set)))
;; The types of changes that are returned are:
;; (appear stroke)
;; (disappear stroke)
;; (split old-stroke . new-strokes)
;; (merge new-stroke . old-strokes)
;; (inflect old-stroke new-stroke)
;; (update stroke)
;; (left-attach vertical-stroke (location . stroke) ...)
;; (right-attach vertical-stroke (location . stroke) ...)



;;;; REGRESSION TESTING
(defregression test-interval-merge
  (with-temps ((fullheight 7) (n-width 5))
    (labels ((same-attach (a1 a2)
	       (and (eq (first a1) (first a2)) 
		    (stroke-equal (cdr a1) (cdr a2))))
	     (same-event (e1 e2)
	       (let ((type1 (first e1)) (type2 (first e2))
		     (head1 (second e1)) (head2 (second e2))
		     (tail1 (cddr e1)) (tail2 (cddr e2))
		     (attaches '(left-attach right-attach split merge)))
		 (and (eq type1 type2)
		      (stroke-equal head1 head2)
		      (let ((test (if (find type1 attaches) 
				      #'same-attach 
				    #'stroke-equal)))
			(not (position nil (mapcar test tail1 tail2)))))))
	     (same (s i f) 
	       (let ((events (incorporate-new-intervals s i)))
		 (not (position nil (mapcar #'same-event events f))))))
    (let ((i1 (make-interval 0 0)) (i2 (make-interval 0 6))
	  (i3 (make-interval 5 6)) (i4 (make-interval 1 6))
	  (i5 (make-interval 1 1)) (i6 (make-interval 2 3))
	  (i7 (make-interval 0 4)) (i8 (make-interval 4 4)))
      (let ((big (make-stroke (list i2)))
	    (bottom (make-stroke (list i1)))
	    (mid1 (make-stroke (list i6)))
	    (mid2 (make-stroke (list i8)))
	    (mid3 (make-stroke (list (make-interval 3 3))))
	    (top (make-stroke (list i3)))
	    (top2 (make-stroke (list (make-interval 6 6))))
	    (fall (make-stroke (list i1 i5 (coerce-interval i6 'small))))
	    (rise (make-stroke (list i5 i1)))
	    (med (make-stroke (list i7))))
	;; Large stroke:
        (format t "~A~%" (list big)) ;; TODO: remove just for debugging
        (format t "~A~%" (list i1 i8 i3)) ;; ditto
	(assert	(same (list big) (list i1 i8 i3) ; attach three
		      '((left-attach (large nil T (large 0 6))
				     (bottom small nil T (small 0 0))
				     (middle small nil T (small 4 4))
				     (top small nil T (small 5 6))))))
	(assert	(same (list big) (list i8) ; attach a small
		      '((left-attach (large nil T (large 0 6))
				     (middle small nil T (small 4 4))))))
	(assert (same (list big) (list i4) ; update w. large
		      '((update (large nil T (large 1 6) (large 0 6))))))
	(assert (same (list big) (list i6 i8) ; two smalls = split
		      '((split (large nil T (large 0 6))
			       (middle small nil T (small 2 3))
			       (middle small nil T (small 4 4))))))
	(assert (same (list big) (list i7) ; update w. med
		      '((update (large nil T (medium 0 4) (large 0 6))))))
	;; Medium stroke:
	(assert (same (list med) (list i2) ; update w. large
		      '((update (large nil T (large 0 6) (medium 0 4))))))
	(assert (same (list med) (list i7) ; update w. med
		      '((update (medium nil T (medium 0 4) (medium 0 4))))))
	(assert (same (list med) (list i8) ; attach one
		      '((left-attach (medium nil T (medium 0 4))
				     (top small nil T (small 4 4))))))
        (assert (same (list med) (list i1 i8) ; attach two
		      '((left-attach (medium nil T (medium 0 4))
				     (bottom small nil T (small 0 0))
				     (top small nil T (small 4 4))))))
	;; Small stroke:
	(assert (same (list mid2) (list i2) ; attach to large
		      '((right-attach (large nil T (large 0 6))
				      (middle horizontal T T (small 4 4))))))
	(assert (same (list mid2) (list i7) ; attach to medium
		      '((right-attach (medium nil T (medium 0 4))
				      (top horizontal T T (small 4 4))))))
	(assert (same (list mid2) (list i8) ; extend 
		      '((update (horizontal nil T (small 4 4) (small 4 4))))))
	(assert (same (list rise) (list (make-interval 2 2)) ; extend
		      '((update (rise nil T (small 2 2) (small 1 1) 
				      (small 0 0))))))
	(assert (same (list rise) (list i1) ; inflect
		      '((inflect (rise nil T (small 1 1) (small 0 0))
				 (small nil T (small 0 0))))))
	;; multiple small in:
	(assert (same (list fall mid2 top2) (list i2) ; connect to large
		      '((right-attach (large nil T (large 0 6))
				      (bottom fall nil T (small 0 0) 
					      (small 1 1) (small 2 3))
				      (middle horizontal T T (small 4 4))
				      (top horizontal T T (small 6 6))))))
	(assert (same (list mid3 mid2) (list i7) ; connect to medium
		      '((merge (medium nil T (medium 0 4))
			       (top horizontal T T (small 3 3))
			       (top horizontal T T (small 4 4))))))
	(assert (same (list bottom mid2 top2) (list i5 i8)
		      '((update (rise nil T (small 1 1) (small 0 0)))
			(disappear (small nil T (small 6 6)))
			(update (horizontal nil T (small 4 4) (small 4 4))))))
	;; disconnections
	(assert (same (list mid2) (list i5) ; un-connected
		      '((disappear (small nil T (small 4 4)))
			(appear (small nil T (small 1 1))))))
	(assert (same (list mid2) nil	; old to blank
		      '((disappear (small nil T (small 4 4))))))
	(assert (same nil (list i5)	; blank to new
		      '((appear (small nil T (small 1 1))))))
		
	;; Multiple small strokes:
        'ok)))))
