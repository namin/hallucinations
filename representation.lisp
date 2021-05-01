;; This uses a shift-register to track strokes reconstructed from 1D
;; visual input.

(defparameter focus-row 4)
(defparameter focus-col 7)

;;;; ***** SIGNATURES *****
;; A signed object is: (signatures . object) where signatures is a list,
;; with the most recent responsible first.  The list can contain duplicates
(defun blank-cell-contents () (list 'blank))
(defun cell-blank? (cell) (eq (first cell) 'blank))
(defun cell-prediction? (cell) (eq (first cell) 'prediction))
(defun cell-observation? (cell) (eq (first cell) 'observation))
(defun cell-postdiction? (cell) (eq (first cell) 'postdiction))
(defun cell-blank! (cell) (rplaca cell 'blank) (rplacd cell '()))
(defun cell-predict! (cell) (rplaca cell 'prediction))
(defun cell-observe! (cell) (rplaca cell 'observation))
(defun cell-postdict! (cell) (rplaca cell 'postdiction))
(defun cell-get-statements (cell) (cdr cell))
(defun cell-clear-statements! (cell) (rplacd cell '()))
(defun cell-add-statement! (cell value signature)
  (rplacd cell (cons (cons value signature) (cell-get-statements cell))))

;; return the top value in the statement
;; Note that this (and other things) do no handle conflicting 
;; postdictions gracefully.
(defun cell-value (cell)
  (car (first (cell-get-statements cell))))

(defun observer? (signature) (eq signature 'observer))
(defun uncertain-observation? (cell)
  (assert (or (cell-observation? cell) (cell-postdiction? cell)))
  (find-if (lambda (statement) (and (observer? (car statement))
				    (eq (cdr statement) '?)))
	   (cell-get-statements cell)))

(defun predict-cell (cell value signature)
  (assert (or (cell-blank? cell) (cell-prediction? cell)))
  (cell-predict! cell)			; ensure it's a prediction
  (cell-add-statement! cell value signature))

(defun score-predictions (value predictions)
  (when predictions			; process one at a time
    (let ((v (if (consp value) (car value) value)))
      (prediction-feedback (cdr (first predictions)) v
			   (eq v (car (first predictions)))))
    (score-predictions value (cdr predictions))))

(defun observe-cell (cell value signature)
  (assert (not (or (cell-observation? cell) (cell-postdiction? cell))))
  (let ((predictions (cell-get-statements cell)))
    (score-predictions value predictions) ; callout to the fingers
    (cell-observe! cell)		; change to be observation
    (cell-add-statement! cell value signature))) ; set value

;; Note: we currently have no way of punishing a bad postdiction
(defun postdict-cell (cell value signature)
  (assert (or (cell-observation? cell) (cell-postdiction? cell)))
  (when (uncertain-observation? cell)
    (cell-postdict! cell)		; change to be observation
    (cell-add-statement! cell value signature)))



;;;; Helper functions for update-shift-register
;; a cell's value is [feature] for pre/post-dictions, and
;; is ([feature] . stroke) for observations
(defun cell-stroke (shift-register row col)
  (let ((v (cell-value (aref shift-register row col))))
    (if (consp v) (cdr v) nil)))
(defun set-cell-stroke (shift-register row col value)
  (let ((v (cell-value (aref shift-register row col))))
    (assert (consp v))
    (rplacd v value)))
(defun cell-feature (shift-register row col)
  (let ((v (cell-value (aref shift-register row col))))
    (if (consp v) (car v) (or v 'blank))))
(defun set-cell-feature (shift-register row col value)
  (let ((v (cell-value (aref shift-register row col))))
    (assert (consp v))
    (rplaca v value)))

(defun current-strokes (shift-register)
  (labels ((strokes-from (i)
	     (if (= i (rows shift-register)) 
		 nil
	       (let ((v (cell-stroke shift-register i focus-col)))
		 (if v (cons v (strokes-from (1+ i)))
		   (strokes-from (1+ i)))))))
    (strokes-from 0)))


;; figure out which row an equivalent stroke is in
(defun find-old-row (shift-register stroke &optional (diff 1))
  (labels ((find-from (row)
	     (assert (< row (rows shift-register)))
	     (if (stroke-eq stroke 
			    (cell-stroke shift-register row (- focus-col diff)))
		 row
	       (find-from (1+ row)))))
    (find-from 0)))

(defun replace-old-versions (shift-register new row col)
  (unless (or (not (stroke-eq new (cell-stroke shift-register row col)))
	      (< col 0))
    (set-cell-stroke shift-register row col new)
    (set-cell-feature shift-register row col (stroke-type new))
    (replace-old-versions shift-register new row (1- col))))

;; feature is stroke-type or nothing
(defun add-observation (shift-register row col stroke)
  (if (and (>= verbosity 3) (consp stroke))
      (format t "Observing ~A at (~A, ~A)~%" stroke row col))
  (let ((cell (aref shift-register row col))
	(value (if (consp stroke) 
		   (cons (stroke-type stroke) stroke) 
		 stroke)))
    (observe-cell cell value 'observer)))

(defun get-observation (shift-register row col)
  (let ((cell (aref shift-register row col)))
    (when (cell-observation? cell) (cell-value cell))))
    
;; Find the right cell to put things into,
;; update old strokes and insert new strokes
(defun incorporate-update (shift-register event) ; a non-change event
  (assert (eq 'update (first event)))
  (let ((changed (second event)))
    (let ((row (find-old-row shift-register changed 0)))
      (replace-old-versions shift-register changed row focus-col))))

(defun incorporate-change (shift-register event)
  (let ((type (first event)))
    (cond 
     ;; For appear: if nothing's occupied, get in focus row.  If something's
     ;; occupied, find the nearest values above and below.  If on an
     ;; edge, go 2 steps away, otherwise go between the two
     ((eq type 'appear)		
      (let ((new (second event)))
	(labels ((find-upper-from (i)
		   (if (>= i (rows shift-register)) nil
		     (let ((obs (cell-stroke shift-register i focus-col)))
		       (if (and obs (most-recent-interval-> obs new))
			   i
			 (find-upper-from (1+ i))))))
		 (find-lower-from (i)
		   (if (<= i 0) nil
		     (let ((obs (cell-stroke shift-register i focus-col)))
		       (if (and obs (most-recent-interval-> new obs))
			   i
			 (find-lower-from (1- i)))))))
	  (let ((upper (find-upper-from 0)) 
		(lower (find-lower-from (1- (rows shift-register)))))
	    (let ((row (cond ((and upper lower) (floor (+ upper lower) 2))
			     (upper (- upper 2))
			     (lower (+ lower 2))
			     (T focus-row))))
	      (assert (not (cell-stroke shift-register row focus-col)))
	      (add-observation shift-register row focus-col new)))))
      )
     ((eq type 'disappear)
      (let ((vanishing (second event)))
	(let ((row (find-old-row shift-register vanishing)))
	  (replace-old-versions shift-register vanishing
				row (1- focus-col)))))
     ((eq type 'split)
      (let ((old (second event)) (news (cddr event)))
	(let ((row (find-old-row shift-register old)))
	  (replace-old-versions shift-register old row (1- focus-col))
	  (assert (= (length news) 2))	; shouldn't see any 3-splits
	  (add-observation shift-register (1+ row) focus-col (elt news 0))
	  (add-observation shift-register (1- row) focus-col (elt news 1))))
      )
     ((eq type 'merge)
      (let ((new (second event)) (olds (cddr event)))
	(let* ((rows (mapcar (pcf find-old-row shift-register) olds))
	       (insert (round (/ (fold #'+ rows 0) (length rows)))))
	  (assert (= (length olds) 2))	; shouldn't see any 3-merges
	  (mapcar (ppf replace-old-versions shift-register ?a ?b (1- focus-col))
		  olds rows)
	  (add-observation shift-register insert focus-col new)))
      )
     ((eq type 'inflect)
      (let ((old (second event)) (new (third event)))
	(let ((row (find-old-row shift-register old)))
	  (replace-old-versions shift-register old row (1- focus-col))
	  (add-observation shift-register row focus-col new))))
     ((eq type 'update)
      (let ((changed (second event)))
	(let ((row (find-old-row shift-register changed)))
	  (if (cell-observation? (aref shift-register row focus-col))
	      (progn 
		(when (>= verbosity 2) (format t "Q hack used!~%"))
		(add-observation shift-register (1+ row) focus-col changed))
	    (add-observation shift-register row focus-col changed))
	  (replace-old-versions shift-register changed row (1- focus-col)))))
     ((eq type 'left-attach)
      (let ((old (second event)) (news (cddr event))
	    (attach-pt '((top . 1) (middle . 0) (bottom . -1))))
	(let ((row (find-old-row shift-register old))
	      (increment (if (eq (stroke-type old) 'large) 2 1)))
	  (replace-old-versions shift-register old row (1- focus-col))
	  (labels ((add-next (attachments)
		     (when attachments
		       (let ((location (caar attachments))
			     (stroke (cdar attachments)))
			 (let ((offset 
				(* (cdr (assoc location attach-pt))
				   increment)))
			   (add-observation shift-register (+ row offset)
					    focus-col stroke)))
		       (add-next (cdr attachments)))))
	    (add-next news)))))
     ((eq type 'right-attach)
      (let ((new (second event)) (olds (cddr event))
	    (attach-pt '((top . -1) (middle . 0) (bottom . 1))))
	(let* ((rows (mapcar (lambda (old)
			       (let ((stroke (cdr old)))
				 (find-old-row shift-register stroke)))
			     olds))
	       (offs (mapcar (lambda (old)
			       (let ((location (car old)))
				 (* (cdr (assoc location attach-pt))
				    (if (eq (stroke-type new) 'large) 2 1))))
			     olds))
	       (insert (round (/ (fold #'+ (append rows offs) 0) (length offs)))))
	  (when (cell-observation? (aref shift-register insert focus-col))
	    (when (>= verbosity 2) (format t "O hack used!~%"))
	    (incf insert))
	  (mapcar (ppf replace-old-versions shift-register ?a ?b (1- focus-col))
		  olds rows)
	  (add-observation shift-register insert focus-col new)))
      )
     (T (error "Unknown event type")))))



(defun observe-blanks (shift-register)
  (labels ((blanks-from (row)
	     (unless (= row (rows shift-register))
	       (let ((cell (aref shift-register row focus-col)))
		 (unless (cell-observation? cell)
		   (add-observation shift-register row focus-col 'nothing)))
	       (blanks-from (1+ row)))))
    (blanks-from 0)))

;; refocusing just shifts rows to put the lowest-row change at the center
(defun cell-stroke-type (shift-register row col)
  (let ((cell (aref shift-register row col)))
    (cond ((cell-blank? cell) 'blank)
	  ((consp (cell-value cell)) (stroke-type (cell-value cell)))
	  (T (cell-value cell)))))

(defparameter shift-history (list 0))

(defun refocus (shift-register)
  (let ((num-rows (rows shift-register)))
    (labels ((first-change-from (row)
	       (if (= row num-rows)
		   (error "Refocus expects that a change exists")
		 (let ((t1 (cell-stroke-type shift-register row focus-col))
		       (t2 (cell-stroke-type shift-register row (1- focus-col))))
		   (assert (and (not (eq t1 'blank))))
		   (if (or (eq t1 t2) (and (eq t1 'nothing) 
					   (find t2 '(nothing blank))))
		       (first-change-from (1+ row))
		     row)))))
      (let ((delta (- (first-change-from 0) focus-row)))
	(push delta shift-history)
	(cond ((minusp delta) 
	       (shift-down shift-register (- delta) #'blank-cell-contents))
	      ((plusp delta) 
	       (shift-up shift-register delta #'blank-cell-contents))
	      (T T))))))			; zero = no change

;; Display shows highest-number rows at top, past at left
(defun pprint-cell (c &optional (stream T))
  (format stream "~A" 
	  (feature-character (if (cell-blank? c) 'blank 
			       (let ((v (cell-value c)))
				 (if (consp v) (stroke-type v) v)))
			     (cell-observation? c))))

(defun pprint-representation (shift-register &optional  (stream T))
  (let ((rows (rows shift-register)) (cols (cols shift-register)))
    (dotimes (i rows)
      (dotimes (j cols)
	(pprint-cell (aref shift-register (- rows i 1) j) stream))
      (format stream "~%"))))

;; intervals are ordered from minimum to maximum
(defun update-shift-register (shift-register intervals)
  (let* ((current (current-strokes shift-register))
	 (changes (incorporate-new-intervals current intervals)))
    (if (find 'update changes :test-not #'eq :key #'first) ; time advances?
	(progn				; enter new column
	  (shift-left shift-register 1 #'blank-cell-contents)
	  (let* ((ret (split (lambda (c) (eq (first c) 'appear)) changes))
		 (others (second ret))
		 (appears (first ret)))
	    (mapcar (pcf incorporate-change shift-register) others)
	    (mapcar (pcf incorporate-change shift-register) appears))
	  (observe-blanks shift-register)
	  (refocus shift-register)
	  (run-correlators shift-register) ; call the learner
	  )
      (mapcar (pcf incorporate-update shift-register) changes))
    shift-register))


;;;;;;;;;;;
;; Virtual references: future is positive, as is high values
(defun sr-ref (shift-register place time &optional (past 0))
  (let ((delta (fold #'+ (subseq shift-history 0 past) 0))
	(time-ref (+ time focus-col)))
    (let ((new-place (+ (- place delta) focus-row)))
      (if (and (>= new-place 0) (< new-place (rows shift-register))
	       (>= time-ref 0) (< time-ref (cols shift-register)))
	  (cell-feature shift-register new-place time-ref)
	'blank))))			; return default for out-of-bounds
(defun (setf sr-ref) (val shift-register place time &optional (past 0))
  (let ((delta (fold #'+ (subseq shift-history 0 past) 0))
	(time-ref (+ time focus-col)))
    (let ((new-place (+ (- place delta) focus-row)))
      (if (and (>= new-place 0) (< new-place (rows shift-register))
	       (>= time-ref 0) (< time-ref (cols shift-register)))
	  (predict-cell (aref shift-register new-place time-ref)
			(car val) (cdr val))
	(warn "setf sr-ref to out-of-bounds (~A, ~A, ~A)" 
	      place time past)))))


(defvar *randstate* (make-random-state *random-state*))
(defun save-random ()
  (setf *randstate* (make-random-state *random-state*)))
(defun restore-random ()
  (setf *random-state* *randstate*)
  (save-random))


#|

(setf img (pgm-to-row-stream "sample.pgm"))
(setf fullheight 7 n-width 5)
(setf sr (make-2D-shift-register 9 10 #'blank-cell-contents))
(pprint-representation sr)
(progn (update-shift-register sr (bits-to-intervals (next-row img)))
       (pprint-representation sr))


(setf fullheight 10 n-width 6)
(setf focus-row 6 focus-col 10)
(setf sr (make-2D-shift-register 13 13 #'blank-cell-contents))
(pprint-representation sr)
(setf img (pgm-to-row-stream "pride/austen.txt0.pgm"))
(progn (dotimes (i 10) (update-shift-register sr (bits-to-intervals (next-row img)))) (pprint-representation sr))

(setf fullheight 39 n-width 25)
(setf focus-row 6 focus-col 20)
(setf sr (make-2D-shift-register 13 24 #'blank-cell-contents))
(dotimes (i 24) (shift-left sr 1 #'blank-cell-contents) (observe-blanks sr))
(pprint-representation sr)
(setf img (pgm-to-row-stream "pride/sampler.txt0.pgm"))
(restore-random)
(init-correlators 100 3)
(progn (dotimes (i 40) (update-shift-register sr (bits-to-intervals (next-row img)))) (pprint-representation sr))


|#
