;; assuming two intervals are related, motion is some subset of 
;; 'RISE, 'FALL, 'EXPAND, 'CONTRACT
(defun interval-relation (old new)
  (let ((b_old (car old)) (b_new (car new))
	(t_old (cdr old)) (t_new (cdr new)))
    (append
     ;;(when (<= (- t_old b_old) (- t_new b_new)) '(EXPAND))
     ;;(when (>= (- t_old b_old) (- t_new b_new)) '(CONTRACT))
     (when (and (= b_old b_new) (= t_old t_new)) '(RISE FALL))
     (when (or (< b_old b_new) (< t_old t_new)) '(RISE))
     (when (or (> b_old b_new) (> t_old t_new)) '(FALL)))))
;; a new trajectory can be anything...
(defun new-trajectory () '(RISE FALL)) ;; expand & contract removed
;; returns 2 values: the new trajectory and whether there's been a change evt
(defun extend-trajectory (cur new)
  (let ((merged (intersection cur new)))
    (or merged 
	(values new 
		(cond ((and (eqv cur '(RISE)) (eqv new '(FALL))) 'CAP)
		      ((and (eqv cur '(FALL)) (eqv new '(RISE))) 'CUP)
		      (T (error "~A-~A is not cup or cap!" cur new)))))))

;; make a matrix of interval similarity relations
(defun similarity-matrix (old new)
  (cons
   (mapcar (lambda (o) (remove-if-not (pcf interval-similar o) new)) old)
   (mapcar (lambda (n) (remove-if-not (ppf interval-similar ? n) old)) new)))
;; returns a list of (event-type pre-intervals post-intervals)
(defun events (old new)
  (let* ((m (similarity-matrix old new))
	 (mold (mapcar #'cons old (car m))) (mnew (mapcar #'cons new (cdr m))))
    (remove 
     nil
     (append
      (mapcar (lambda (e)
		(cond ((not (cdr e)) (list 'disappear (list (car e))))
		      ((> (length (cdr e)) 1) 
		       (list 'split (list (car e)) (cdr e)))))
	      mold)
      (mapcar (lambda (e)
		(cond ((not (cdr e)) (list 'appear nil (list (car e))))
		      ((> (length (cdr e)) 1) 
		       (list 'merge (cdr e) (list (car e))))))
	      mnew)))))
	  
(defregression test-1D-basics
  (let ((slices #(#(0 0 0 0 0 0 0 0) 
		  #(0 0 1 0 0 0 0 0) 
		  #(0 1 0 0 0 1 1 1)
		  #(1 1 0 1 1 0 0 0) 
		  #(0 1 1 1 1 0 0 0) 
		  #(1 0 0 1 0 0 1 0)
		  #(0 0 1 0 0 0 0 0))))
    (tick "Testing interval creation...")(tick #\newline)
    (assert (eqv (bits-to-intervals (aref slices 0)) '()))
    (assert (eqv (bits-to-intervals (aref slices 1)) '((2 . 2))))
    (assert (eqv (bits-to-intervals (aref slices 2)) '((1 . 1) (5 . 7))))
    (assert (eqv (bits-to-intervals (aref slices 3)) '((0 . 1) (3 . 4))))
    (assert (eqv (bits-to-intervals (aref slices 4)) '((1 . 4))))
    (assert (eqv (bits-to-intervals (aref slices 5)) '((0 . 0)(3 . 3)(6 . 6))))
    (tick "Testing similarity detection...")(tick #\newline)
    (assert (interval-similar '(4 . 5) '(2 . 3)))
    (assert (not (interval-similar '(4 . 5) '(1 . 2))))
    (assert (interval-similar '(0 . 1) '(2 . 3)))
    (assert (not (interval-similar '(0 . 0) '(3 . 3))))
    (assert (interval-similar '(1 . 1) '(1 . 1)))
    (assert (interval-similar '(1 . 3) '(2 . 5)))
    (assert (interval-similar '(2 . 5) '(1 . 3)))
    (tick "Testing interval relation categorization...")(tick #\newline)
    (let ((r 'RISE) (f 'FALL))
      (assert (eqv (interval-relation '(1 . 3) '(2 . 4)) (list r)))
      (assert (eqv (interval-relation '(1 . 3) '(2 . 2)) (list r f)))
      (assert (eqv (interval-relation '(1 . 3) '(2 . 3)) (list r)))
      (assert (eqv (interval-relation '(1 . 3) '(0 . 4)) (list r f)))
      (assert (eqv (interval-relation '(1 . 3) '(0 . 2)) (list f)))
      (assert (eqv (interval-relation '(1 . 3) '(0 . 3)) (list f)))
      (assert (eqv (interval-relation '(1 . 3) '(1 . 4)) (list r)))
      (assert (eqv (interval-relation '(1 . 3) '(1 . 2)) (list f)))
      (assert (eqv (interval-relation '(1 . 3) '(1 . 3)) (list r f))))
    (tick "Testing trajectory extension...")(tick #\newline)
    (let ((tr (new-trajectory)))
      (setf tr (extend-trajectory tr (interval-relation '(1 . 3) '(2 . 3))))
      (assert (not (set-exclusive-or tr '(RISE))))
      (setf tr (extend-trajectory tr (interval-relation '(2 . 3) '(2 . 4))))
      (assert (not (set-exclusive-or tr '(RISE))))
      (setf tr (extend-trajectory tr (interval-relation '(2 . 4) '(2 . 4))))
      (assert (not (set-exclusive-or tr '(RISE))))
      (setf tr (extend-trajectory tr (interval-relation '(2 . 4) '(1 . 3))))
      (assert (not (set-exclusive-or tr '(FALL)))) ;post change
      (setf tr (extend-trajectory tr (interval-relation '(1 . 3) '(2 . 2))))
      (assert (not (set-exclusive-or tr '(FALL)))))
    (tick "Testing event detections...")(tick #\newline)
    (let* ((ints (map 'list #'bits-to-intervals slices))
	   (evts (mapcar #'events (subseq ints 0 6) (subseq ints 1 7))))
      (assert (eqv (elt evts 0) '((appear nil ((2 . 2)))))) 
      (assert (eqv (elt evts 1) '((appear nil ((5 . 7)))))) 
      (assert (eqv (elt evts 2) '()))
      (assert (eqv (elt evts 3) '((merge ((0 . 1) (3 . 4)) ((1 . 4))))))
      (assert (eqv (elt evts 4) '((split ((1 . 4)) ((0 . 0) (3 . 3))) 
				 (appear nil ((6 . 6))))))
      (assert (eqv (elt evts 5) '((disappear ((0 . 0))) 
				  (disappear ((6 . 6)))))))
    'ok))

;; System state is a set of intervals & their associated live network
;; edges An edge has start and end events, a trajectory, and a seq. of
;; intervals @ times. An event has previous edges, following edges, a
;; type, and a time
;; Each event pushes a set of new events onto the map
;; The event always shifts things left by one step, and puts the
;; first listed change in the middle.
;; If the event is connected to an edge (i.e. not an APPEAR), then it 
;;   shifts up/down to put that edge in the middle.
;; If the event is an APPEAR, then it shifts into the nearest unoccupied
;; row, searching downward.

;;;; ***** EVENTS *****
;; In our shift register, each cell stores an event (or nil), with elements:
(defclass event (signable)
  ;; edges listed in rising order
  ((back-edges :initform '() :initarg :back :accessor back-edges)
   (forw-edges :initform '() :initarg :forw :accessor forw-edges)
   (type :initarg :type :accessor etype) ; symbol
   (time :initarg :time :initform nil :accessor etime))) ; number
(defmethod print-object ((e event) stream)
  (format stream "~:@<~A~:_ ~A~:_ ~A~:_ ~A~:>" (etype e) (etime e)
	  (back-edges e) (forw-edges e)))
(defun make-default-event () (make-instance 'event :type 'empty))
(defmethod pixrow ((e event))		; minimum pixel row in the event
  (let ((be1 (first (back-edges e))) (fe1 (first (forw-edges e))))
    (minv (and be1 (car (current-interval be1)))
	  (and fe1 (car (elt (intervals fe1) 0))))))
(defmethod empty-event? ((e event)) (eq (etype e) 'EMPTY))
;; Note: when postdiction starts, ^ can't visually distinguish pred v. obs
(defmethod event-character ((e event))
  (ecase (etype e) 
    (EMPTY #\.) (CUP #\U) (CAP #\^) (APPEAR #\A) 
    (DISAPPEAR #\D) (SPLIT #\S) (MERGE #\M)))

;;;; ***** EDGES *****
(defclass edge (signable)
  ((start :initarg :start :initform nil :accessor start) ; event
   (end :initform nil :accessor end)	; event
   (intervals :initform (make-array 0 :fill-pointer 0 :adjustable t) 
	      :accessor intervals)
   (trajectory :initform (new-trajectory) :initarg :tr :accessor trajectory)))
(defmethod initialize-instance :after ((e edge) &key int)
  (vector-push-extend int (intervals e)))
(defmethod print-object ((e edge) stream)
  (format stream "~:@<EDGE~:_ ~A~:_ ~A~:_ TYPE=~A~:_ LEN=~A~:_ CE=~A~:>" 
	  (and (start e) (etype (start e))) (and (end e) (etype (end e))) 
	  (trajectory e) (length (intervals e)) (current-interval e)))
(defun make-default-edge () (make-instance 'signable))
(defmethod empty-edge? (edge) (not (typep edge 'edge)))
;; returns either nil or a change event connecting it to a new edge
;; (the change event needs to have its time filled in)
(defmethod pixrow ((e edge)) (car (current-interval e)))
(defmethod live? (default) (declare (ignore default)) nil)
(defmethod live? ((e edge)) (not (end e)))
(defmethod current-interval ((e edge))
  (elt (intervals e) (1- (length (intervals e)))))
(defmethod extend ((e edge) interval)
  (assert (live? e))		; can't extend closed edges
  (multiple-value-bind (tr evt) 
      (extend-trajectory (trajectory e) 
			 (interval-relation (current-interval e) interval))
    (if evt
	(let ((new (observe (make-instance 'edge :tr tr :int interval))))
	  (setf (end e) 
	    (observe (make-instance 'event :back (list e) 
				    :forw (list new) :type evt)))
	  (setf (start new) (end e))
	  (end e))
      (progn 
	(vector-push-extend interval (intervals e))
	(setf (trajectory e) tr)
	nil))))
;; if precisely one interval matches the edge, extend it
(defmethod find-and-extend ((e edge) intervals)
  (let ((found (remove-if-not (pcf interval-similar (current-interval e)) 
			      intervals)))
    (when (= (length found) 1) (extend e (first found)))))


;;;;; ***** TEXT-REGISTER *****
(defclass text-register ()
  ((time :initform -1 :accessor etime)	; current at insertion-point
   (insertion-point :initarg :ip :accessor ip) ; (row column) for insertion
   (register :accessor register)	; shift register of (event . edge)
   (history :initform (list 0) :accessor history) ; list of shift deltas
   (edges :accessor edges)))		; current live edges
(defun make-default-tr-entry ()
  (cons (make-default-event) (make-default-edge)))
(defmethod initialize-instance :after ((tr text-register) &key r c)
  (setf (register tr) (make-2D-shift-register r c)) ; empty to start
  (dotimes (i r)
    (dotimes (j c)
      (setf (aref (register tr) i j) (make-default-tr-entry))))
  (setf (edges tr) nil))		; initially no live intervals
;; observed = upcase, predicted = lowcase, neither = .
(defmethod print-object ((tr text-register) stream)
  (dotimes (i (rows (register tr)))
    (dotimes (j (cols (register tr)))
      (let* ((val (aref (register tr) i j))
	     (event (car val)) (edge (cdr val)))
	(format stream "~A"
		(cond ((not (empty-event? event)) ; event -> character
		       (let ((c (event-character event)))
			 (if (or (prediction? event) (prediction? edge))
			     (char-downcase c) ; lower case if prediction
			   (char-upcase c))))
		      ((and (observation? edge) (not (empty-edge? edge))) "-")
		      ((prediction? edge) "~")
		      ((not (default? event)) ".") ; observed empty
		      (T "_")))))	; default
    (format stream "~%")))

;; Virtual references: future is positive, as is high values
(defmethod sr-ref ((tr text-register) place time &optional (past 0))
  (let ((delta (fold #'+ (subseq (history tr) 0 past) 0))
	(time-ref (+ time (second (ip tr)))))
    (let ((new-place (+ (- place delta) (first (ip tr)))))
      (if (and (>= new-place 0) (< new-place (rows (register tr)))
	       (>= time-ref 0) (< time-ref (cols (register tr))))
	  (aref (register tr) new-place time-ref)
	(make-default-tr-entry)))))	; return default
(defmethod (setf sr-ref) 
    (val (tr text-register) place time &optional (past 0))
  (let ((delta (fold #'+ (subseq (history tr) 0 past) 0))
	(time-ref (+ time (second (ip tr)))))
    (let ((new-place (+ (- place delta) (first (ip tr)))))
      (if (and (>= new-place 0) (< new-place (rows (register tr)))
	       (>= time-ref 0) (< time-ref (cols (register tr))))
	  (setf (aref (register tr) new-place time-ref) val)
	(warn "setf sr-ref to out-of-bounds (~A, ~A, ~A)" 
	      place time past)))))

(defmethod current-intervals ((tr text-register))
  (mapcar #'current-interval (edges tr)))
(defmethod interval-to-edge ((tr text-register) interval)
  (find interval (edges tr) :key #'current-interval :test #'eqv))
;; return the row containing the edge, if one exists
(defmethod edge-to-row ((tr text-register) edge &optional col)
  (unless col (setf col (second (ip tr))))
  (dotimes (i (rows (register tr)))
    (when (eq edge (cdr (aref (register tr) i col))) (return i))))
;; return the row containing the event, if one exists
(defmethod event-to-row ((tr text-register) event &optional col)
  (unless col (setf col (second (ip tr))))
  (dotimes (i (rows (register tr)))
    (when (eq event (car (aref (register tr) i col))) (return i))))
;; return the change events generated by extending live edges
(defmethod trajectory-events ((tr text-register) intervals)
  (let* ((live (remove-if-not #'live? (edges tr)))
	 (evts (remove nil (mapcar (ppf find-and-extend ? intervals) live))))
    (dolist (x evts) (setf (etime x) (etime tr))) 
    evts))
;; find the raw events, then convert them to event objects
(defmethod interval-events ((tr text-register) intervals)
  (labels ((make-edge (e) 
	     (observe (make-instance 'edge :tr (new-trajectory) :int e)))
	   (new-evt (e)
	     (let* ((b (mapcar (pcf interval-to-edge tr) (second e)))
		    (f (mapcar #'make-edge (third e)))
		    (new (observe (make-instance 'event :back b :forw f 
					:type (first e) :time (etime tr)))))
	       (dolist (x b) (setf (end x) new))
	       (dolist (x f) (setf (start x) new))
	       new)))
    (mapcar #'new-evt (events (current-intervals tr) intervals))))




;; live edge, no event: same row
;; If ever something tries to write to a non-empty cell, collision gives error
;; If there's a prediction in the register, check and see if it's reasonable
(defun write-to-register (sr x y val)
  (when (aref sr x y)
    (let ((old (aref sr x y)))
      (assert (or (default? (car old)) (prediction? (car old))))
      (assert (or (default? (cdr old)) (prediction? (cdr old))))
      (check-for-collision old val reg)))
  (setf (aref sr x y) val))

;; Rules for extending the grid with a new set of events:
(defmethod insert-event ((tr text-register) (e event))
  (destructuring-bind (row col) (ip tr)	; default to insertion point
    (ecase (etype e) 
      ((cup cap) ;; CHANGE: event to same row as back edge, bound to forw edge
       (setf row (edge-to-row tr (first (back-edges e)) (1- col)))
       (assert row (e col) "Couldn't find edge for ~A in col ~A" e (1- col))
       (write-to-register (register tr) row col 
			  (cons e (first (forw-edges e)))))
      (merge ;; MERGE: into row of lowest back edge
       (setf row (edge-to-row tr (first (back-edges e)) (1- col)))
       (assert row (e col) "Couldn't find edge for ~A in col ~A" e (1- col))
       (write-to-register (register tr) row col 
			  (cons e (first (forw-edges e)))))
      (disappear ;; DISAPPEAR: into row of back edge, no live edge
       (setf row (edge-to-row tr (first (back-edges e)) (1- col)))
       (assert row (e col) "Couldn't find edge for ~A in col ~A" e (1- col))
       (write-to-register (register tr) row col 
			  (cons e (observe (make-default-edge)))))
      (split ;; SPLIT: forw separated by 1 blank, centered around back edge
       (setf row (edge-to-row tr (first (back-edges e)) (1- col)))
       (assert row (e col) "Couldn't find edge for ~A in col ~A" e (1- col))
       (let ((homerow row))
	 (incf row (- 1 (length (forw-edges e)))) ; shift IP down
	 (dolist (fe (forw-edges e))
	   (write-to-register (register tr) row col 
			      (cons (if (= row homerow) e (make-default-event))
				    fe))
	   (incf row 2))
	 (when (evenp (length (forw-edges e))) ; when even, homerow was skipped
	   (write-to-register (register tr) homerow col 
			      (cons e (observe (make-default-edge)))))))
      (appear ;; APPEAR: add far from live or default to insertion point
       (let* ((lower nil) (upper nil) (edge (first (forw-edges e)))
	      (int (current-interval edge)))
	 (dotimes (i (rows (register tr)))
	   (let* ((edge- (cdr (aref (register tr) i (1- col))))
		  (edge+ (cdr (aref (register tr) i col)))
		  (eint (or (and (live? edge-) (current-interval edge-))
			    (and (live? edge+) (current-interval edge+)))))
	     (when eint
	       (when (< (cdr eint) (car int)) (setf lower i))
	       (when (and (not upper) (> (car eint) (cdr int)))
		 (setf upper i)))))
	 ;; split difference if in middle, otherwise separate w. one blank
	 (cond ((and lower upper) (setf row (floor (/ (+ lower upper) 2))))
	       (upper (setf row (- upper 2)))
	       (lower (setf row (+ lower 2))))
	 (write-to-register (register tr) row col (cons e edge)))))))

;; extend those edges that are live but have no events
;; Must be called after all events are inserted(
(defmethod extend-live-edges ((tr text-register))
  (let ((col (second (ip tr))))
    (dotimes (i (rows (register tr)))
      (let ((old (cdr (aref (register tr) i (1- col))))
	    (new (cdr (aref (register tr) i col))))
	(when (and (live? old) (not (observation? new)))
	  (write-to-register (register tr) i col 
			     (cons (observe (make-default-event)) old)))))))

(defmethod add-observations ((tr text-register) events)
  (shift-left (register tr) 1 #'make-default-tr-entry) ; shift to insert
  (let ((appears (remove 'appear events :key #'etype :test-not #'eq))
	(others (remove 'appear events :key #'etype)))
    (dolist (e others) (insert-event tr e)) ; insert events
    (dolist (e appears) (insert-event tr e)) ; insert events
    (extend-live-edges tr))		; extend the live edges
  (dotimes (i (rows (register tr)))	; add observations of emptiness
    (let* ((val (aref (register tr) i (second (ip tr))))
	   (old (cons (car val) (cdr val))))
      (unless (observation? (car val)) 
	(rplaca val (observe (make-default-event))))
      (unless (observation? (cdr val))
	(rplacd val (observe (make-default-edge))))
      (when (or (prediction? (car old)) (prediction? (cdr old)))
	(check-for-collision old val tr)))))

;; assuming no split/merge overlap, no edge participates in more than
;; one event at a time, and may participate in none.
(defmethod update ((tr text-register) percepts)
  (incf (etime tr))			; begin by adding time
  (let* ((ints (bits-to-intervals percepts))
	 (ievts (interval-events tr ints))
	 (evts (append ievts (trajectory-events tr ints))))
    (when evts				; do nothing if no events
      (add-observations tr evts)
      (let* ((sortevts (sort evts #'lt :key #'pixrow)) ; recenter on first evt
	     (new-center (event-to-row tr (first sortevts))))
	(assert new-center (sortevts) "~A" sortevts)
	(let ((delta (- new-center (first (ip tr)))))
	  (push delta (history tr))
	  (if (> new-center (first (ip tr)))
	      (shift-up (register tr) delta  #'make-default-tr-entry)
	    (shift-down (register tr) (- delta)  #'make-default-tr-entry)))
	(setf (edges tr)		; update edge list
	  (sort (apply #'append (remove-if-not #'live? (edges tr))
		       (mapcar #'forw-edges sortevts))
		#'< :key #'pixrow)))
      (run-correlators tr))
    tr))



;;; For asserting predictions and changes to perception, 
;;; the specified content must be able to assert that it
;;; does not intend to clobber certain values.
;;; Type may be STET, meaning I don't want to clobber this.
;;; Trajectory attributes are a list of symbols and (NOT symbol)
;;;          sublists, where a symbol X says X should be added
;;;          to the attribute list, (NOT X) says it should be
;;;          deleted, and not mentioning X means it's left alone.
(defun content (cell)
  (let ((event (car cell)) (edge (cdr cell)))
    (cons (etype event)			; event type
	  nil)))			; edge information is denied!
;;	  (if (empty-edge? edge) nil (trajectory edge))))) ; attributes


;; Merge a prediction into the contents of the text register, clobbering
;; the current contents as necessary.
;; Note: place & time references are in virtual coordinates
;; Note that this currently does not handle actual predictions of the
;; future correctly, only postdictions of the past.
(defparameter *allow-postdiction* nil)
(defun force-value (text-register place time prediction signature)
  (let* ((type (car prediction)) (attributes (cdr prediction))
	 (elt (sr-ref text-register place time))
	 (evt (car elt)) (edge (cdr elt)))
    (when (or *allow-postdiction* (plusp time))
      ;; first, force the event
      (unless (eq type 'stet)
	(assert (symbolp type))
	(when (observation? evt)
	  (unless (or (empty-event? evt) (eq (etype evt) type))
	    (prediction-feedback signature text-register nil)))
	(setf (etype evt) type)
	(sign evt signature))
      ;; next, force the edge attributes
      ;; Note: right now, only positive things are handled
      (when attributes
	(if (empty-edge? edge)
	    (rplacd elt (sign (make-instance 'edge :tr attributes) signature))
	  (progn
	    (setf (trajectory edge)
	      (union (trajectory edge) attributes))
	    (sign edge signature))))
      (report-prediction text-register prediction place time signature))
    elt))

(defun check-for-collision (prediction observed register)
  (let ((p_event (car prediction)) (o_event (car observed))
	(p_edge (cdr prediction)) (o_edge (cdr observed))
	(ok_event T) (ok_edge T))
    (assert (not (observation? p_event)))
    (assert (not (observation? p_edge)))
    (when (prediction? p_event)
      (setf ok_event (eqv (etype p_event) (etype o_event))))
    (when (prediction? p_edge)
      (setf ok_edge (and (eq (empty-edge? p_edge) (empty-edge? o_edge))
			 (subsetp (trajectory p_edge) (trajectory o_edge)))))
    ;; report misses, not double-counting
    (if (and (prediction? p_event)
	     (eq (signature p_event) (signature p_edge)))
	(prediction-feedback (signature p_event) register 
			     (and ok_event ok_edge))
      (progn
	(when (prediction? p_event)
	  (prediction-feedback (signature p_event) register ok_event))
	(when (prediction? p_edge)
	  (prediction-feedback (signature p_edge) register ok_edge))))))

(defvar *randstate*)
(defun save-random ()
  (setf *randstate* (make-random-state *random-state*)))
(defun restore-random ()
  (setf *random-state* *randstate*)
  (save-random))

#|
;; testing
(save-random)
(restore-random)
(init-correlators 100 5)
(init-correlators 1000 5)
(setf reg (make-instance 'text-register :r 12 :c 11 :ip '(6 7)))
(setf img (pgm-to-1D-vision "sample.pgm")) ;; or sample2.pgm
(setf n 0)
(update reg (elt img (incf n)))
(with-set ((pix img)) (update reg pix))
|#

