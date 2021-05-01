;;; History, implemented as a ring buffer
(defstruct history-element past now future)
(defmethod print-object ((h history-element) stream)
  (format stream "~A"
	  (do ((cur (history-element-future h) (history-element-future cur))
	       (l (list (history-element-now h)) 
		  (cons (history-element-now cur) l)))
	      ((eq cur h) (reverse l)))))

(defun make-history (n)
  (let ((first (make-history-element)))
    (setf (history-element-now first) NIL)
    (let ((current first) (last))
      (do ((i 1 (+ i 1)))
	  ((= i n)
	   (setf (history-element-past first) current)
	   (setf (history-element-future current) first)
	   (cons first 0))
	(setq last current)
	(setq current (make-history-element))
	(setf (history-element-now current) NIL)
	(setf (history-element-future last) current)
	(setf (history-element-past current) last)))))

(defun add-to-history (history value)
  (incf (cdr history))
  (rplaca history (history-element-future (car history)))
  (setf (history-element-now (car history)) value))

(defun empty-history? (history) (= (cdr history) 0))

;; positive is future, negative is past
(defun read-history (history index)
  (labels ((walk (h i)
	     (cond ((= i 0) (history-element-now h))
		   ((> i 0) (walk (history-element-future h) (1- i)))
		   (T (walk (history-element-past h) (1+ i))))))
    (walk (car history) index)))

(defregression test-history
  (let ((h (make-history 3)))
    (assert (empty-history? h))
    (dolist (x '(a b c d e)) (add-to-history h x))
    (assert (not (empty-history? h)))
    (assert (eq (read-history h 0) 'e))
    (assert (eq (read-history h 1) 'c))
    (assert (eq (read-history h -1) 'd))
    (assert (eq (read-history h 3) 'e))
    (assert (eq (read-history h -5) 'c))
    'ok))





(defclass finger ()
  ((waiting :initform nil :accessor waiting)
   (history :accessor history)
   (place)
   (time)))
(defmethod print-object ((f finger) stream)
  (with-slots (history place time waiting) f
    (format stream "~:@<F~:_ ~A~:_ ~A~:_ ~A~:_ ~A~:>" 
	    place time waiting history)))
(defmethod future-finger? (finger) (plusp (slot-value finger 'time)))
(defmethod wait-for-input (finger)
  (with-slots (waiting time) finger 
    (assert (not waiting))
    (setf waiting (1+ time))))

(defvar *correlators*)

(defparameter boredom 30)

;; NOTE(namin): this is defined in utils/utils.lisp
;;  but I was getting an error like
;;  https://www.cs.cmu.edu/Groups/AI/html/faqs/lang/lisp/part3/faq-doc-8.html
;;  so redefining here
(defmacro make-iset (init size &rest args)
  (let ((a (gensym)) (i (gensym)) (s (gensym)))
    `(let* ((,s ,size) (,a (make-array ,s ,@args)))
       (dotimes (,i ,s) (setf (aref ,a ,i) ,init)) ,a)))

(defclass correlator ()
  ((name :initarg :name :accessor name)
   (uses :initform 0 :accessor uses)	; number of successful predictions
   (timer :initform boredom :accessor timer) ; countdown to reboot
   (has-pattern :initform nil :accessor has-pattern) ; boolean
   (fingers )				; array of histories
   (pattern )))				; array of contents (?)
(defmethod initialize-instance :after ((c correlator) &key n)
  (with-slots (fingers pattern) c
    (setf fingers (make-iset (make-instance 'finger) n))
    (setf pattern (make-array n))
    (init-a-correlator c)))
(defmethod print-object ((c correlator) stream)
  (with-slots (has-pattern fingers pattern name uses) c
    (format stream "~:@<CORRELATOR ~A ~A ~A ~A~:>" 
	    name has-pattern uses
	    (map 'list 
	      (lambda (p f) 
		(with-slots (place time) f 
		  (list place time p))) 
	      pattern fingers))))

(defun init-correlators (n-correlators n-fingers)
  (reset-counters)
  (setq *correlators* (make-array n-correlators))
  (with-set (i (c *correlators*))
    (setf c (make-instance 'correlator :n n-fingers :name i)))
  'done)

(defun init-a-correlator (the-correlator)
  (with-slots (fingers timer has-pattern uses) the-correlator
    (setf has-pattern nil uses 0)
    (setf timer boredom)
    (with-set (i (f fingers))		; precisely one finger in front...
      (with-slots (time place history has-pattern) f
        (setf time (if (plusp i) (- (random 4)) (1+ (random 3))));[0,-3]or[1,3]
	(setf place (if (plusp i) (- (random 9) 4) (- (random 5) 2)));[-4,+4]or2
	;;(setf history (make-empty-history))
	(setf history (make-history 4))))))

(defun empty-moment? (contents) (eqv contents 'nothing))

;; An important note: the contents acquired by sr-ref don't discriminate
;; between observations and predictions.
(defun run-correlators (text-register)
  (report-step)
  (with-set ((c *correlators*))
    (with-slots (fingers pattern has-pattern timer) c
      (let ((contents 
	     (map 'array 
	       (lambda (f) 
		 (with-slots (place time) f
		   (sr-ref text-register place time)))
	       fingers)))
	;; If there's something interesting at every finger, memorize it
	(when (and (not (find-if #'waiting fingers))
		   (not (position-if #'empty-moment? contents :start 1)))
	  (with-set ((f fingers) (c contents))
	    (if (future-finger? f)
		(wait-for-input f)
	      (with-slots (history) f (add-to-history history c)))))
	;; check if future-fingers are done waiting...
	;; right now, it's hardwired to assume 1 future finger
	(when (waiting (elt fingers 0))
	  (decf (waiting (elt fingers 0)))
	  (unless (plusp (waiting (elt fingers 0)))
	    (with-slots (history time place) (elt fingers 0)
	      (add-to-history 
	       history 
	       (sr-ref text-register place 0 time)))
	    (setf (waiting (elt fingers 0)) nil) 
	    ;; When there's history, but no pattern yet, grab contents
	    (unless has-pattern
	      (map-into pattern (ppf read-history ? 0) 
			(map 'list #'history fingers))
	      (report-learning c)
	      (setf has-pattern T timer 0))))
	;; Match pattern against contents, and if enough match, then force
	(when has-pattern
	  (incf timer)
	  (let ((same (map 'list #'eqv pattern contents)))
	    (when (= (count nil same) 1)
	      (let* ((idx (position nil same))
		     (f (aref fingers idx)) (p (aref pattern idx)))
		(when (zerop idx)	; For now, postdictions are banned
		  (with-slots (place time) f
		    (setf (sr-ref text-register place time) (cons p c))
		    (report-prediction p place time c)))))
	    (report-recognition c)))	; instrumentation for monitoring
	;; If haven't found a pattern, count down toward boredom reset
	(unless has-pattern (decf timer))
	(when (zerop timer) 
	  (report-boredom c)
	  (init-a-correlator c))))))

(defun prediction-feedback (correlator value good?)
  (if good?
      (progn
	(incf (uses correlator))
	(report-confirmed correlator value))
    (progn
      (report-conflict correlator value)
      (init-a-correlator correlator))))


;; testing & instrumentation
(defparameter *step-wallp* nil)
(defvar step-counter 0)
(defun report-step () 
  (and *step-wallp* (format t "."))
  (incf step-counter))
(defparameter *recognition-wallp* nil)
(defvar recognition-counter 0)
(defun report-recognition (correlator)
  (and *recognition-wallp* (format t "Found a correlation!~% ~A~%" correlator))
  (incf recognition-counter))
(defparameter *boredom-wallp* nil)
(defvar boredom-counter 0)
(defun report-boredom (correlator)
  (and *boredom-wallp* 
       (format t "Correlator is resetting from boredom!~% ~A~%" correlator))
  (incf boredom-counter))
(defparameter *learning-wallp* T)
(defvar learning-counter 0)
(defun report-learning (correlator)
  (and *learning-wallp* 
       (format t "Correlator is learning! (T=~A)~% ~A~%" 
	       step-counter correlator))
  (incf learning-counter))
(defparameter *conflict-wallp* T)
(defvar conflict-counter 0)
(defun report-conflict (correlator value)
  (when *conflict-wallp*
    (format t "Correlator conflict detected! (T=~A)~% ~A vs. ~A~%" 
	    step-counter value correlator)
    (pprint-representation sr))
  (incf conflict-counter))
(defparameter *confirmed-wallp* T)
(defvar confirmed-counter 0)
(defun report-confirmed (correlator value)
  (when *confirmed-wallp*
    (format t "Correlator predicted successfully! (T=~A)~% ~A vs. ~A~%"
	    step-counter value correlator)
    (pprint-representation sr))
  (incf confirmed-counter))
(defparameter *prediction-wallp* T)
(defvar prediction-counter 0)
(defun report-prediction (prediction place time correlator)
  (when *prediction-wallp*
    (format t "Correlator is forcing~% ~A at ~A, ~A (T=~A)~% ~A~%" 
	    prediction place time step-counter correlator)
    (pprint-representation sr))
  (incf prediction-counter))



(defun reset-counters ()
  (setf step-counter 0 recognition-counter 0 boredom-counter 0
	learning-counter 0
	conflict-counter 0 confirmed-counter 0 prediction-counter 0))



;;;;;;;;;; BATCH TESTS
(defun book-test (stem n)
  (restore-random)
  
  (setf fullheight 39 n-width 25)
  (setf focus-row 6 focus-col 20)
  (setf sr (make-2D-shift-register 13 24 #'blank-cell-contents))
  (dotimes (i 24) (shift-left sr 1 #'blank-cell-contents) (observe-blanks sr))
  (init-correlators 1000 5)

  (dotimes (i n)
    (format t "Working on portion number ~A...~%" i)
    (setf img (pgm-to-row-stream (format nil "~A~A.pgm" stem i)))
    (while (more-rows img)
      (update-shift-register sr (bits-to-intervals (next-row img)))))
  'done)

;;(book-test "pride/austen.txt" 100) ; 2569 total

;; alisp -L "~/utils/utils" -L "load.lisp" -e '(book-test "pride/austen.txt" 2000)' -e "(pprint *correlators*)" -e '(pprint (list step-counter recognition-counter boredom-counter learning-counter conflict-counter confirmed-counter prediction-counter))' -kill > austen2K.out
;; grep 'Working' austen2K.out | tail -n 1
