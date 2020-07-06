(make-module :fsm (:utils))
(in-package :fsm)

;; FSM implements a Moore or Mealy machine regulated by a collection of 
;; transition tests.  At each step, the machine calls the test for each
;; possible transition from its current state and changes state to the
;; first one that matches; if none match, it stays in the same state.
;; state is transformed to a number, machine is an array of 
;;   (atom output . (test state [output]))
(defclassx fsm ()
  ((state)
   (machine :initarg :machine)))
(defmethod initialize-instance :after ((f fsm) &key state)
  (with-slots ((s state) machine) f
    (setf s (if (numberp state) state (position state machine :key #'first)))))

;; By default, the machine returns its state.  If the state has an output
;; value associated with it, that value is returned instead.  If the 
;; transition has a value associated with it, that value overrides both.
(defmethodx update ((f fsm))
  (with-slots (state machine outputs) f
    (let* ((desc (aref machine state))
	   (new (find-if (lambda (x) (funcall (first x) f)) (cddr desc))))
      (when new (setf state (second new)))
      (if (cddr new) (third new) (second (aref machine state))))))

;; specified as ({state | (state output)} (test new-state [output]))
(defunx fsm-transitions (&rest transitions)
  (let ((set (make-array (length transitions) :initial-element nil)) (names T))
    (labels ((getname (x) (if (consp (car x)) (caar x) (car x)))
	     (tname (x) `(,(first x) ,(position (second x) names) ,@(cddr x))))
      (setf names (map 'array #'getname transitions))
      (dotimes (i (length set))
	(let* ((v (first transitions)) (trans (mapcar #'tname (cdr v)))
	       (out (if (consp (car v)) (cadar v) (car v))))
	  (setf (aref set i) `(,(aref names i) ,out ,@trans)))
	(pop transitions))
      set)))

;; testing gear
(defclass testfsm (fsm)
  ((counter :initform 0)))
(defmethod update ((f testfsm)) 
  (with-slots (counter) f (incf counter) (call-next-method f)))
(defun test3 (f) (with-slots (counter) f (= 0 (mod counter 3))))
(defun test4 (f) (with-slots (counter) f (= 0 (mod counter 4))))
(defun test5 (f) (with-slots (counter) f (= 0 (mod counter 5))))
(defparameter trans
    (fsm-transitions `(a      (,#'test3 b) (,#'test4 c z) (,#'test5 d))
		     `((b x)  (,#'test3 a) (,#'test4 b h) (,#'test5 c))
		     `(c      (,#'test3 d) (,#'test4 e q))
		     `(d      (,#'test3 b) (,#'test4 a) (,#'test5 e r))
		     `((e y))))
(defregression test-fsm
  (let ((mach (make-instance 'testfsm :state 'a :machine trans))
	(log '()))
    (with-slots (counter) mach
      (dotimes (i 15) (let ((v (update mach))) (push (list counter v) log)))
      (setf log (reverse log))
      (tick log) (tick #\newline)
      (assert (eqv (subseq log 00 05) '((01 a) (02 a) (03 x) (04 h) (05 c))))
      (assert (eqv (subseq log 05 10) '((06 d) (07 d) (08 a) (09 x) (10 c))))
      (assert (eqv (subseq log 10 15) '((11 c) (12 d) (13 d) (14 d) (15 x))))))
  (let ((mach (make-instance 'testfsm :state 'b :machine trans))
	(log '()))
    (with-slots (counter) mach
      (setf counter 18)
      (dotimes (i 25) (let ((v (update mach))) (push (list counter v) log)))
      (setf log (reverse log))
      (tick log) (tick #\newline)
      (assert (eqv (subseq log 00 05) '((19 x) (20 h) (21 a) (22 a) (23 a))))
      (assert (eqv (subseq log 05 10) '((24 x) (25 c) (26 c) (27 d) (28 a))))
      (assert (eqv (subseq log 10 15) '((29 a) (30 x) (31 x) (32 h) (33 a))))
      (assert (eqv (subseq log 15 20) '((34 a) (35 d) (36 x) (37 x) (38 x))))
      (assert (eqv (subseq log 20 25) '((39 a) (40 z) (41 c) (42 d) (43 d))))))
  (let ((mach (make-instance 'testfsm :state 'd :machine trans))
	(log '()))
    (with-slots (counter) mach
      (setf counter 33)
      (dotimes (i 5) (let ((v (update mach))) (push (list counter v) log)))
      (setf log (reverse log))
      (tick log) (tick #\newline)
      (assert (eqv (subseq log 00 05) '((34 d) (35 r) (36 y) (37 y) (38 y))))))
  'ok)