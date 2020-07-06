(defpackage :utils
  (:use :common-lisp)
  (:export "DEFINE-EXPORTER" "PUBLIC" "COPY" "CLONE" "EVALWHEN"))
(eval-when (:load-toplevel :execute)
  (use-package :utils)) ;; make whoever's loading it use it...
(in-package utils)

;;;; EVALWHEN
(defmacro evalwhen (time &rest body)
  "Allows shorter keywords to be used in eval-when statements"
  (let* ((newargs (substitute :compile-toplevel :c 
			      (substitute :load-toplevel :l
					  (substitute :execute :x time)))))
    `(eval-when ,newargs ,@body)))

;;;; RANDOM RANDOMIZATION
;; Sometimes the random state isn't automatically randomized on startup.
;; This fixes that.
(evalwhen (:l :x) (setf *random-state* (make-random-state t)))

;;;; EXPORT FORMS
;; (define-exporter n) makes a macro nx that exports the symbol it
;; creates.  The following exporters are defined here: defunx,
;; defconstantx, defvarx, defparameterx, defclassx, defmacrox,
;; defmethodx.  Finally, (public &rest x) is an macro to export from the
;; current package
(defmacro define-exporter (name)
  (let ((xname (intern (format nil "~AX" (symbol-name name)))))
    `(progn
       (evalwhen (:l :x) (export ',xname))
       (defmacro ,xname (name &rest args)
	 (list 'progn
	    (cons ',name (cons name args))
	    `(evalwhen (:l :x) (export ',name)))))))

(define-exporter defun)
(define-exporter defconstant)
(define-exporter defvar)
(define-exporter defparameter)
(define-exporter defclass)
(define-exporter defmacro)
(define-exporter defmethod)

(defmacro public (&rest args)
  `(evalwhen (:l :x) (export (quote ,args))))

;;;; REGRESSION TESTS
(defvarx regression-tests '())
(defmacrox defregression (name &rest body)
  `(progn
     (pushnew ',name regression-tests)
     (defun ,name () ,@body)))
(defunx regression-test ()
  (let ((fail nil))
    (format t "Beginning regression testing~%")
    (dolist (test regression-tests)
      (format t "Beginning ~W~%" test)
      (multiple-value-bind (value condition)
	  (ignore-errors (funcall (symbol-function test)))
	(declare (ignore value))
	(format t "~A " test) 
	(if (typep condition 'error) 
	    (progn (setf fail t) (format t "FAILED: ~A~%" condition))
	  (format t "ok~%"))))
    (if fail :failed :ok)))

;;;; UNSPECIFIED
;; unspecified is a special value, taken from Scheme, which allows
;; a function to distinguish between "I'm not supplying a value" and NIL
(defvarx unspecified (make-symbol "UNSPECIFIED"))
(defunx specifiedp (x)
  (not (eq x unspecified)))

;;;; NODEFAULT
;; I often define base methods for the purpose of announcing that 
;; this method is expected to be defined for subclasses.  This is
;; so I don't have to keep rewriting the contents.
(defunx nodefault (id) (error "No default method for ~A." id))

;;;; NOP
;; No-operation function: supply to callbacks that you don't want to use
;; nop returns nil; nop* returns a function that returns rval
(defunx nop (&rest args) (declare (ignore args)) nil)
(defunx nop* (rval) (lambda (&rest args) (declare (ignore args)) rval))

;;;; TP
;; tp converts a generalized boolean to a boolean (only T or NIL)
(defunx tp (x) (if x t nil))

;;;; NULLP
;; a generalized test for emptiness/blankness
(defmethodx nullp (x) (null x))

;;;; COPY
(defmethod copy (obj) obj)
(defmethod copy ((obj structure-object)) (copy-structure obj))
(defmethod copy ((obj sequence)) (copy-seq obj))

;;;; CLONE
;; clone is like copy, except it guarantees that the two objects can be safely 
;; mutated without affecting one another.
(defmethod clone (obj)  ;; default is to throw an error 
  (if obj (error (format nil "No clone function defined for ~A" obj))
    nil))
(defmethod clone ((s symbol)) s)
(defmethod clone ((n number)) n)
(defmethod clone ((c character)) c)
(defmethod clone ((c cons)) (cons (clone (car c)) (clone (cdr c))))
(defmethod clone ((s string)) s)
(defmethod clone ((f function)) f)
(defmethod clone ((s sequence)) (map (type-of s) #'clone s))

;;;; NUM-SAFE
;; takes a predicate that requires numbers and checks for numbers before
;; applying it.  When given arguments, it evaluates the function, otherwise
;; it returns the new safe predicate.
(defmacrox num-safe (fn &optional (x nil xp))
  (if xp
      `(and (numberp ,x) (,fn ,x))
    `(lambda (x) (and (numberp x) (,fn x)))))

;;;; APPROXIMATE EQUALITY
;; Meant to allow equality testing for floating point variables or other
;; cases where a small difference is insignificant
(defunx ~= (n1 n2 epsilon)
  (<= (abs (- n1 n2)) epsilon))
	 
;;;; WITH-INFINITY
;; extends standard numerical comparators to use infinity also
;; This involves adjoining infinity, -infinity, and undefined to the
;; number system.
(defconstantx infinity :infinity)	; (= infinity infinity) -> nil
(defconstantx -infinity :-infinity)	; (= -infinity -infinity) -> nil
(defconstantx undefined :undefined)	; (= undefined undefined) -> nil
(defun i-binary (op a b)
  (cond
   ((eql op #'=) (or (eq a b) (and (numberp a) (numberp b) (= a b))))
   ((eql op #'<) (and (not (eq infinity a)) (not (eq -infinity b))
		      (or (eq -infinity a) (eq infinity b) (< a b))))
   ((eql op #'>) (and (not (eq -infinity a)) (not (eq infinity b))
		      (or (eq infinity a) (eq -infinity b) (> a b))))
   ((eql op #'>=) (or (i-binary #'= a b) (i-binary #'> a b)))
   ((eql op #'<=) (or (i-binary #'= a b) (i-binary #'< a b)))
   (t (error "Unknown infinite-math binary operation ~A" op))))
(defun i-nary (op args)
  (cond 
   ((eql op #'+)
    (let ((p (find infinity args)) (m (find -infinity args)))
      (cond ((and p m) undefined)	; if both +,- then undefined
	    ((or p m) (or p m))		; if only one, it dominates
	    (T (apply #'+ args)))))
   ((eql op #'-)
    (cond ((= (length args) 0)		; no arguments is an error 
	   (error "Subtraction needs at least one argument."))
	  ((= (length args) 1)		; one argument is negation
	   (let ((n (first args)))
	     (cond ((eq n infinity) -infinity)
		   ((eq n -infinity) infinity)
		   (T (apply #'- args)))))
	  (T				; 2+ arguments is subtraction
	   (with-infinity + (first args) 
			  (with-infinity - (i-nary #'+ (rest args)))))))
   ((eql op #'*)
    (let ((p (count infinity args)) (m (count -infinity args))
	  (z (count-if (num-safe zerop) args)))
      (cond ((and (> (+ p m) 0) (> z 0)) undefined) ; infinity * 0 = undefined
	    ((> (+ p m) 0)		; infinities dominate
	     (if (oddp (+ m (count-if (num-safe minusp) args)))
		 -infinity
	       infinity))
	    (T (apply #'* args)))))
   ((eql op #'/) 
    (cond ((= (length args) 0)		; no arguments is an error
	   (error "Division needs at least one argument"))
	  ((= (length args) 1)		; one argument is inversion
	   (let ((n (first args)))
	     (if (or (eq n infinity) (eq n -infinity)) 0 (apply #'/ args))))
	  (T				; 2+ arguments is division
	   (with-infinity * (first args)
			  (with-infinity / (i-nary #'* (rest args)))))))
   (t (error "Unknown infinite-math n-ary operation ~A" op))))

;; if any argument is undefined, the whole thing is undefined
(defunx with-infinity-fn (op &rest args)
  (if (find undefined args) 
      undefined
    (if (find op (list #'= #'< #'> #'>= #'<=))
	(apply #'i-binary op args)
      (i-nary op args))))
(defmacrox with-infinity (op &rest args)
  `(with-infinity-fn (function ,op) ,@args))

;; type of numbers extended to include infinity
(public inumber)
(deftype inumber ()
  `(or number (eql ,infinity) (eql ,-infinity) (eql ,undefined)))

;; runs a sampling of functions to see if it's correct
(defun check-infinite-math ()
  (let ((compressed-truth-table
	 '((u = u 5) (T > 7 6) (F = 9 4) (T > i -i) (F > i i) (T <= -i 5)
	   (u > 7 u) (T = -i -i) (T < 7 i) (F < i -i) (T = 2 2) (T = i i)
	   (u + 5 6 3 u) (9 + 4 3 2) (0 +) (i + i) (-i + 5 9 3 -i) (u + i -i)
	   (i + i i 4 5 i) (-i + 9 4 5 -i -i)
	   (u - 3 4 u 2 9) (-9 - 9) (-i - i) (i - -i) (3 - 8 4 1) (i - 5 3 -i)
	   (-i - -i 5 6 3 5 3 i) (u - i 5 4 i) (u - 5 -i i)
	   (u * i u 4 5) (1 *) (i * i) (6 * 2 3) (-i * -5 i 6 -i -i)
	   (u * -i 0 8) (u * 0 9 4 i) 
	   (0 / i) (0 / 5 -i) (u / i i) (-i / -i 4 5 3 6) (i / i 6 4 4)
	   (0 / 8 3 4 i) (1/8 / 8)))
	(subs `((,infinity i) (,-infinity -i) (,undefined u) (nil F))))
    (labels ((suball (l b) 
	       (if l 
		   (subst (first (car l)) (second (car l)) (suball (cdr l) b))
		 b)))
      (mapcar (lambda (x)
		(assert (eql (first x) (eval `(with-infinity ,@(rest x))))
		    (x) 
		  "Infinite math failed: ~A is not ~A" (rest x) (first x)))
	      (suball subs compressed-truth-table))
      :OK)))


;;;; GENERIC COMPARISON
;; Provides the 5 comparison operators as generic functions usable for
;; any objects.

(defconstant type-order '(inumber symbol character string)) ;; earlier < later
(defmethodx eqv (a b) (equalp a b))
(defmethodx gt (a b) ;; includes dealing with infinite numbers
  (let ((pa (position-if (pcf typep a) type-order))
	(pb (position-if (pcf typep b) type-order)))
    (if (and pa pb) 
	(> pa pb)
      (unless (eqv a b)
	(error "Don't know how to compare ~W > ~W" a b)))))
(defmethodx lt (a b) ; includes dealing with infinite numbers
  (let ((pa (position-if (pcf typep a) type-order))
	(pb (position-if (pcf typep b) type-order)))
    (if (and pa pb) 
	(< pa pb)
      (unless (eqv a b)
	(error "Don't know how to compare ~W < ~W" a b)))))
(defmethodx ge (a b) (or (eqv a b) (gt a b)))
(defmethodx le (a b) (or (eqv a b) (lt a b)))

(defmethod eqv ((a number) (b number)) (= a b))
(defmethod gt ((a number) (b number)) (> a b))
(defmethod lt ((a number) (b number)) (< a b))

;; Character comparators:
(defmethod eqv ((a character) (b character)) (char= a b))
(defmethod gt ((a character) (b character)) (char> a b))
(defmethod lt ((a character) (b character)) (char< a b))

;; String comparators: inherit from strings
;; redefines ge/le for significant efficiency increase
(defmethod eqv ((a string) (b string)) (string= a b))
(defmethod lt ((a string) (b string)) (tp (string< a b)))
(defmethod gt ((a string) (b string)) (tp (string> a b)))
(defmethod le ((a string) (b string)) (tp (string<= a b)))
(defmethod ge ((a string) (b string)) (tp (string>= a b)))

;; Symbol comparators: compare names
;; redefines ge/le for significant efficiency increase
(defmethod eqv ((a symbol) (b symbol)) (eqv (symbol-name a) (symbol-name b)))
(defmethod lt ((a symbol) (b symbol)) (lt (symbol-name a) (symbol-name b)))
(defmethod gt ((a symbol) (b symbol)) (gt (symbol-name a) (symbol-name b)))
(defmethod le ((a symbol) (b symbol)) (le (symbol-name a) (symbol-name b)))
(defmethod ge ((a symbol) (b symbol)) (ge (symbol-name a) (symbol-name b)))

;; Tuple comparators
;; cons comparison is essentially tuple comparison.
;; a tuple is an ordered set of numbers, lexicographically ordered.
;; A tuple is considered to have -infinity past the end of its list, for
;; purposes of comparison.
(defmethod eqv ((a cons) (b cons))
  (and (eqv (car a) (car b)) (eqv (cdr a) (cdr b))))
(defmethod gt ((a cons) (b cons))
  (or (gt (car a) (car b)) (and (eqv (car a) (car b)) (gt (cdr a) (cdr b)))))
(defmethod lt ((a cons) (b cons))
  (or (lt (car a) (car b)) (and (eqv (car a) (car b)) (lt (cdr a) (cdr b)))))
;;(defmethod eqv ((a string) (b string))

;;Sequence comparators
;; essentially just tuple-comparators
(defmethod eqv ((a sequence) (b sequence))
  (let ((la (length a)) (lb (length b)))
    (when (= la lb)
      (let ((res T))
	(dotimes (i la) (unless (eqv (elt a i) (elt b i)) (setf res nil)))
	res))))
(defmethod gt ((a sequence) (b sequence))
  (let* ((la (length a)) (lb (length b)) (stop (min la lb)))
    (labels ((iter (i)
	       (if (>= i stop)
		   (> la lb)
		 (let ((ai (elt a i)) (bi (elt b i)))
		   (or (gt ai bi) (and (eqv ai bi) (iter (1+ i))))))))
      (iter 0))))
(defmethod lt ((a sequence) (b sequence))
  (let* ((la (length a)) (lb (length b)) (stop (min la lb)))
    (labels ((iter (i)
	       (if (>= i stop)
		   (< la lb)
		 (let ((ai (elt a i)) (bi (elt b i)))
		   (or (lt ai bi) (and (eqv ai bi) (iter (1+ i))))))))
      (iter 0))))

;; anything that makes it to list/list must have a null in at least one
;; slot, since cons is more specific.
(defmethod gt ((a list) (b list))
  (not (null a)))
(defmethod lt ((a list) (b list))
  (not (null b)))
(defmethod eqv ((a list) (b list))
  (and (null b) (null a)))


(defregression test-comparison
  (let ((a #(0 0 1 1)) (b #(0 0 1 0)) (c #(0 0 1)) (d #(0 0 2)))
    (assert (and (eqv a a) (eqv b b) (not (eqv a b)) (not (eqv a c))))
    (assert (and (gt a b) (gt a c) (gt d a)))
    (assert (not (or (gt b a) (gt c a) (gt a d))))
    (assert (not (or (lt a b) (lt a c) (lt d a))))
    (assert (and (lt b a) (lt c a) (lt a d)))
    'ok))

;;;; GENERIC NUMBER FUNCTIONS
;; These are generalizations of standard functions which use generic
;; comparison.

(defmethodx maxv (a &rest b)
  (if (not b) a (let ((vb (apply #'maxv b))) (if (ge a vb) a vb))))
(defmethodx minv (a &rest b)
  (if (not b) a (let ((vb (apply #'minv b))) (if (le a vb) a vb))))

;;;; LCD
;; Calculate the least common denominator of a set of numbers
(defunx lcd (&rest n) (apply #'lcm (mapcar #'denominator n)))


;;;; COUNT-LIST
;; makes a list of the number from x through y step n.  If only x is specified,
;; then if counts from 0 to x.
(defunx count-list (x &optional y (step 1))
  (let ((l '()))
    (unless y (setf y x) (setf x 0))
    (do ((n x (incf n step)))
	((or (and (plusp step) (> n y))
	     (and (minusp step) (< n y))) (reverse l))
      (push n l))))

;;;; TICK
;; outputs over a fixed level of verbosity.  Intended for use creating
;; debugging output and progress bars.
(defvarx verbosity 0)
(defunx tick (&optional (sym #\.) (lvl 1))
  (when (>= verbosity lvl) (format T "~A" sym)))
(defunx debuglog (level string &rest args)
  (when (>= verbosity level) (apply #'format T string args)))

;;;; NAMABLE
;; Gives an easy way for objects to name themselves and their children for
;; identification
(public name-of namable)
(defclass namable () ((name :initarg :name :accessor name-of)))
(defmethod print-object ((n namable) stream)
  (if (slot-boundp n 'name)
      (format stream "~A" (slot-value n 'name))
    (call-next-method n stream)))
(defmethodx ->name (&rest components) (format nil "~{~A~^_~}" components))

;;;; WITH-TEMPS
;; makes temporary values for forms
(defun custom-gensym (x)
  (if (symbolp x) (gensym (symbol-name x)) (gensym)))
(defmacrox with-temps (decs &rest body)
  (let* ((vars (mapcar (cmp custom-gensym first) decs))
	 (saves (mapcar #'list vars (mapcar #'first decs)))
	 (news (apply #'append decs))
	 (restores (mapcan #'list (mapcar #'first decs) vars)))
    `(let ,saves
       (unwind-protect (progn (setf ,@news) ,@body)
	 (setf ,@restores)))))

;;;; UIDs
;; maps objects to gensyms
(defvar uidtable (make-hash-table))
(defunx uid (object)
  (ensure (gethash object uidtable) (gensym)))

;;;; PLACE 
;; allows capture of a place form for later expansion
(defmacrox place (x)
  (let ((sym (gensym)))
    `(make-instance 'place
       :f (function (lambda (,sym) (setf ,x ,sym)))
       :v ,x)))

(defclassx place ()
  ((setfn :initarg :f)
   (value :initarg :v)))

(defmethodx placeof (x)
  (slot-value x 'value))

(defmethodx (setf placeof) (v x)
  (funcall (slot-value x 'setfn) v))


;;;; ALIAS
;; alias provides a nickname for a function
(defmacrox alias (f1 f2)
  "Macro makes f1 an alias for f2"
  `(defmacro ,f1 (&rest args) (cons ',f2 args)))


;;;; FUNCTIONAL COMPOSITION: CMP
;; makes a lambda composing the functions together
(defmacrox cmp (&rest fns)
  (let ((v (gensym)))
    (labels ((expand (l) 
	       (if l (cons (car l) (list (expand (cdr l)))) v)))
      `(lambda (,v) ,(expand fns)))))

;;;; PARTIAL CALL MACROS
;; when called, make a lambda of the missing arguments, to be applied later
;;; TODO: make pcf/ppf handle partial-evaluation macros

;; pcf (partial call function) appends rest of arguments and calls the function
;; example: (mapcar (pcf + 2 3) '(4 5 6)) -> (9 10 11)
(defmacrox pcf (f &rest args)
  `(function (lambda (&rest completion) (apply #',f ,@args completion))))
     
;; ppf (partial pattern call function) first fills in blanks left by
;;   call-variables then appends rest of args to the end of the call
;; example: (mapcar (ppf find ?X '(1 2 3 4)) '(2 5 1 d)) -> (2 nil 1 nil)
(defunx call-variablep (x)
  (and (symbolp x) (eq (elt (symbol-name x) 0) #\?)))
(defmacrox ppf (f &rest args)
  (let ((params (remove-if-not #'call-variablep args)))
    `(lambda (,@params &rest completion) (apply #',f ,@args completion))))


;;;; SET/CALL FUNCTIONS
;; !=, !n= set the result into one of the arguments of the function.
;; That argument must be idempotent for evalation, as it must be evaluated
;; twice to run a set/call
(defmacrox != (fn arg &rest args)
  `(setf ,arg (,fn ,arg ,@args)))
(defmacrox !n= (n fn &rest args)
  `(setf ,(elt args n) (,fn ,@args)))

;;;; WRAP-WHEN
;; prefaces body with the wrapper expressions when predicate is true
(defmacrox wrap-when (predicate wrapper &rest body)
  `(if ,predicate 
       (,@wrapper ,@body)
     (progn ,@body)))


;;;; ENSURE
;; If the accessed value is default, initializes it.
(defmacrox ensure (accessor init &optional (default? #'null))
  (let ((v (gensym)))
    `(let ((,v ,accessor))
       (when (funcall ,default? ,v) (setf ,v (setf ,accessor ,init)))
       ,v)))

;;;; LIST->ALIST
;; transforms a list of n elements into n/2 pairs.
;; If n is odd, the last pair ends with a nil
(defunx list->alist (&rest pairs)
  (when pairs
    (cons (cons (first pairs) (second pairs))
	  (apply #'list->alist (cddr pairs)))))


;;;; ASSOC-OVER
;; destructively change the alist to have item in it with the requested value
;; does not guarantee that the old reference will contain the item --- the 
;; result should be setf'ed to the target.
;; the reference to alist should not be side-effecty, as will be used twice
(defunx assoc-over (alist item value &rest args &key (test #'eql))
  (let ((a (assoc item alist :test test)))
    (if a
	(progn (setf (cdr a) value) alist)
      (push (cons item value) alist))))


;;;; REF
;; Like nth, but runs on several coordinates
;; (ref '((1 2 3) (4 (5)) (6 (7 8))) 1 1 0) -> 5

(defunx ref (list &rest n)
  (if n (apply #'ref (nth (car n) list) (cdr n)) list))

;;;; SWAP-DIMENSIONS
;; take a nth order list of lists and swap the xth and yth order lists
;; The lists must be rectangular up to the dimension swapped.
;; e.g. (ref l a b c d) = (ref (swap-dimensions l 2 3) a c b d)
;; (swap-dimensions '((a b) (c d))) -> ((a c) (b d))
;; (swap-dimensions '(((a b) (c d)) ((e f) (g h))) 2) 
;;  ->  (((a e) (c g)) ((b f) (d h)))

(defunx swap-dimensions (l &optional (x 1) (y 0))
  (let ((d1 (min x y)) (d2 (max x y)))
    (cond ((or (= d1 d2) (< d1 0)) l)
	  ((> d1 0) 
	   (mapcar (lambda (s) (swap-dimensions s (1- d1) (1- d2))) l))
	  ((= d2 1) (apply #'mapcar #'list l)) ; swap dim 1 & 0
	  (T (dolist (x (append (count-list 0 (- d2 1)) ; walk up & down
				(count-list (- d2 2) 0 -1)))
	       (setf l (swap-dimensions l x (1+ x))))
	     l))))

;;;; SPLIT
;; given a key function, split a list into values of the key function
;; (effectively creates the list of equivalent classes)

(defunx split (fn list &key (test #'eqv) (key #'identity))
  (let ((alist '()))
    (dolist (x list)
      (let* ((class (funcall fn (funcall key x))) 
	     (value (assoc class alist :test test)))
	(if value (pushdown x value) (push (list class x) alist))))
    (mapcar #'cdr alist)))

;;;; REMOVE-SORTED-DUPLICATES
;; faster version of remove-duplicates
(defunx remove-sorted-duplicates (l) 
  (when l 
    (if (eqv (first l) (second l)) 
	(remove-sorted-duplicates (cdr l)) 
      (cons (car l) (remove-sorted-duplicates (cdr l))))))

;;;; MERGE-SORTED
;; Takes one or more sorted lists and produces a new list containing the 
;; union of the lists, sans dupliates.
(defunx merge-sorted-pair (x y)
  (let ((x1 (car x)) (xr (cdr x)) (y1 (car y)) (yr (cdr y)))
    (cond ((null x) y) ((null y) x)
	  ((eqv x1 y1) (cons x1 (merge-sorted-pair xr yr)))
	  ((lt x1 y1) (cons x1 (merge-sorted-pair xr y)))
	  ((gt x1 y1) (cons y1 (merge-sorted-pair x yr)))
	  (T (error "Don't know how to merge ~W and ~W" x y)))))
(defunx merge-sorted (&rest lists)
  (if (null (cdr lists))
      (remove-sorted-duplicates (car lists))
    (merge-sorted-pair (remove-sorted-duplicates (car lists)) 
		       (apply #'merge-sorted (cdr lists)))))

;;;; MERGE-ALISTS/MERGE-SORTED-ALISTS
;; Takes two alists, copies items not in each into the other, then fuses
;; items in both lists.
;; If the lists are already known to be sorted by #'lt, then they may be
;; merged more efficiently with merge-sorted-alists.
;; (merge-alists #'max '((1 . 2) (3 . 4) (7 . 8)) 
;;                    '((0 . 1) (1 . 3) (3 . 4) (5 . 6) (6 . 7) (9 . 10)))
;;   -> ((0 . 1) (1 . 3) (3 . 4) (5 . 6) (6 . 7) (7 . 8) (9 . 10))
(defunx merge-sorted-alist-pair (fuse x y)
  (let ((xid (caar x)) (xval (cdar x)) (yid (caar y)) (yval (cdar y)))
    (cond ((null x) y)
	  ((null y) x)
	  ((eqv xid yid) (cons (cons xid (funcall fuse xval yval)) 
			       (merge-sorted-alist-pair fuse (cdr x) (cdr y))))
	  ((lt xid yid) (cons (cons xid xval) 
			      (merge-sorted-alist-pair fuse (cdr x) y)))
	  ((gt xid yid) (cons (cons yid yval) 
			      (merge-sorted-alist-pair fuse x (cdr y))))
	  (T (error "Don't know how to merge ~W and ~W" xid yid)))))
(defunx merge-sorted-alists (fuse x &rest y)
  (if (not y) x 
    (merge-sorted-alist-pair fuse x (apply #'merge-sorted-alists fuse y))))
(defunx merge-alists (fuse x &rest y)
  (apply #'merge-sorted-alists fuse (mapcar (ppf sort ?x #'lt) (cons x y))))
(defunx sorted-alist-insert (fuse item alist)
  (cond ((null alist) (list item))	; null: make new list
	((eqv (car item) (caar alist))	; equal: fuse items
	 (cons (cons (car item) (funcall fuse (cdr item) (cdar alist)))
	       (cdr alist)))
	((lt (car item) (caar alist)) (cons item alist)) ; less: insert
	(T (cons (car alist) (sorted-alist-insert fuse item (cdr alist))))))

;;;; ENDPUSH
;; inverse of push --- adds to end, not start
(defmacrox endpush (item place)
  `(if ,place
       (progn (rplacd (last ,place) (list ,item)) ,place)
     (setf ,place (list ,item))))

;;;; PUSHDOWN
;; if the length is longer than the list, add to the end
(defmacrox pushdown (item place &optional (n 1))
  (let ((v (gensym)))
    `(let* ((,v (nthcdr ,n ,place)))
       (if ,v
	   (progn (rplacd ,v (cons (car ,v) (cdr ,v))) 
		  (rplaca ,v ,item) ,place)
	 (endpush ,item ,place)))))

(excl:without-package-locks
 (define-setf-expander nthcdr (n list &environment env)
   (multiple-value-bind (temps vals stores store-form access)
       (get-setf-expansion list env)
     (declare (ignore stores store-form))
     (let ((v (gensym)) (ntemp (gensym)) (store (gensym)))
       (values (cons ntemp temps)
	       (cons n vals)
	       (list store)
	       `(if ,store
		    (let ((,v (nthcdr ,ntemp ,access)))
		      (rplaca ,v (car ,store)) (rplacd ,v (cdr ,store)))
		  (if (> ,ntemp 0)
		      (rplacd (nthcdr (1- ,ntemp) ,access) nil)
		    (setf ,access nil)))
	       `(nthcdr ,ntemp ,access)
	       )))))

;;;; ENDPOP
;; inverse of pop --- removes from end, not start
(defmacrox endpop (place)
  (let ((v (gensym)) (r (gensym)))
    `(let ((,v (last ,place 2)))
       (cond ((null ,v) nil)
	     ((= (length ,v) 1) (setf ,place nil) (car ,v))
	     (T (let ((,r (second ,v))) (rplacd ,v nil) ,r))))))

;;;; POPDOWN
;; if the length is longer than the list, nothing will be removed
(defmacrox popdown (place &optional (n 1))
  `(pop (nthcdr ,n ,place)))
      
    
;;;; LIST-HEAD
;; returns the first k elements of a list
(defunx list-head (l k)
  (subseq l 0 k))

;;;; INDEX-OP
;; operates on an array, only in the locations with 1s
;; index-op gives sequence values; index-op* gives index and values; 
;; index-op** gives index only
(defunx index-op (fn index &rest seqs)	; uses a bit-array index
  (do ((i (position 1 index) (position 1 index :start (1+ i))))
      ((not i) nil)
    (apply fn (mapcar (lambda (s) (aref s i)) seqs))))
(defunx index-op* (fn index &rest seqs)	; uses a bit-array index
  (do ((i (position 1 index) (position 1 index :start (1+ i))))
      ((not i) nil)
    (apply fn i (mapcar (lambda (s) (aref s i)) seqs))))
(defunx index-op** (fn index)		; uses a bit-array index
  (do ((i (position 1 index) (position 1 index :start (1+ i))))
      ((not i) nil)
    (funcall fn i)))

;;;; BIT-ARRAY OPS
(defunx bit-array (size)
    (make-array size :initial-element 0 :element-type 'bit))
(defmacrox clear-bits (bits) `(bit-xor ,bits ,bits ,bits))
(defconstantx true 1) (defunx true (bit) (= bit 1))
(defconstantx false 0) (defunx false (bit) (= bit 0))

;;;; SET/MATRIX OPERATORS
;; make an array initialized with the expression init, evaluated each time
(defmacrox make-iset (init size &rest args)
  (let ((a (gensym)) (i (gensym)) (s (gensym)))
    `(let* ((,s ,size) (,a (make-array ,s ,@args)))
       (dotimes (,i ,s) (setf (aref ,a ,i) ,init)) ,a)))
;; Set group operator, assumes that all sets are arrays of the same size
(defmacrox with-set (sets &rest body)
  (let* ((i (or (find-if #'atom sets) (gensym)))
	 (vars (mapcar #'first (remove-if #'atom sets)))
	 (vecs (mapcar #'second (remove-if #'atom sets)))
	 (gens (mapcar (cmp gensym symbol-name) vars)) (idx (first gens)))
    `(let ,(mapcar (lambda (g v) (list g v)) gens vecs)
       (dotimes (,i (length ,idx))
	 (symbol-macrolet ,(mapcar (lambda (v d) `(,v (aref ,d ,i))) vars gens)
	   ,@body)))))
;; with-array is like with-set, but for n-dim arrays.  Index not provided.
(defmacrox with-array (arrays &rest body)
  (let* ((i (gensym))
	 (vars (mapcar #'first arrays)) (vecs (mapcar #'second arrays))
	 (gens (mapcar (cmp gensym symbol-name) vars)) (idx (first gens)))
    `(let ,(mapcar (lambda (g v) (list g v)) gens vecs)
       (dotimes (,i (array-total-size ,idx))
	 (symbol-macrolet ,(mapcar (lambda (v d) `(,v (row-major-aref ,d ,i)))
				   vars gens)
	   ,@body)))))
(defmacrox with-mat (row col cross &rest crossop)
  (with-matpat row col cross '? crossop T))
(defmacrox with-rmaj (row col cross wrap &rest crossop) ; row-major matrix op
  (with-matpat row col cross wrap crossop T))
(defmacrox with-cmaj (row col cross wrap &rest crossop) ; row-major matrix op
  (with-matpat row col cross wrap crossop nil))
(defun with-matpat (row col cross pat crossop rmaj)
  (let* ((oa (gensym)) (ov (first (if rmaj row col))) (o (gensym)) 
	 (ia (gensym)) (iv (first (if rmaj col row))) (i (gensym)) 
	 (os (second (if rmaj row col))) (is (second (if rmaj col row)))
	 (xa (gensym)) (xref `(aref ,xa ,(if rmaj o i) ,(if rmaj i o)))
	 (iloop `(dotimes (,i (length ,ia))
		   (symbol-macrolet ((,iv (aref ,ia ,i))(,(first cross) ,xref))
		     ,@crossop))))
    (labels ((got (c) (if (atom c) (call-variablep c)
			(or (got (car c)) (got (cdr c))))))
      `(let ((,oa ,os) (,ia ,is) (,xa ,(second cross)))
	 (dotimes (,o (length ,oa))
	   (symbol-macrolet ((,ov (aref ,oa ,o)))
	     ,(if (got pat)
		  (subst-if iloop #'call-variablep pat)
		`(progn ,pat ,iloop))))))))

;; Do the sets contain the same items in the same cardinality?
(defunx multiset-eqv (setA setB &optional (test #'eqv))
  (let ((remB setB) (failed nil))
    (dolist (a setA)			; remove elements of A one at a time
      (if (find a remB)
	  (setf remB (remove a remB :count 1))
	(setf failed t)))
    (not (or failed remB))))		; anything left in B?

(defregression test-setops
  (let* ((q -1) (x (make-iset (incf q) 4)) (y (copy #(4 5 6 7))) 
	 (z (make-array '(4 4) :initial-element 3)) (j '()) (k '()))
    (assert (eqv x #(0 1 2 3)))
    (with-set (i (y y)) (push (list i y) k))
    (assert (eqv k '((3 7) (2 6) (1 5) (0 4))))
    (with-set ((y x) (x y)) (incf y x))
    (assert (eqv x #(4 6 8 10)))
    (with-mat (r x) (c y) (x z) (setf x (mod (+ r c) 4)))
    (assert (eqv z #2A((0 1 2 3) (2 3 0 1) (0 1 2 3) (2 3 0 1))))
    (with-rmaj (r x) (c y) (x z) (setf r (decf q)) (push c j) (setf x (+ r c)))
    (assert (eqv x #(2 1 0 -1)))
    (assert (eqv z #2A((6 7 8 9) (5 6 7 8) (4 5 6 7) (3 4 5 6))))
    (assert (eqv j '(7 6 5 4 7 6 5 4 7 6 5 4 7 6 5 4)))
    (with-cmaj (r x) (c y) (x z) (decf c) (incf r) (setf x (+ r c)))
    (assert (eqv y #(3 4 5 6)))
    (assert (eqv x #(6 5 4 3)))
    (assert (eqv z #2A((6 8 10 12) (5 7 9 11) (4 6 8 10) (3 5 7 9))))
    (with-rmaj (r x) (c y) (x z) (when (evenp r) ?) (setf x 0))
    (assert (eqv z #2A((0 0 0 0) (5 7 9 11) (0 0 0 0) (3 5 7 9))))
    (setf k nil) (with-array ((y z)) (if (zerop y) (incf y) (push y k)))
    (assert (eqv k '(9 7 5 3 11 9 7 5)))
    (assert (eqv z #2A((1 1 1 1) (5 7 9 11) (1 1 1 1) (3 5 7 9))))
    (assert (multiset-eqv '(a a b c c c) '(c a c b c a)))
    (assert (not (multiset-eqv '(a a b c c c) '(a b c))))
    (assert (not (multiset-eqv '(a b c) '(a a b c c c))))
    (assert (not (multiset-eqv '(z y x) '(y z))))
    'ok))

;;;; ITERATOR
;; a general mechanism for walking an ordered data structure from bottom to top
;; contains: source, ref, read-ref-fn advance-ref-fn
;; read & advance both take arguments (source ref)
;; advance is never called once ref has become nil
(defunx make-iterator (source ref read-ref-fn advance-ref-fn)
  (list source ref read-ref-fn advance-ref-fn))
(defunx next-value (iterator)		; returns value, ref
  (with-accessors ((source first) (ref second) (read third) (advance fourth)) 
      iterator
    (let ((r ref) (v (when ref (funcall read source ref))))
      (setf ref (and ref (funcall advance source ref)))
      (values v r))))
(defunx more-values? (iterator)		; returns a boolean
  (with-accessors ((ref second)) iterator (tp ref)))
(defunx mapiter (fn &rest iterators)	; applies (fn value) for each entry
  (if (find-if-not #'more-values? iterators)
      nil
    (let ((v (apply fn (mapcar #'next-value iterators))))
      (cons v (apply #'mapiter fn iterators)))))
(defmacrox doiter ((v iterator &optional (ref (gensym))) result &rest code)
  (let ((iter (gensym)))
    `(let ((,iter ,iterator))
       (block nil
	 (tagbody 
	  iterloop
	   (multiple-value-bind (,v ,ref) (next-value ,iter)
	     (declare (ignorable ,v))
	     (if ,ref
		 (progn ,@code (go iterloop))
	       (return ,result))))))))

;;  (let ((iter (gensym)))
;;    `(let ((,iter ,iterator))
;;       (loop 
;;	   for (,v ,ref) = (multiple-value-list (next-value ,iter))
;;	   while ,ref
;;	   do ,@code
;;	   finally (return ,result)))))
					;(declare (ignorable ,v))

;; a couple of standard iterator types
(defmethodx iterator ((source list))
  (labels ((car2 (x y) (declare (ignore x)) (car y))
	   (cdr2 (x y) (declare (ignore x)) (cdr y)))
    (make-iterator source source #'car2 #'cdr2)))
(defmethodx iterator ((source vector))
  (labels ((inc-aref (a n) (unless (= n (1- (length a))) (1+ n))))
    (make-iterator source (when (> (length source) 0) 0) #'aref #'inc-aref)))
;; n-dimensional arrays
(defmethodx iterator ((source array))
  (labels ((inc-aref (a n) (unless (= n (1- (array-total-size a))) (1+ n))))
    (make-iterator source (when (> (array-total-size source) 0) 0) 
		   #'row-major-aref #'inc-aref)))
;; recursively iterates through the source to fixed depth N
;; The reference is a triple: src/ref/reader
;; The source is a list of walkers, starting with the [] general
(defmethodx nested-iterator (src n)
  (labels ((fill-out (src)
	     (if (>= (length src) n)
		 src
	       (fill-out (cons (iterator (next-value (first src))) src))))
	   (strip-empties (src)
	     (and src (if (more-values? (first src)) src 
			(strip-empties (cdr src)))))
	   (ensure-full-ref (src)	; returns a ref
	     (if (more-values? (first src))
		 (copy (first src))
	       (let ((new (strip-empties src)))
		 (when new		; if we can go on
		   (map-into src #'identity (fill-out new))
		   (ensure-full-ref src))))))
    (let ((src (fill-out (list (iterator src)))))
      (make-iterator
       src
       (ensure-full-ref src)
       (lambda (src ref) 
	 (declare (ignore src))
	 (values (funcall (third ref) (first ref) (second ref)) ref))
       (lambda (src ref)
	 (declare (ignore ref))
	 (next-value (first src)) (ensure-full-ref src))))))

;; filter an iterator so that only things where test returns true are returned
(defunx filter-iterator (iter test)
  (labels ((next-ref (src)		; scoot forward to the next good one
	     (multiple-value-bind (v r) (next-value src)
	       (when r (if (funcall test v) r (next-ref src))))))
    (make-iterator 
     iter (next-ref iter)
     (lambda (src ref) (funcall (third src) (first src) ref))
     (lambda (src ref) (declare (ignore ref)) (next-ref src)))))
;; filter-iterator* adds the reference to the test-fn call   
(defunx filter-iterator* (iter test)
  (labels ((next-ref (src)		; scoot forward to the next good one
	     (multiple-value-bind (v r) (next-value src)
	       (when r (if (funcall test r v) r (next-ref src))))))
    (make-iterator 
     iter (next-ref iter)
     (lambda (src ref) (funcall (third src) (first src) ref))
     (lambda (src ref) (declare (ignore ref)) (next-ref src)))))

(defregression test-iterator
  (let ((l '(1 2 3 4 5)) (a #(5 4 3 2 1)) (o '()) (n 0)
	(x '(#((1) () () #(2 3)) #(#((4) 5 6)) #(#(7 8) #(9 0) #())))
	(z '(:A 2 3 :C 4 :G))
	(k #2A((1 2 3) (4 5 6) (7 8 9))))
    (assert (equal (mapiter #'+ (iterator l) (iterator a)) '(6 6 6 6 6)))
    (assert (equal (mapiter #'1- (iterator l)) '(0 1 2 3 4)))
    (assert (equal (doiter (v (iterator a)) o (push v o)) '(1 2 3 4 5)))
    (assert (equal (mapiter #'1- (iterator k)) '(0 1 2 3 4 5 6 7 8)))
    (assert (equal (doiter (v (iterator nil)) o (incf v) (push 'z o)) 
		   '(1 2 3 4 5)))
    (assert (equal (doiter (v (iterator l) r) n (incf n (length r)) (incf n v))
		   30))
    (assert (equal (doiter (v (iterator l) r) n (incf n (length r))) 45))
    (assert (equal (mapiter #'identity (nested-iterator x 3))
		   '(1 2 3 (4) 5 6 7 8 9 0)))
    (assert (equal (mapiter #'identity 
			    (nested-iterator '((()) (() ()) (() (3))) 3))
		   '(3)))
    (assert (equal (mapiter #'identity (filter-iterator (iterator a) #'oddp))
		   '(5 3 1)))
    (assert (equal (mapiter #'1+ (filter-iterator (iterator a) #'evenp))
		   '(5 3)))
    (assert (equal (mapiter #'1+ (filter-iterator* (iterator a) #'>))
		   '(3 2)))
    (assert (equal (mapiter #'identity (filter-iterator 
					(filter-iterator 
					 (iterator z) #'numberp) #'evenp))
		   '(2 4)))
    (assert (equal (mapiter #'identity (filter-iterator 
					(nested-iterator 
					 #((1 2) (3 4) (5 6)) 2) #'evenp))
		    '(2 4 6)))
    (assert (equal (mapiter #'identity (filter-iterator 
					(nested-iterator 
					 #((1 2) (3 4) (5 6)) 2) #'oddp))
		    '(1 3 5)))
    'ok))


;;;; QUEUE
;;; maintains head & tail pointers
;;; Structure: (length head . tail)
(defunx make-queue (&optional (initial-size 0) initial-element)
  (let ((l (make-list initial-size :initial-element initial-element)))
    (cons initial-size (cons l (last l)))))

(defunx popq (q)
  (if (emptyq q)
      nil
    (let ((v (pop (headq q))))
      (decf (sizeq q))
      (when (emptyq q) (setf (tailq q) nil))
      v)))

(defunx pushq (q v)
  (let ((elt (list v)))
    (if (emptyq q) (setf (headq q) elt) (rplacd (tailq q) elt))
    (incf (sizeq q))
    (setf (tailq q) elt)))
;; pushes only if not already in queue (expensive)
(defunx idempushq (q v)
  (unless (find v (headq q)) (pushq q v)))

(defunx delq (q v)
  (let* ((first (member-if-not (pcf eql v) (headq q))) (last first) (count 1))
    (do ((link (cdr first) (cdr link))) ((not link))
      (unless (eql v (car link))
	(incf count) (rplacd last link) (setf last link)))
    (when last (rplacd last nil))
    (setf (headq q) first (tailq q) last (sizeq q) (if first count 0))))

(public sizeq emptyq peekq)
(alias sizeq car)
(alias peekq caadr)
(alias headq cadr)
(alias tailq cddr)
(defunx emptyq (q) (= (sizeq q) 0))

(defregression test-queue
  (let ((q (make-queue)))
    (assert (emptyq q))
    (pushq q 1)(pushq q 2)(pushq q 3)(pushq q 4)
    (assert (= (sizeq q) 4)) (assert (not (emptyq q)))
    (assert (= (popq q) 1))(assert (= (popq q) 2))(assert (= (sizeq q) 2))
    (pushq q 5)(pushq q 4)(pushq q 4)(delq q 3)
    (assert (= (sizeq q) 4))(assert (eqv (headq q) '(4 5 4 4)))
    (pushq q 7)(delq q 5)(delq q 7) 
    (assert (= (sizeq q) 3))
    (delq q 4)
    (assert (= (sizeq q) 0)) (assert (not (popq q))) (assert (emptyq q))
    'ok))
;;;; TRANSFORM-TREE
;; For purposes of this function, trees consist of proper and dotted lists.
;; For a proper list, transform-tree is applied to all cars, which are then
;; joined using the branch function.  For a dotted list it is the same except
;; that the last cdr is used as well.  When use-dot is true, the first 
;; argument to branch is a boolean indicating whether the list was dotted.
;; For non-lists, leaf is called.
;; e.g. (transform-tree '(9 (4 16 1) 16 25) #'sqrt #'list) 
;;   -> (3.0 (2.0 4.0 1.0) 4.0 5.0)

;; undot is used by transform-tree; it returns two values: a proper-list and
;; whether it used to be dotted.
(defun undot (l)
  (if (consp l)
      (multiple-value-bind (sub dotted) (undot (cdr l))
	(values (cons (car l) sub) dotted))
    (if (null l)
	(values l nil)
      (values (list l) t))))

(defunx transform-tree (tree leaf branch &optional use-dot)
  (if (listp tree)
      (multiple-value-bind (l dotted) (undot tree)
	(let ((xformed (mapcar (ppf transform-tree ?X leaf branch use-dot) l)))
	  (apply branch (if use-dot (cons dotted xformed) xformed))))
    (funcall leaf tree)))

;;;; FLATTEN
;; turns a tree into a flat list, traversing cars before cdrs
;; e.g. (flatten '(a (b c (d e) f) (((g h)) i j))) -> (A B C D E F G H I J)
;; This will also turn a pair into a list of two elements 
;; e.g. (flatten '(a . ((b . c) . (d . e)))) -> (A B C D E)
(defunx flatten (l)
  (transform-tree l #'list #'append))

;;;; FOLD
;; fold is similar to reduce, but takes a default value, which is
;; modifies by merging elements of the list into it, rather than
;; combining pairs of list elements The fold function is of form: (fn
;; running-result new-element) -> running-result
;; Fold runs left to right.
;; Fold also runs on arrays of arbitrary dimension, in row-major order.
(defunx fold (fn seq base)
  (let ((result base))
    (if (arrayp seq)
	(with-array ((x seq)) (setf result (funcall fn result x)))
      (dolist (x seq)
	(setf result (funcall fn result x))))
    result))


;;;; AMAPCAR
;; Like mapcar, but operates on the associated elements in an alist
;; The function can only be a function of one element
(defunx amapcar (fn alist)
  (when alist
    (let ((v (first alist)))
      (cons (cons (car v) (funcall fn (cdr v))) (amapcar fn (cdr alist))))))

;;;; MAPSET/MAPSETC
;; A converse to the standard map function, mapset
;; takes each element of list as a set of arguments for the
;; function, and returns a list of results of application.
;; e.g. (mapset #'+ '((1 2) (3 4 5) (6) (7 8 9))) -> (3 12 6 24)
(defunx mapset (fn list)
  (when (consp list)
    (cons (apply fn (car list))
	  (mapset fn (cdr list)))))
;; mapsetc is just like mapset, but concatenates the results together.
;; e.g. (mapsetc #'list '((1 2) (3) (4 5))) -> (1 2 3 4 5)
(defunx mapsetc (fn list)
  (apply #'nconc (mapset fn list)))


;;;; PAIRWISE
;; Call f on all combinations of arguments, for side-effects.
(defunx pairwise (f &rest arglists)
  "Applies f to all combinations of arguments, for side effects."
  (if arglists
      (dolist (x (car arglists))
	(apply #'pairwise 
	       #'(lambda (&rest y) (apply f x y)) 
	       (cdr arglists)))
    (apply f '())))

;;;; COMBINATIONS
;; Returns all combinations of elements in the lists, drawing one from
;; the first list, one from the second, etc. Thus (combinations '(1 2)
;; '(3) (4 5 6)) will have six combinations, starting with '(1 3 4)
;; and ending with '(2 3 6)
(defunx combinations (&rest lists)
  (if lists
      (let ((sub (apply #'combinations (cdr lists))))
	(mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) sub))
		(car lists)))
    '(())))

;;;; SUBSETS
;; Returns a list of all 2^k subsets of a length k set (including the null
;; set and the whole set)
;; Elements are in the same order as they appear in the set.
(defunx subsets (set)
  (if set
      (let ((subs (subsets (cdr set))))
	(append (mapcar (lambda (x) (cons (first set) x)) subs)
		subs))
    (list '())))


;;;; RANDOM-PERMUTATION
;; generates a new list in a random order from the one created
(defunx random-permutation (list)
  (mapcar #'cdr
	  (sort (mapcar (lambda (x) (cons (random 1.0) x)) list)
		#'< :key #'car)))
;; shuffles the elements of array randomly
(defunx permute-array (array)
  (map-into array (lambda (x) (cons (random 1.0) x)) array)
  (sort array #'< :key #'car)
  (map-into array #'cdr array))
;; generic for all sequences
(public randomize)
(defmethod randomize ((l list)) (random-permutation l))
(defmethod randomize ((a array)) (permute-array a))

;;;; CHOOSE-RANDOM
;; selects a random element of a sequence
(defunx choose-random (seq &optional weightfn)
  (let ((index (if weightfn (choose-weighted (mapcar weightfn seq))
		 (random (length seq)))))
    (elt seq index)))
;; k-random variance returns a list of non-repeated elements
(defunx choose-k-random (seq k)
  (if (= k 0) '()
    (let ((rest (choose-k-random seq (- k 1))))
      (do ((x (choose-random seq) (choose-random seq)))
	  ((not (find x rest)) (cons x rest))))))
;; destructive version good for non-sparse choices
(defunx random-subset (seq k) (subseq (randomize seq) 0 k))
;; return the index of the chosen weight
(defunx choose-weighted (weights)
  (let* ((sum 0.0) (cum (map 'list (lambda (w) (incf sum w)) weights))
	 (choice (random sum)))
    (position-if (pcf <= choice) cum)))

;;;; FIND-MAX/FIND-MIN
;; selects the extreme from a sequence, breaking ties with order
(defunx find-max (seq measure)
  (let ((best nil) (val nil))
    (doiter (x (iterator seq)) best
      (let ((m (funcall measure x)))
	(when (or (not val) (> m val)) (setf best x val m))))))
(defunx find-min (seq measure)
  (let ((best nil) (val nil))
    (doiter (x (iterator seq)) best
      (let ((m (funcall measure x)))
	(when (or (not val) (< m val)) (setf best x val m))))))

;;;; PAIRWISE-MAP
;; List the results of calling f on all combinations of arguments.
(defunx pairwise-map (f &rest arglists)
  (mapcar (lambda (x) (apply f x)) (apply #'combinations arglists)))
;; test with: (pairwise-map #'* '(1 2) '(2 3) '(3 4))
;; should yield: '(6 8 9 12 12 16 18 24)

;;;; PAIRWISE-FIND
;; Lazily construct all combinations of arguments, searching for the first
;; where predicate p returns true. 
;; Returns two values: predicate result, arguments
(defunx pairwise-find (f &rest arglists)
  (let ((answer nil))
    (labels ((pfhelper (args lists)
	       (if lists
		   (find-if (lambda (x) (pfhelper (cons x args) (cdr lists))) 
			    (car lists))
		 (let ((value (apply f args)))
		   (when value (setf answer (cons value args)))))))
      (pfhelper '() (reverse arglists))
      (values (car answer) (cdr answer)))))
;; tests: (pairwise-find #'equal '(1 2 3 4 5) '(9 6 3 5 7)) -> T, '(3 3)
;;        (pairwise-find #'equal '(1 2 8 4 5) '(9 6 3 0 7)) -> nil, nil 

;;;; DOHASH
;; dohash is syntactic sugar on maphash to act like dolist
(defmacrox dohash ((key value table) &rest body)
  `(maphash #'(lambda (,key ,value) ,@body) ,table))

(defunx hash->list (ht)
  (let ((out '()))
    (dohash (k v ht) (push (list k v) out))
    out))

(defunx hash-keys (ht)
  (let ((out '()))
    (dohash (k v ht) (push k out))
    out))

(defunx hash-values (ht)
  (let ((out '()))
    (dohash (k v ht) (push v out))
    out))

;;;; RANDOM-HASH-KEY
;; Chooses a random key from a hash table (nil if no keys)
(defunx random-hash-key (table &optional default)
  (if (plusp (hash-table-count table))
      (let ((n 0) (which (random (hash-table-count table))) (res nil))
	(maphash (lambda (k v) 
		   (declare (ignore v)) 
		   (when (= n which) (setf res k)) (incf n))
		 table)
	res)
    default))

;;;; HASHVAL
;; extends sxhash to arbitrary objects
(defmethodx hashval (obj) (sxhash obj))
(defmethod hashval ((c cons)) 
  (let ((i (hashval (car c))) (j (hashval (cdr c)))) 
    (logxor i (- j i))))
(defmethod hashval ((a array))
  (let ((res 0))
    (dotimes (i (length a)) (!= logxor res (hashval (* (1+ i) (aref a i)))))
    res))

;;;; CONSTANT
;; Constant makes a function which, given an arbitrary set of arguments,
;; always returns v
(defunx constant (v)
  (lambda (&rest x) (declare (ignore x)) v))

;;;; KEYWORD-FOR
;; a little piece of syntactic sugar, this function takes
;; a symbol or string and converts it into a keyword symbol
(defunx keyword-for (x) (intern x :keyword))

;;;; CONTAINS
;; like find, but returns t or nil instead of the item.
;; This is particularly useful for searching for nil in lists
(defunx listcontains (item l &key (test #'eql) key)
  (if (consp l)
      (let ((v (if key (funcall key (car l)) (car l))))
	(or (funcall test item v)
	    (listcontains item (cdr l) :test test :key key)))
    nil))


;;;; SSETF
;; Safe setf: makes a copy of the thing about to be set, to protect it
;; from damage
(defmacrox ssetf (place value)
  `(setf ,place (copy ,value)))

;;;; AUGMENTATION
;; brute-force aspect-oriented programming: given a collection of
;; aspects, create extensions of class with every possible combination
;; of present and absent augmentations

;; table of legal extensions
(defvar augmentation-table '())
(defun add-to-aug-table (entry)
  (let ((pre-existing (assoc (car entry) augmentation-table)))
    (if pre-existing
	(rplacd pre-existing
		(append 
		 (set-difference (cdr entry) (cdr pre-existing) :key #'car)
		 (cdr pre-existing)))
      (push entry augmentation-table))))

;; (declare-augmentations A B C D) produces 7 new classes:
;; B-A, C-A, D-A, B-C-A, B-D-A, C-D-A, B-C-D-A
;; The augmentation-table will be extended with the following 12 transitions:
;; (A (B . B-A) (C . C-A) (D . D-A))
;; (B-A (C . B-C-A) (D . B-D-A))
;; (C-A (B . B-C-A) (D . C-D-A))
;; (D-A (B . B-D-A) (C . C-D-A))
;; (B-C-A (D . B-C-D-A))
;; (B-D-A (C . B-C-D-A))
;; (C-D-A (B . B-C-D-A))
;; A set of k augmentations can be viewed as a k-cube, and thus will 
;; produce 2^k-1 classes and k*2^(k-1) transitions.
(defmacro create-augmentations (class exported &rest aspects)
  (let ((subs (subsets (mapcar (lambda (x) (if (consp x) (second x) x)) 
			       aspects)))
	(names (mapcar (lambda (x) (if (consp x) (reverse x) (list x x))) 
		       aspects))
	(deffer (if exported 'defclassx 'defclass)))
    (labels ((name-of (x) (second (assoc x names)))
	     (make-name (args) 
	       (intern (format nil "~{~A-~}~A" (mapcar #'name-of args) class)))
	     (extensions (set) 
	       (mapcar
		(lambda (ext) (cons (car (set-difference ext set))
				    (make-name ext)))
		(remove-if-not 
		 (lambda (s) (and (= (length s) (1+ (length set))) 
				  (subsetp set s)))
		 subs))))
      (cons 
       'progn
       (mapcan
	(lambda (subset)
	  (let* ((name (make-name subset))
		 (cdef `(,deffer ,name (,@subset ,class) ()))
		 (tabdef `(add-to-aug-table '(,name ,@(extensions subset)))))
	    (cond ((not subset) (list tabdef))
		  ((equal subset aspects) (list cdef))
		  (T (list cdef tabdef)))))
	subs)))))
;; The interfaces just select whether or not the created symbols are exported
(defmacrox declare-augmentations (class &rest aspects)
  `(create-augmentations ,class nil ,@aspects))
(defmacrox declare-augmentationsx (class &rest aspects)
  `(create-augmentations ,class T ,@aspects))
  
(defunx augment (object aspect &rest args)
  (unless (typep object aspect)		; unless it's already got the augment
    (let* ((subtable (cdr (assoc (type-of object) augmentation-table)))
	   (augmented (cdr (assoc aspect subtable))))
      (assert augmented (object aspect) 
	"Cannot augment ~A with ~A" (type-of object) aspect)
      (apply #'change-class object augmented args))))

(defclass augtesta () ()) 
(defclass augtestb () ())
(defclass augtestc () ()) 
(defclass augtestd () ())
(defmethod augtestf ((x augtesta)) (list 1))
(defmethod augtestf ((x augtestb)) (cons 2 (call-next-method x)))
(defmethod augtestf ((x augtestc)) (cons 3 (call-next-method x)))
(defmethod augtestf ((x augtestd)) (cons 4 (call-next-method x)))
(declare-augmentations augtesta (b augtestb) (c augtestc) augtestd)
(defregression test-augmentation
  (let ((x (make-instance 'augtesta))
	(y (make-instance 'c-augtesta))
	(z (make-instance 'augtesta)))
    (assert (eqv (augtestf x) '(1)))
    (assert (eq (type-of x) 'augtesta))
    (assert (eqv (augtestf y) '(3 1)))
    (augment x 'augtestb)		; basic augmentation
    (assert (eqv (augtestf x) '(2 1)))
    (assert (eqv (augtestf y) '(3 1)))
    (assert (eq (type-of y) 'c-augtesta))
    (augment y 'augtestc) (augment x 'augtestb) ; idempotence
    (assert (eqv (augtestf x) '(2 1)))
    (assert (eq (type-of x) 'b-augtesta))
    (assert (eqv (augtestf y) '(3 1)))
    (augment x 'augtestc) (augment y 'augtestb) ; multiple augmentation
    (assert (eqv (augtestf x) '(2 3 1)))
    (assert (eq (type-of x) 'b-c-augtesta))
    (assert (eqv (augtestf y) '(2 3 1)))
    (augment x 'augtestd) (augment y 'augtestd) ; all the way
    (augment z 'augtestd) (augment z 'augtestc) (augment z 'augtestb)
    (assert (eqv (augtestf x) '(2 3 4 1)))
    (assert (eqv (augtestf y) '(2 3 4 1)))
    (assert (eqv (augtestf z) '(2 3 4 1)))
    (assert (eq (type-of x) 'b-c-augtestd-augtesta))
    (assert (eq (type-of y) 'b-c-augtestd-augtesta))
    (assert (eq (type-of z) 'b-c-augtestd-augtesta))
    'ok))


;;;; TERMINATE
;; Clean way to kill all hanging state, etc.
(defvar terminate-actions '())
(defmacrox on-terminate (id action)
  (eval-when (:load-toplevel :execute)
    (let ((v1 (gensym)) (v2 (gensym)))
      `(let* ((,v1 ',id) (,v2 (assoc ,v1 terminate-actions)))
	 (if ,v2
	     (setf (cdr ,v2) ',action)
	   (setf terminate-actions (acons ,v1 ',action terminate-actions)))
	 (format nil "Termination condition ~A" ,v1)))))
(defun remove-termination (id)
  (setf terminate-actions (delete id terminate-actions)))
(defunx terminate (&rest ids)
  (dolist (act terminate-actions)
    (handler-case 
	(when (or (not ids) (member (car act) ids))
	  (eval (cdr act)))
      (condition () (format t "Error in termination ~A~%" (car act))))))
(eval-when (:load-toplevel :execute)
  (terminate))



(defunx compile-and-load (file &key (verbose *compile-verbose*) 
			       (print *compile-print*))
  (excl:compile-file-if-needed file :verbose verbose :print print)
  (load file :verbose verbose :print print))

;;;; IMPORT MACROS
;; known bootstrapping bug: the utils file will always reload itself
;; the first time it is referenced
(declaim (special sys::*utils-directory*))
(defvar path `("." ,sys::*utils-directory*))
(defun ensure-path (p)
  (unless (find p utils::path :test #'eqv) (push p utils::path)))
(defclass pfrec ()
  ((id :initarg :id :reader pfrec-id)
   (time :initarg :time)
   (compiled :initarg :cf)
   (source :initarg :sf)))
(defvar package-files '())		; list of pfrecs
(defconstant compiled-suffixes '(".fasl"))
(defconstant source-suffixes '(".lisp"))

;; periods in a package name mean sub-modules. These can be interpreted as
;; either sub-directories or as periods. So package "foo.bar.baz" might refer
;; to either "$PATH/foo.bar.baz", "$PATH/foo/bar.baz" or "$PATH/foo/bar/baz"
(defun get-file-stems (package)
  (labels ((rest-stems (path stem)
	     (let ((n (position #\. stem)))
	       (cons (format nil "~A~A" path stem)
		     (when n
		       (rest-stems (format nil "~A~A/" path (subseq stem 0 n))
				   (subseq stem (+ n 1))))))))
    (rest-stems "" (string-downcase package))))

(defunx find-type-in-path (package types)
  (labels ((goodrefp (path prefix suffix)
	     (let ((f (format nil "~A/~A~A" path prefix suffix)))
	       (when (probe-file f) f))))
    (let* ((stems (get-file-stems package)))
      (pairwise-find #'goodrefp path stems types))))

(defunx find-compiled-in-path (package)
  (find-type-in-path package compiled-suffixes))

(defunx find-source-in-path (package)
  (find-type-in-path package source-suffixes))

(defunx refresh-from-path (package)
  (let ((rec (find package package-files :key #'pfrec-id :test #'equal)))
    (if rec
	(with-slots (time compiled source) rec
	  (when (and source
		     (< (file-write-date compiled)
			(file-write-date source)))
	    (format t "Source ~A is newer than compiled version. ~
                       Recompiling..." source)
	    (setf compiled (compile-file source)))
	  (when (< time (file-write-date compiled))
	    (format t "Compiled ~A is newer than loaded version. ~
                       Reloading..." compiled)
	    (load compiled)
	    (setf time (file-write-date compiled))))
      (let ((cf (find-compiled-in-path package))
	    (sf (find-source-in-path package)))
	(when (and sf (or (not cf)
			  (< (file-write-date cf) (file-write-date sf))))
	  (format t "Source ~A is newer than compiled version. ~
                     Recompiling..." sf)
	  (setf cf (compile-file sf)))
	(cond (cf
	       (load cf)
	       (push (make-instance 'pfrec :id package 
				    :time (file-write-date cf) :cf cf :sf sf)
		     package-files))
	      (t
	       (error "Could not find package ~A" package)))))))

(defmacrox make-module (name (&rest uses) &rest args)
  (let* ((usestrs (mapcar #'symbol-name uses))
	 (namestr (symbol-name name)))
    `(evalwhen (:c :l :x)
       (dolist (p ',usestrs) (refresh-from-path p))
       (defpackage ,namestr
	 (:use "COMMON-LISP" ,@usestrs)
	 ,@args))))

(defmacrox assume-package (&rest names)
  (let ((namestrs (mapcar #'symbol-name names))
	(output '()))
    (dolist (n namestrs) 
      (when (not (find-package n)) (push `(defpackage ,n) output)))
    `(progn ,@output)))

;; (assume-public-in (:bob :x :y) (:jim :z)) 
;; will make sure that bob:x, bob:y, and jim:z exist
(defmacrox assume-public-in (&rest sympacks)
  (let ((packs (mapcar #'first sympacks))
	(x (gensym)) (y (gensym)))
    `(progn 
       (assume-package ,@packs)
       (dolist (,x ',sympacks)
	 (dolist (,y (cdr ,x))
	   (export (intern (symbol-name ,y) (car ,x)) (car ,x)))))))

(defmacrox import-module (&rest packages)
  (let ((pnames (mapcar #'symbol-name packages)))
    `(evalwhen (:c :l :x) 
       (dolist (p ',pnames) (refresh-from-path p))
       (use-package ',pnames))))

(defun forget-modules () (setf package-files nil))
(defunx loaded-modules () 
  (mapcar #'pfrec-id package-files))



;; A regression-test on small, random utils
(defregression util-test
  (let ((a 3) (b 4) (c 2))		; SET/CALL FUNCTIONS
    (assert (eql (!= + a b c) 9))
    (assert (eql (!n= 2 + a b c) 15))
    (assert (and (eql a 9) (eql b 4) (eql c 15))))
  (assert (eqv (split (ppf mod ? 3) '(9 3 6 2 5 1 0 4 3)) ; SPLIT
	       '((4 1) (5 2) (3 0 6 3 9))))
  (assert (eqv (split #'oddp '((9 3) (6 2) (5 1) (0 4)) :key #'second)
	       '(((0 4) (6 2)) ((5 1) (9 3)))))
  (assert (eqv (let ((x '()))		; INDEX-OP*
		 (index-op* (lambda (i) (push i x)) #*001110001) x)
	       '(8 4 3 2)))
  (assert (eql (lcd 1/3 4/8 2/3 7) 6))	; LCD
  (let ((x #(0 1 nil 3 4)))		; ENSURE
    (assert (eqv (ensure (aref x 2) 9) 9))
    (assert (eqv (ensure (aref x 2) 8) 9))
    (assert (eqv (ensure (aref x 3) 7 #'oddp) 7))
    (assert (eqv (ensure (aref x 4) 6) 4))
    (assert (eqv x #(0 1 9 7 4))))
  (assert (eql (find-max #(3 6 4 8 1 2 9 3) #'identity) 9)) ; FIND-MIN/MAX
  (assert (eqv (find-min '((6) (1 3) (2 4 5) (7 4)) #'length) '(6)))
  (let ((tab (make-hash-table)) (na 0) (nb 0) (nc 0)) ; RANDOM-HASH-KEY
    (assert (eqv (random-hash-key tab 'foo) 'foo))
    (setf (gethash 'a tab) 1 (gethash 'b tab) 2 (gethash 'c tab) 3)
    (dotimes (i 1000) 
      (let ((k (random-hash-key tab)))
	(cond ((eq k 'a) (incf na)) ((eq k 'b) (incf nb)) ((eq k 'c) (incf nc)) 
	      (T (error "RANDOM-HASH-KEY returns bad key ~A" k)))))
    (assert (and (> na 250) (> nb 250) (> nc 250))))
  'ok)

