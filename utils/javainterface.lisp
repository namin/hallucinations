(make-module :javainterface (:utils))
(in-package :javainterface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process)
  (require :jlinker)
  (use-package :javatools.jlinker)
  (use-package :multiprocessing))

;;;; CONNECTION MAINTAINENCE
;; these routines start a thread in the background which constantly attempts
;; to maintain a connection with a java process via jlinker

(defvar connected nil)
(defvar active t)
(defvar connection-process nil)

(defmethod maintain-connection ()
  (when (not connected)			; if not connected, try to get linked up
    (jlinker-init :lisp :lisp-file nil :lisp-port 4857 :timeout 10))
  (setf connected (jlinker-query t))
  (when (and active connected) (process-sleep 2)) ; if connected, don't recheck for a bit
  (if active 
      (maintain-connection)		; ... and loop
    (jlinker-end)))

(defmethodx j.disconnect ()
  (setf active nil))

(defmethodx j.connect ()
  (when (not (and active connection-process (process-active-p connection-process)))
    (setf active t)
    (when connection-process
      (process-kill connection-process))
    (setf connection-process
      (process-run-function "connector" #'maintain-connection))))

(j.connect)				; start the connection process

(defmethodx j.toString (x)
  (format nil "~A" x))
(defmethodx j.toString ((x tran-struct))
  (if (jarrayp x)
      (j.toString (ja->l x))
    (jcall "toString" x)))
(defmethod print-object :around ((x tran-struct) stream)
  (if *print-escape* 
      (call-next-method x stream)
    (format stream "~A" (j.toString x))))

(defmethod ja->l ((x tran-struct))
  (let ((r '()))
    (dotimes (n (jarrayp x 0) (nreverse r))
      (push (jarray-ref x n) r))))
  
(defmethod jv->l ((x tran-struct))
  (let ((r '()))
    (dotimes (n (jcall "size" x) (nreverse r))
      (push (jcall "get" x n) r))))

(defunx j.println (&rest x)
  (let ((xstr (format nil "~{~A~}" (mapcar #'j.toString x))))
    (jcall (jmethod "java.io.PrintStream" "println" "java.lang.String") 
	   (jfield "java.lang.System" "out")
	   xstr)))

(defun test () (j.println "Connection is live"))

;;;; CLASS RIPPING
;; these routines let a java class be sucked wholesale into lisp.
;; ripping a class imports:
;; 1. constructors
;; 2. public fields (static & instance)
;; 3. public methods (static & instance)
;;
;; New version: hierarchical ripping copies only the declared methods for a class and
;; walks its inheritance hierarchy for all others. Ripping a class implicitly rips all
;; its parents. The default name of a ripped class is j[java class name]
;;
;; Also, classes are ripped into a lisp package corresponding to the java package.
;; then use-package on those packages acts like an import call in java
;; To prevent name conflicts, and package not starting with "java" gets "j." prepended to its name
;; Thus, the full name of Vector is: java.util:Vector and utils.Symbol is "j.utils:Symbol"
;;
;; TODO: test to see if class casting works correctly

(defmethod staticp ((x tran-struct))
  (jstatic "isStatic" "java.lang.reflect.Modifier" (jcall "getModifiers" x)))

(defmethod publicp ((x tran-struct))
  (jstatic "isPublic" "java.lang.reflect.Modifier" (jcall "getModifiers" x)))

(defunx flush-java-imports ()
  (clrhash jimport-classtable))

(defclass jimport ()
  ((jtype :allocation :class)
   (ltype :allocation :class)
   (javainitargs :initarg :java)))	; for passthrough of java ctors

(defclass jimport-class ()
  ((jtype :initarg :type)
   (jsuper :initarg :super)
   (ltype :initarg :ltype)
   (ctable :initform nil)
   (mtable :initform (make-hash-table))))
(defvar jimport-classtable (make-hash-table :test #'equal))
(defunx jclass-data (name) (gethash name jimport-classtable))
(defunx (setf jclass-data) (v name) (setf (gethash name jimport-classtable) v))

(defmethod ensure-package (name)
  (when (not (find-package name)) (make-package name :use '(:javainterface :common-lisp))))

(defmacrox import-java (jname &optional name force)
  `(import-java-wrapper ,jname ',name ,force))

(defun mkjavaname (x)
  (if (equal "JAVA" (subseq x 0 4)) x (format nil "J.~A" x)))

(defun import-java-wrapper (jname name force)
  (when (not connected) (error "Cannot import while not connected."))
  (if (and (not force) (jclass-data jname)) ; check if it's already imported
      (format t "~A is already imported.~%" jname) 
    (let* ((cl (jclass jname))
	   (super (jcall "getSuperclass" cl))
	   (superid nil)
	   (rawpack (string-upcase (jcall "getName" (jcall "getPackage" cl))))
	   (package (intern (mkjavaname rawpack) :keyword)))
      (when super 
	(setf superid (import-java-wrapper (jcall "getName" super) nil nil))) ; import superclass
      (ensure-package package)		; ensure that the package exists
      (when (not name)			; default name is "j[java class name]"
	(setf name (intern
		    (string-upcase 
		     (format nil "j~A" (subseq (jcall "getName" cl) 
					       (+ 1 (length rawpack)))))
		    package)))
      (format t "Importing java class ~A as ~A" jname name)
      (import-body jname name superid package)
      (format t "~%")))			; import itself 
  (slot-value (jclass-data jname) 'ltype)) ; return the name of the created class

(defun import-body (jname name super package)
  (format t ".")
  (let* ((cl (jclass jname))
	 ;;(methods (jcall "getDeclaredMethods" cl))
	 ;;(methods (remove-if-not #'publicp (ja->l (jcall "getDeclaredMethods" cl))))
	 (fields (ja->l (jcall "getFields" cl)))
	 (ctors (ja->l (jcall "getConstructors" cl))))
    (format t ".")
    (labels ((nameof (x) (jcall "getName" x))
	     (xnameof (x) (intern (format nil "~A.~A" name (string-upcase (nameof x))) package))
	     (fentry (x) `(,(nameof x) :accessor ,(xnameof x))))
      (format t ".")
      (let ((i-methods (jv->l (jstatic "getClassInstancePublics" "utils.PatientConnector" jname)))
	    (s-methods (jv->l (jstatic "getClassStaticPublics" "utils.PatientConnector" jname)))
	    (i-fields (mapcar #'fentry (remove-if #'staticp fields)))
	    (s-fields (mapcar #'fentry (remove-if-not #'staticp fields))))
	;;(format t "Import Class: ~W ~W ~W ~W~%" jname name super package)
	;;(format t "~W~%" methods)
	(format t ".")
	(setf (jclass-data name) (make-instance 'jimport-class :super super :type jname :ltype name))
	(setf (jclass-data jname) (jclass-data name)) ; leave a copy under the java name
	(eval 
	 `(def-java-class (,name ,jname) (,(if super super 'jimport)) ,s-fields ,i-fields 
			  ((jtype :initform ,jname :allocation :class)
			   (ltype :initform ',name :allocation :class))))
	(export (cons name (mapcar #'xnameof fields)) package)
	(format t ".")
	(dolist (x ctors)
	  (def-gm-ctor name x package))
	(format t ".")
	(dolist (x s-methods)
	  (def-gm-method name x package t))
	(format t ".")
	(dolist (x i-methods)
	  (def-gm-method name x package nil))))))

(defclass coerced (jimport)
  ((ref :initarg :ref :accessor coerced-ref)
   (jtype :initarg :type :accessor coerced-type :allocation :instance)))

(defmacrox jcast (class object)
  (let ((v (cond ((stringp class) class)
		 ((dist-object-p class) class)
		 ((subtypep class 'jimport) (slot-value (jclass-data class) 'jtype))
		 (t (error "Don't know how to cast ~A as a ~A" object class)))))
    `(make-instance 'coerced :ref ,object :type ,v)))

(defmethod print-object ((x coerced) stream)
  (format stream "(~A) ~A" (coerced-type x) (coerced-ref x)))

;; given a java.lang.reflect.Constructor, goes and DTRT to make the call
(defmethod def-gm-ctor (name ctor package)
  (with-slots (ctable) (jclass-data name)
    (labels ((nameof (x) (jcall "getName" x)))
      (let* ((cname (intern (format nil "NEW-~A" (symbol-name name)) package))
	     (ctname (intern (format nil "~A~A" cname (length ctable)) package))
	     (args (mapcar #'nameof (ja->l (jcall "getParameterTypes" ctor)))))
	(eval 
	 `(progn
	    (defmethod ,cname (&rest args) (table-dispatch ',name ',cname args t))
	    (def-java-constructor ,ctname (,name ,@args))))
	(export cname package)
	(pushnew (list cname (symbol-function ctname) args) ctable :test #'eqv)))))

;; given a java.lang.reflect.Method, creates either a static or instance method table entry
(defmethod def-gm-method (name method package static)
  (let ((def-fn (if static 'def-java-static 'def-java-method)))
    (labels ((nameof (x) (jcall "getName" x)))
      (let* ((jname (nameof method))
	     (fname (intern (format nil "~A" (string-upcase jname))
			    :keyword))
	     (ftname (intern (format nil "~A-~A~A" (symbol-name name) fname 
				     (length (get-fn-list name fname)))
			     package))
	     (args (mapcar #'nameof (ja->l (jcall "getParameterTypes" method)))))
	;;(format t "Import Method: ~A ~A ~A ~A ~A~%" name method static fname ftname)
	(eval 
	 `(progn
	    (defmacro ,name (fn obj &rest args)
	      (let ((name ',name))
		`(table-dispatch ',name (intern ',fn :keyword) (list ,obj ,@args) nil)))
	    (,def-fn (,ftname ,jname) (,name ,@args))))
	;;(export fname package)
	(push (list fname (symbol-function ftname) args) (get-fn-list name fname))))))

(defun get-fn-list (class name)
  (gethash name (slot-value (jclass-data class) 'mtable)))
(defun (setf get-fn-list) (v class name)
  (setf (gethash name (slot-value (jclass-data class) 'mtable)) v))
(defun get-fn-list-all (class name)	; including inherited
  (if class 
      (append (get-fn-list class name) 
	      (get-fn-list-all (slot-value (jclass-data class) 'jsuper) name))
    '()))

(defmethod jprimitive-p (class)
  (member class '("boolean" "byte" "short" "int" "long" "single" "double" "java.lang.String") 
	  :test #'equal))

(defun argmatch (name args entry)
  ;;(format t "matching! ~A ~A ~A~%" name args entry)
  (labels ((incompat (obj arg)
	     ;;(format t "incompat: ~A ~A~%" obj arg)
	     (cond ((jprimitive-p arg) nil)
		   ((typep obj 'coerced)
		    (not (eqv (coerced-type obj) arg)))
		   ((typep obj 'jimport) 
		    (not (or (eqv (slot-value obj 'jtype) arg)
			     (jcall "isAssignableFrom" (jclass arg) (jclass (jclass-of obj))))))
		   ((eqv arg "com.franz.jlinker.TranStruct") nil)
		   ((typep obj 'tran-struct) (not (eqv (jclass-of obj) arg)))
		   (t (not (and (jclass-of obj)
				(jcall "isAssignableFrom" (jclass arg) (jclass (jclass-of obj)))))))))
    (let ((fname (first entry))
	  (fn (second entry))
	  (fargs (third entry)))
      (and (eqv name fname)
	   (= (length args) (length fargs))
	   (not (find t (mapcar #'incompat args fargs)))
	   fn))))

(defun table-dispatch (class fn args ctor)
  (let ((fnlist (if ctor (slot-value (jclass-data class) 'ctable)
		  (get-fn-list-all class fn))))
    (when (not fnlist) (error (format nil "Function ~A not defined for class ~A" fn class)))
    ;;(format t "~W ~W ~W ~W~%" class fn args ctor)
    (apply (table-lookup fnlist fn (if ctor args (cdr args)))
	   args)))

(defun table-lookup (fnlist fn args)
  (let ((m (find-if (ppf argmatch fn args) fnlist)))
    (if m (second m)
      (error (format nil "No matching function for ~A ~A." fn args)))))

;; how to subclass a lisp class to a java class
(public defjclass)
(defmacro defjclass (name jparent lparents &rest args)
  (let ((jctor (intern (format nil "NEW-~A" (symbol-name jparent)) (symbol-package jparent))))
    `(progn
       (defclass ,name ,(cons jparent lparents) ,@args)
       (defmethod make-instance ((x (eql ',name)) &rest initargs &key java &allow-other-keys)
	 (let ((jobj (apply (function ,jctor) java)))
	   (apply #'change-class jobj ',name initargs))))))
