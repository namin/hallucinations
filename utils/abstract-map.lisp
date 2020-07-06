(make-module :abstract-map (:utils))
(in-package :abstract-map)

;; general map class; cannot contain nil elements
;; examples of maps: alists, hash-tables, sparse-array, functions
(public abstract-map)
(defclass abstract-map () () )
(defmethodx clear ((m abstract-map)) (nodefault :clear)) ; return blanked table
(defmethodx contains? ((m abstract-map) ref)
  (declare (ignore ref)) (nodefault :contains)) ; t when ref in domain
(defmethodx mref ((m abstract-map) ref)
  (declare (ignore ref)) (nodefault :mref)) ; return referred elt
(defmethodx (setf mref) (v (m abstract-map) ref)
  (declare (ignore v ref)) (nodefault "SETF MREF"))
(defmethodx size ((m abstract-map)) (nodefault :size)) ; # of domain elements
(defmethodx capacity ((m abstract-map)) (nodefault :capacity)) ; possible size
(defmethodx copy ((m abstract-map)) (nodefault :copy)) ; shallow copy of map
(defmethodx iterator ((m abstract-map)) (nodefault :iterator))
(defmethodx restrict ((m abstract-map) mask)
  ;; return new map w. domain intersected with a bit-vector mask
  (declare (ignore mask)) (nodefault :restrict)) 

;; A macro for guaranteeing that a ref is occupied in a map.
;; If it's currently unoccupied, init is evaluated to initialize it.
(defmacrox ensure (map ref init)
  (let ((r (gensym "REF")) (m (gensym "MAP")))
    `(let ((,r ,ref) (,m ,map))
       (or (mref ,m ,r) (setf (mref ,m ,r) ,init)))))

;; resizables are abstract-maps which notify users when their capacity changes
(defclassx resizable ()
  ((observers :initform '())))
;; idempotent addition of observer
;; Hook is (fn observer resizable new-capacity)
(defmethodx observe ((r resizable) observer &optional (hook #'observe-resize))
  (with-slots ((o observers)) r 
    ;;(format t "OBSERVE ~A~:_ with ~A~%" r observer)
    (pushnew (cons hook observer) o :test #'equal)))
(defmethodx resize ((r resizable) new-cap) 
  (declare (ignore new-cap)) (nodefault :resize))
(defmethod resize :after ((r resizable) new-cap)
  (with-slots (observers) r
    ;;(format t "RESIZED ~A to ~A~:_ TELL ~A~%" r new-cap observers)
    (dolist (l observers) (funcall (car l) (cdr l) r new-cap))
    r))
;; dummy method for observing resizes
(defmethodx observe-resize (observer resizable new-capacity)
  (declare (ignorable observer resizable new-capacity)) t)

;; nil is a special type of abstract map that acts as a /dev/null
;; You can dump anything into it anywhere but can't get it back out
(defmethod clear ((m null)) nil)
(defmethod contains? ((m null) ref) (declare (ignore ref)) nil)
(defmethod mref ((m null) ref) (declare (ignore ref)) nil)
(defmethod (setf mref) (v (m null) ref) (declare (ignore ref)) v)
(defmethod size ((m null)) 0)
(defmethod capacity ((m null)) 0)
(defmethod copy ((m null)) nil)
;; iterator inherited from list
(defmethod restrict ((m null) mask) (declare (ignore mask)) nil)

;; null-map is like nil, except that it includes a size and default value
(defclassx null-map (abstract-map resizable)
  ((default :initarg :default :initform nil)
   (cap :initarg :cap :initform 0)))
(defmethod print-object ((m null-map) stream)
  (with-slots (default cap) m
    (format stream "~:@<NULL-MAP SIZE=~A CONTENT=~A~:>" cap default)))
(defmethod clear ((m null-map)) nil)
(defmethod contains? ((m null-map) ref) (declare (ignore ref)) nil)
(defmethod mref ((m null-map) ref) 
  (declare (ignore ref)) (slot-value m 'default))
(defmethod (setf mref) (v (m null-map) ref) 
  (error "Cannot write ~A to ~A in ~A" v ref m))
(defmethod size ((m null-map)) 0)
(defmethod capacity ((m null-map)) (slot-value m 'cap))
(defmethod copy ((m null-map)) m)
(defmethod interator ((m null-map)) (iterator nil))
(defmethod restrict ((m null-map) mask) (declare (ignore mask)) m)
(defmethod resize ((m null-map) new-cap) (setf (slot-value m 'cap) new-cap))


;; hash tables can be used as abstract maps
(defmethod clear ((m hash-table)) (clrhash m))
(defmethod contains? ((m hash-table) ref) (nth-value 1 (gethash ref m)))
(defmethod mref ((m hash-table) ref) (gethash ref m))
(defmethod (setf mref) (v (m hash-table) ref) (setf (gethash ref m) v))
(defmethod size ((m hash-table)) (hash-table-count m))
(defmethod capacity ((m hash-table)) (hash-table-size m)) ; non-useful
;; copy, iterator, and restrict will be ignored until I first need them

;; 1-dim arrays can be used as abstract maps
(defmethod clear ((m array)) (fill m nil))
(defmethod clear ((m bit-vector)) (bit-xor m m m)) ; cheaper for bit-vectors
(defmethod contains? ((m array) ref) 
  (and (> ref 0) (< ref (length m)) (tp (aref m ref))))
(defmethod mref ((m array) ref) (aref m ref))
(defmethod (setf mref) (v (m array) ref) (setf (aref m ref) v))
(defmethod size ((m array)) (length m))
(defmethod capacity ((m array)) (length m))
;; copy & iterator already defined in utils
(defmethod restrict ((m array) mask)
  (map 'array (lambda (mask val) (when mask val)) mask m))
(defmethod restrict ((m bit-vector) mask) ; cheaper for bit-vectors
  (bit-and m mask))


;; alists can be used as abstract maps
(defmethod contains? ((m list) ref) (tp (assoc ref m)))
(defmethod mref ((m list) ref) (cdr (assoc ref m)))
;; setfing can't be done on alists because null is a legitimate alist.
(defmethod size ((m list)) (length m))
(defmethod capacity ((m list)) 1e20)	; eh, that's enough for now
(defmethod restrict ((m list) mask)
  (mapcar (lambda (mask val) (cons (car val) (if mask (cdr val)))) mask m))
;; copy & iterator already defined in utils
;; sans access to the place, mutators clear and (setf mref) are left undefined

;;; Regression test
(defclass testr (resizable) ())
(defmethod resize ((r testr) new-cap) new-cap) ; scratch method
(defregression test-abstract-map
  (let* ((x '((a . b) (c . d) (e . f) (g . h) (i . j) (k . l)))
	 (y #(1 2 3 4 5 6))
	 (slot 0) (z (make-instance 'testr)) 
	 (nm (make-instance 'null-map :cap 50 :default :q))
	 (fn (lambda (x y z) (declare (ignore x y)) (incf slot z)))
	 (mask '(T nil nil T T nil)))
    (assert (and (= (size x) 6) (= (size y) 6) (= (capacity y) 6))) ; ALIST
    (assert (and (contains? x 'e) (not (contains? x 'd))))
    (assert (and (eq (mref x 'a) 'b) (eq (mref x 'g) 'h))) 
    (assert (and (mref (restrict x mask) 'g) 
		 (not (mref (restrict x mask) 'c))))
    (assert (and (contains? y 5) (not (contains? y 7)))) ; ARRAY
    (assert (and (eq (mref y 3) 4) (eq (mref y 5) 6))) 
    (assert (and (mref (restrict y mask) 4) 
		 (not (mref (restrict y mask) 5))))
    (setf (mref y 2) 7)
    (assert (and (contains? y 2) (eq (mref y 2) 7)))
    (setf (mref nil 495) :foo)		; NIL
    (assert (eq (mref nil 495) nil))
    (assert (eq (mref nil 3495783) nil))
    (assert (eq (mref nm 5) :q))	; NULL-MAP
    (assert (and (= (size nm) 0) (= (capacity nm) 50))) 
    (observe z T fn) (observe z T fn)	; RESIZING
    (resize z 100)
    (assert (= slot 100))
    (setf slot (make-hash-table))	; HASH TABLE
    (ensure slot 3 'q) (ensure slot 3 'x) (ensure slot 6 'r) ; ensure macro
    (assert (null (contains? slot 7)))
    (assert (contains? slot 3))
    (assert (eq (mref slot 3) 'q))
    (assert (= (size slot) 2))
    (clear slot)
    (assert (= (size slot) 0)) (assert (null (contains? slot 3)))
    'ok))

