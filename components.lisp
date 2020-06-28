;; Finding graph components is non-trivial.
;; Here is an inefficient function for doing so, but good enough for
;; our small-numbers graphs
(defun bigraph-components (a b test)
  (labels ((inv-test (x y) (funcall test y x))
	   (nbrs (elt y test) 
	     (remove-if-not (lambda (x) (funcall test elt x)) y))
	   (set-nbrs (set y test)
	     (fold #'union (mapcar (ppf nbrs ? y test) set) nil))
	   (find-component (sub1 sub2 set1 set2 test inv-test)
	     (let ((expanded (set-nbrs sub1 set2 test)))
	       (if (set-difference expanded sub2)
		   (reverse
		    (find-component expanded sub1 set2 set1 inv-test test))
		 (list sub1 sub2))))
	   (subset-components (set1 set2)
	     (if set1
		 (let ((component (find-component (list (car set1)) nil
						  set1 set2 test #'inv-test)))
		   (cons component
			 (subset-components
			  (set-difference set1 (first component))
			  (set-difference set2 (second component)))))
	       (mapcar (lambda (e) (list nil (list e))) set2))))
    (subset-components a b)))

(defregression test-bigraph-components
  (let* ((edges '((B P) (B Q) (C P) (D Q) (E R) (E S) (F S) (G S) (H S)
		  (I T) (I U) (J U) (J V) (K V) (K W) (L W) (L X) (M X) (M Y)))
	 (set1  '(A B C D E F G H   I J K L M))
	 (set2  '(N O P Q R S      T U V W X Y))
	 (test (lambda (e1 e2) (find (list e1 e2) edges :test #'equal)))
	 (same-elts (lambda (e1 e2)
		      (not (or (set-exclusive-or (first e1) (first e2))
			       (set-exclusive-or (second e1) (second e2)))))))
    (assert (not (set-exclusive-or (bigraph-components set1 set2 test)
				   '(((A) nil) (nil (N)) (nil (O))
				     ((B C D) (P Q)) ((E F G H) (R S))
				     ((I J K L M) (T U V W X Y)))
				   :test same-elts)))
    'ok))
		      
