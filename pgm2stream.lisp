;; Turns a "plain" ASCII format PGM file into an array of Black &
;; White 1D percepts Each row is a percept.  This does not handle
;; comments: they must be deleted from the PGM.
(defun pgm-to-1D-vision (file)
  (with-open-file (in file :direction :input)
    (let* ((magicnum (read in))		; should be 'P2'
	   (width (read in))
	   (height (read in))
	   (maxgrey (read in))
	   (out (make-array height)))
      (assert (eq magicnum 'P2))
      (dotimes (y height)
	(let ((row (make-array width)))
	  (dotimes (x width)
	    (setf (aref row x) (if (> (read in) (/ maxgrey 2)) 0 1)))
	  (setf (aref out y) row)))
      out)))

(defun pgm-to-row-stream (file)
  (cons -1 (pgm-to-1D-vision file)))

(defun seek-row (row-stream row)
  (rplaca row-stream (1- row))
  (next-row row-stream))

(defun more-rows (row-stream)
  (< (car row-stream) (1- (length (cdr row-stream)))))  

;; return a row, or nil when done
(defun next-row (row-stream)
  (when (more-rows row-stream)
    (aref (cdr row-stream) (incf (car row-stream)))))

#|
(let ((rs (pgm-to-row-stream "sample.pgm")))
  (labels ((loop (i) (if (next-row rs) (loop (1+ i)) i)))
    (loop 0)))
|#