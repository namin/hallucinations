(make-module :javaconnector (:utils))
(in-package javaconnector)

(evalwhen (:c :l :x)
  (require :sock)
  (require :process)
  (use-package :socket)
  (use-package :multiprocessing))

(defvarx jc-sock (make-socket :connect :passive :local-port 5867
			      :reuse-address t :nodelay t))
(defvarx jcs nil)
(defvar jcs-lock (make-process-lock))
(defconstant ack (code-char 6))
(defvar connection-process nil)


(on-terminate 
 javaconnector
 (progn 
   (when connection-process 
     (process-kill connection-process)
     (setf connection-process nil))
   (kill-connection)))

(defunx jc-send (str &rest args) 
  (handler-case 
      (when jcs
	(apply #'format jcs str args))
    (stream-error () (kill-connection))))
(defunx jc-flush ()
  (handler-case
      (when jcs
	(force-output jcs))
    (stream-error () (kill-connection))))
(defunx jc-recv ()
  (handler-case
      (with-timeout (10 (make-condition 'stream-error))
	(do ((done nil) (result nil))
	    (done result)
	  (when (not (eq (peek-char t jcs) ack))
	    (setf result (read jcs))
	    (setf done t))))
    (stream-error () (kill-connection))))
(defunx jc-live ()
  (tp jcs))

(defunx kill-connection ()
  (with-process-lock (jcs-lock)
    (when jcs 
      (shutdown jcs :direction :input)
      (shutdown jcs :direction :output)
      (format t "Java connection down~%")
      (setf jcs nil))))

(defmethod maintain-connection ()
  (do () nil
    (when (not jcs)
      (with-process-lock (jcs-lock)
	(progn				; ignore-errors
	  (setf jcs (accept-connection jc-sock))
	  (format t "Java connection up~%"))))
    (when jcs (write-char ack jcs) (write-char #\Space jcs) (jc-flush))
    (process-sleep 0.5)))
  
(defunx restart-connection ()
  (when (or (not connection-process) (not (process-active-p connection-process)))
    (setf connection-process
      (process-run-function "connector" #'maintain-connection))))

(restart-connection)