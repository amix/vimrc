;;; swank-mrepl.lisp
;;
;; Licence: public domain

(in-package :swank)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((api '(
	       *emacs-connection*
	       channel 
	       channel-id
	       define-channel-method
	       defslimefun 
	       dcase
	       log-event
	       process-requests
	       send-to-remote-channel
	       use-threads-p
	       wait-for-event
	       with-bindings
	       with-connection
	       with-top-level-restart
	       with-slime-interrupts
	       )))
    (eval `(defpackage #:swank-api
	     (:use)
	     (:import-from #:swank . ,api)
	     (:export . ,api)))))

(defpackage :swank-mrepl
  (:use :cl :swank-api)
  (:export #:create-mrepl))

(in-package :swank-mrepl)

(defclass listener-channel (channel)
  ((remote :initarg :remote)
   (env :initarg :env)
   (mode :initform :eval)
   (tag :initform nil)))

(defun package-prompt (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
	  (cons (package-name package) (package-nicknames package))))

(defslimefun create-mrepl (remote)
  (let* ((pkg *package*)
         (conn *emacs-connection*)
	 (thread (if (use-threads-p)
		     (spawn-listener-thread conn)
		     nil))
         (ch (make-instance 'listener-channel :remote remote :thread thread)))
    (setf (slot-value ch 'env) (initial-listener-env ch))
    (when thread
      (swank/backend:send thread `(:serve-channel ,ch)))
    (list (channel-id ch)
	  (swank/backend:thread-id (or thread (swank/backend:current-thread)))
	  (package-name pkg)
	  (package-prompt pkg))))

(defun initial-listener-env (listener)
  `((*package* . ,*package*)
    (*standard-output* . ,(make-listener-output-stream listener))
    (*standard-input* . ,(make-listener-input-stream listener))))

(defun spawn-listener-thread (connection)
  (swank/backend:spawn 
   (lambda ()
     (with-connection (connection)
       (dcase (swank/backend:receive)
	 ((:serve-channel c)
	  (loop
	   (with-top-level-restart (connection (drop-unprocessed-events c))
	     (process-requests nil)))))))
   :name "mrepl thread"))

(defun drop-unprocessed-events (channel)
  (with-slots (mode) channel
    (let ((old-mode mode))
      (setf mode :drop)
      (unwind-protect
	   (process-requests t)
	(setf mode old-mode)))
    (send-prompt channel)))

(define-channel-method :process ((c listener-channel) string)
  (log-event ":process ~s~%" string)
  (with-slots (mode remote) c
    (ecase mode
      (:eval (mrepl-eval c string))
      (:read (mrepl-read c string))
      (:drop))))

(defun mrepl-eval (channel string)
  (with-slots (remote env) channel
    (let ((aborted t))
      (with-bindings env
	(unwind-protect 
	     (let ((result (with-slime-interrupts (read-eval-print string))))
	       (send-to-remote-channel remote `(:write-result ,result))
	       (setq aborted nil))
	  (setf env (loop for (sym) in env
			  collect (cons sym (symbol-value sym))))
	  (cond (aborted
		 (send-to-remote-channel remote `(:evaluation-aborted)))
		(t
		 (send-prompt channel))))))))

(defun send-prompt (channel)
  (with-slots (env remote) channel
    (let ((pkg (or (cdr (assoc '*package* env)) *package*))
	  (out (cdr (assoc '*standard-output* env)))
	  (in (cdr (assoc '*standard-input* env))))
      (when out (force-output out))
      (when in (clear-input in))
      (send-to-remote-channel remote `(:prompt ,(package-name pkg)
					       ,(package-prompt pkg))))))
  
(defun mrepl-read (channel string)
  (with-slots (tag) channel
    (assert tag)
    (throw tag string)))

(defun read-eval-print (string)
  (with-input-from-string (in string)
    (setq / ())
    (loop
       (let* ((form (read in nil in)))
	 (cond ((eq form in) (return))
	       (t (setq / (multiple-value-list (eval (setq + form))))))))
    (force-output)
    (if /
	(format nil "~{~s~%~}" /) 
	"; No values")))

(defun make-listener-output-stream (channel)
  (let ((remote (slot-value channel 'remote)))
    (swank/backend:make-output-stream 
     (lambda (string)
       (send-to-remote-channel remote `(:write-string ,string))))))

(defun make-listener-input-stream (channel)
  (swank/backend:make-input-stream (lambda () (read-input channel))))

(defun set-mode (channel new-mode)
  (with-slots (mode remote) channel
    (unless (eq mode new-mode)
      (send-to-remote-channel remote `(:set-read-mode ,new-mode)))
    (setf mode new-mode)))

(defun read-input (channel)
  (with-slots (mode tag remote) channel
    (force-output)
    (let ((old-mode mode)
	  (old-tag tag))
      (setf tag (cons nil nil))
      (set-mode channel :read)
      (unwind-protect 
	   (catch tag (process-requests nil))
	(setf tag old-tag)
	(set-mode channel old-mode)))))

(provide :swank-mrepl)
