;;; swank-clipboard.lisp --- Object clipboard
;;
;; Written by Helmut Eller in 2008.
;; License: Public Domain

(defpackage :swank-clipboard
  (:use :cl)
  (:import-from :swank :defslimefun :with-buffer-syntax :dcase)
  (:export :add :delete-entry :entries :entry-to-ref :ref))

(in-package :swank-clipboard)

(defstruct clipboard entries (counter 0))

(defvar *clipboard* (make-clipboard))

(defslimefun add (datum)
  (let ((value (dcase datum
		 ((:string string package)
		  (with-buffer-syntax (package)
		    (eval (read-from-string string))))
		 ((:inspector part) 
		  (swank:inspector-nth-part part))
		 ((:sldb frame var)
		  (swank/backend:frame-var-value frame var)))))
    (clipboard-add value)
    (format nil "Added: ~a"
	    (entry-to-string (1- (length (clipboard-entries *clipboard*)))))))

(defslimefun entries ()
  (loop for (ref . value) in (clipboard-entries *clipboard*)
	collect `(,ref . ,(to-line value))))

(defslimefun delete-entry (entry)
  (let ((msg (format nil "Deleted: ~a" (entry-to-string entry))))
    (clipboard-delete-entry entry)
    msg))

(defslimefun entry-to-ref (entry)
  (destructuring-bind (ref . value) (clipboard-entry entry)
    (list ref (to-line value 5))))

(defun clipboard-add (value)
  (setf (clipboard-entries *clipboard*)
	(append (clipboard-entries *clipboard*) 
		(list (cons (incf (clipboard-counter *clipboard*))
			    value)))))

(defun clipboard-ref (ref)
  (let ((tail (member ref (clipboard-entries *clipboard*) :key #'car)))
    (cond (tail (cdr (car tail)))
	  (t (error "Invalid clipboard ref: ~s" ref)))))

(defun clipboard-entry (entry)
  (elt (clipboard-entries *clipboard*) entry))

(defun clipboard-delete-entry (index)
  (let* ((list (clipboard-entries *clipboard*))
	 (tail (nthcdr index list)))
    (setf (clipboard-entries *clipboard*)
	  (append (ldiff list tail) (cdr tail)))))

(defun entry-to-string (entry)
  (destructuring-bind (ref . value) (clipboard-entry entry)
    (format nil "#@~d(~a)" ref (to-line value))))

(defun to-line  (object &optional (width 75))
  (with-output-to-string (*standard-output*)
    (write object :right-margin width :lines 1)))

(provide :swank-clipboard)
