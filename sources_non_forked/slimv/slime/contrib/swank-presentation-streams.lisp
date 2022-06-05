;;; swank-presentation-streams.lisp --- Streams that allow attaching object identities
;;;                                     to portions of output
;;;
;;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;;          Helmut Eller  <heller@common-lisp.net>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-presentations))

;; This file contains a mechanism for printing to the slime repl so
;; that the printed result remembers what object it is associated
;; with.  This extends the recording of REPL results.
;;
;; There are two methods:
;;
;; 1. Depends on the ilisp bridge code being installed and ready to
;;    intercept messages in the printed stream. We encode the
;;    information with a message saying that we are starting to print
;;    an object corresponding to a given id and another when we are
;;    done. The process filter notices these and adds the necessary
;;    text properties to the output.
;;
;; 2. Use separate protocol messages :presentation-start and
;;    :presentation-end for sending presentations.
;;
;; We only do this if we know we are printing to a slime stream,
;; checked with the method slime-stream-p. Initially this checks for
;; the knows slime streams looking at *connections*. In cmucl, sbcl, and
;; openmcl it also checks if it is a pretty-printing stream which
;; ultimately prints to a slime stream.
;;
;; Method 1 seems to be faster, but the printed escape sequences can 
;; disturb the column counting, and thus the layout in pretty-printing.
;; We use method 1 when a dedicated output stream is used.  
;;
;; Method 2 is cleaner and works with pretty printing if the pretty
;; printers support "annotations".  We use method 2 when no dedicated
;; output stream is used.

;; Control
(defvar *enable-presenting-readable-objects* t
  "set this to enable automatically printing presentations for some
subset of readable objects, such as pathnames."  )

;; doing it

(defmacro presenting-object (object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl"
  `(presenting-object-1 ,object ,stream #'(lambda () ,@body)))

(defmacro presenting-object-if (predicate object stream &body body)
  "What you use in your code. Wrap this around some printing and that text will
be sensitive and remember what object it is in the repl if predicate is true"
  (let ((continue (gensym)))
  `(let ((,continue #'(lambda () ,@body)))
    (if ,predicate
	(presenting-object-1 ,object ,stream ,continue)
	(funcall ,continue)))))

;;; Get pretty printer patches for SBCL at load (not compile) time.
#+#:disable-dangerous-patching ; #+sbcl
(eval-when (:load-toplevel)
  (handler-bind ((simple-error
		  (lambda (c)
		    (declare (ignore c))
		    (let ((clobber-it (find-restart 'sb-kernel::clobber-it)))
		      (when clobber-it (invoke-restart clobber-it))))))
    (sb-ext:without-package-locks
      (swank/sbcl::with-debootstrapping
	(load (make-pathname
	       :name "sbcl-pprint-patch"
	       :type "lisp"
	       :directory (pathname-directory
			   swank-loader:*source-directory*)))))))

(let ((last-stream nil)
      (last-answer nil))
  (defun slime-stream-p (stream)
    "Check if stream is one of the slime streams, since if it isn't we
don't want to present anything.
Two special return values: 
:DEDICATED -- Output ends up on a dedicated output stream
:REPL-RESULT -- Output ends up on the :repl-results target.
"
    (if (eq last-stream stream)
	last-answer
	(progn
	  (setq last-stream stream)
	  (if (eq stream t) 
	      (setq stream *standard-output*))
	  (setq last-answer 
		(or #+openmcl 
		    (and (typep stream 'ccl::xp-stream) 
					;(slime-stream-p (ccl::xp-base-stream (slot-value stream 'ccl::xp-structure)))
			 (slime-stream-p (ccl::%svref (slot-value stream 'ccl::xp-structure) 1)))
		    #+cmu
		    (or (and (typep stream 'lisp::indenting-stream)
			     (slime-stream-p (lisp::indenting-stream-stream stream)))
			(and (typep stream 'pretty-print::pretty-stream)
			     (fboundp 'pretty-print::enqueue-annotation)
			     (let ((slime-stream-p
				    (slime-stream-p (pretty-print::pretty-stream-target stream))))
			       (and ;; Printing through CMUCL pretty
				    ;; streams is only cleanly
				    ;; possible if we are using the
				    ;; bridge-less protocol with
				    ;; annotations, because the bridge
				    ;; escape sequences disturb the
				    ;; pretty printer layout.
				    (not (eql slime-stream-p :dedicated-output))
				    ;; If OK, return the return value
				    ;; we got from slime-stream-p on
				    ;; the target stream (could be
				    ;; :repl-result):
				    slime-stream-p))))
		    #+sbcl
		    (let ()
		      (declare (notinline sb-pretty::pretty-stream-target))
		      (and (typep stream (find-symbol "PRETTY-STREAM" 'sb-pretty))
                           (find-symbol "ENQUEUE-ANNOTATION" 'sb-pretty)
                           (not *use-dedicated-output-stream*)
                           (slime-stream-p (sb-pretty::pretty-stream-target stream))))
		    #+allegro
		    (and (typep stream 'excl:xp-simple-stream)
			 (slime-stream-p (excl::stream-output-handle stream)))
		    (loop for connection in *connections*
			  thereis (or (and (eq stream (connection.dedicated-output connection))
					   :dedicated)
				      (eq stream (connection.socket-io connection))
				      (eq stream (connection.user-output connection))
				      (eq stream (connection.user-io connection))
				      (and (eq stream (connection.repl-results connection))
					   :repl-result)))))))))

(defun can-present-readable-objects (&optional stream)
  (declare (ignore stream))
  *enable-presenting-readable-objects*)

;; If we are printing to an XP (pretty printing) stream, printing the
;; escape sequences directly would mess up the layout because column
;; counting is disturbed.  Use "annotations" instead.
#+allegro
(defun write-annotation (stream function arg)
  (if (typep stream 'excl:xp-simple-stream)
      (excl::schedule-annotation stream function arg)
      (funcall function arg stream nil)))
#+cmu
(defun write-annotation (stream function arg)
  (if (and (typep stream 'pp:pretty-stream)
	   (fboundp 'pp::enqueue-annotation))
      (pp::enqueue-annotation stream function arg)
      (funcall function arg stream nil)))
#+sbcl
(defun write-annotation (stream function arg)
  (let ((enqueue-annotation
	 (find-symbol "ENQUEUE-ANNOTATION" 'sb-pretty)))
    (if (and enqueue-annotation
	     (typep stream (find-symbol "PRETTY-STREAM" 'sb-pretty)))
	(funcall enqueue-annotation stream function arg)
	(funcall function arg stream nil))))
#-(or allegro cmu sbcl)
(defun write-annotation (stream function arg)
  (funcall function arg stream nil))

(defstruct presentation-record 
  (id)
  (printed-p)
  (target))

(defun presentation-start (record stream truncatep) 
  (unless truncatep
    ;; Don't start new presentations when nothing is going to be
    ;; printed due to *print-lines*.
    (let ((pid (presentation-record-id record))
	  (target (presentation-record-target record)))
      (case target
	(:dedicated 
	 ;; Use bridge protocol
	 (write-string "<" stream)
	 (prin1 pid stream)
	 (write-string "" stream))
	(t
	 (finish-output stream)
	 (send-to-emacs `(:presentation-start ,pid ,target)))))
    (setf (presentation-record-printed-p record) t)))
	   
(defun presentation-end (record stream truncatep)
  (declare (ignore truncatep))
  ;; Always end old presentations that were started.
  (when (presentation-record-printed-p record)
    (let ((pid (presentation-record-id record))
	  (target (presentation-record-target record)))
      (case target
	(:dedicated 
	 ;; Use bridge protocol
	 (write-string ">" stream)
	 (prin1 pid stream)
	 (write-string "" stream))
	(t
	 (finish-output stream)
	 (send-to-emacs `(:presentation-end ,pid ,target)))))))

(defun presenting-object-1 (object stream continue)
  "Uses the bridge mechanism with two messages >id and <id. The first one
says that I am starting to print an object with this id. The second says I am finished"
  ;; this declare special is to let the compiler know that *record-repl-results* will eventually be
  ;; a global special, even if it isn't when this file is compiled/loaded.
  (declare (special *record-repl-results*))
  (let ((slime-stream-p 
	 (and *record-repl-results* (slime-stream-p stream))))
    (if slime-stream-p
	(let* ((pid (swank::save-presented-object object))
	       (record (make-presentation-record :id pid :printed-p nil
						 :target (if (eq slime-stream-p :repl-result)
							     :repl-result
							     nil))))
	  (write-annotation stream #'presentation-start record)
	  (multiple-value-prog1
	      (funcall continue)
	    (write-annotation stream #'presentation-end record)))
	(funcall continue))))

(defun present-repl-results-via-presentation-streams (values)
  ;; Override a function in swank.lisp, so that 
  ;; nested presentations work in the REPL result.
  (let ((repl-results (connection.repl-results *emacs-connection*)))
    (flet ((send (value)
	     (presenting-object value repl-results
	       (prin1 value repl-results))
	     (terpri repl-results)))
      (if (null values)
	  (progn 
	    (princ "; No value" repl-results)
	    (terpri repl-results))
	  (mapc #'send values)))
    (finish-output repl-results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+openmcl
(in-package :ccl)

#+openmcl
(defun monkey-patch-stream-printing ()
  (let ((*warn-if-redefine-kernel* nil)
	(*warn-if-redefine* nil))
    (defun %print-unreadable-object (object stream type id thunk)
      (cond ((null stream) (setq stream *standard-output*))
	    ((eq stream t) (setq stream *terminal-io*)))
      (swank::presenting-object object stream
	(write-unreadable-start object stream)
	(when type
	  (princ (type-of object) stream)
	  (stream-write-char stream #\space))
	(when thunk
	  (funcall thunk))
	(if id
	    (%write-address object stream #\>)
	    (pp-end-block stream ">"))
	nil))
    (defmethod print-object :around ((pathname pathname) stream)
      (swank::presenting-object-if
	  (swank::can-present-readable-objects stream)
	  pathname stream (call-next-method))))
  (ccl::def-load-pointers clear-presentations ()
    (swank::clear-presentation-tables)))

(in-package :swank)

#+cmu
(progn
  (fwrappers:define-fwrapper presenting-unreadable-wrapper (object stream type identity body)
    (presenting-object object stream
      (fwrappers:call-next-function)))

  (fwrappers:define-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (fwrappers:call-next-function)))

  (defun monkey-patch-stream-printing ()
    (fwrappers::fwrap 'lisp::%print-pathname  #'presenting-pathname-wrapper)
    (fwrappers::fwrap 'lisp::%print-unreadable-object  #'presenting-unreadable-wrapper)))

#+sbcl
(progn
  (defvar *saved-%print-unreadable-object*
    (fdefinition 'sb-impl::%print-unreadable-object))

  (defun monkey-patch-stream-printing ()
    (sb-ext:without-package-locks
      (when (eq (fdefinition 'sb-impl::%print-unreadable-object)
		*saved-%print-unreadable-object*)
	(setf (fdefinition 'sb-impl::%print-unreadable-object)
	      (lambda (object stream &rest args)
		(presenting-object object stream
                  (apply *saved-%print-unreadable-object*
                         object stream args)))))
      (defmethod print-object :around ((object pathname) stream)
	(presenting-object object stream
	  (call-next-method))))))

#+allegro
(progn
  (excl:def-fwrapper presenting-unreadable-wrapper (object stream type identity continuation)
    (swank::presenting-object object stream (excl:call-next-fwrapper)))
  (excl:def-fwrapper presenting-pathname-wrapper (pathname stream depth)
    (presenting-object-if (can-present-readable-objects stream) pathname stream
      (excl:call-next-fwrapper)))
  (defun monkey-patch-stream-printing ()
    (excl:fwrap 'excl::print-unreadable-object-1
		'print-unreadable-present 'presenting-unreadable-wrapper)
    (excl:fwrap 'excl::pathname-printer
		'print-pathname-present 'presenting-pathname-wrapper)))

#-(or allegro sbcl cmu openmcl)
(defun monkey-patch-stream-printing ()
  (values))

;; Hook into SWANK.

(defslimefun init-presentation-streams ()
  (monkey-patch-stream-printing)
  ;; FIXME: import/use swank-repl to avoid package qualifier.
  (setq swank-repl:*send-repl-results-function*
	'present-repl-results-via-presentation-streams))

(provide :swank-presentation-streams)
