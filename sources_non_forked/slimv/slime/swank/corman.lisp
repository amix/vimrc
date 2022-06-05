;;;
;;; swank-corman.lisp --- Corman Lisp specific code for SLIME.
;;;
;;; Copyright (C) 2004, 2005 Espen Wiborg (espenhw@grumblesmurf.org)
;;;
;;; License
;;; =======
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the author be held liable for any damages
;;; arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any purpose,
;;; including commercial applications, and to alter it and redistribute
;;; it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product documentation
;;;    would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source 
;;;    distribution.
;;;
;;; Notes
;;; =====
;;; You will need CCL 2.51, and you will *definitely* need to patch
;;; CCL with the patches at
;;; http://www.grumblesmurf.org/lisp/corman-patches, otherwise SLIME
;;; will blow up in your face.  You should also follow the
;;; instructions on http://www.grumblesmurf.org/lisp/corman-slime.
;;;
;;; The only communication style currently supported is NIL.
;;;
;;; Starting CCL inside emacs (with M-x slime) seems to work for me
;;; with Corman Lisp 2.51, but I have seen random failures with 2.5
;;; (sometimes it works, other times it hangs on start or hangs when
;;; initializing WinSock) - starting CCL externally and using M-x
;;; slime-connect always works fine.
;;;
;;; Sometimes CCL gets confused and starts giving you random memory
;;; access violation errors on startup; if this happens, try redumping
;;; your image.
;;;
;;; What works
;;; ==========
;;; * Basic editing and evaluation
;;; * Arglist display
;;; * Compilation
;;; * Loading files
;;; * apropos/describe
;;; * Debugger
;;; * Inspector
;;; 
;;; TODO
;;; ====
;;; * More debugger functionality (missing bits: restart-frame,
;;; return-from-frame, disassemble-frame, activate-stepping,
;;; toggle-trace)
;;; * XREF
;;; * Profiling
;;; * More sophisticated communication styles than NIL
;;;

(in-package :swank/backend)

;;; Pull in various needed bits
(require :composite-streams)
(require :sockets)
(require :winbase)
(require :lp)

(use-package :gs)

;; MOP stuff

(defclass swank-mop:standard-slot-definition ()
  ()
  (:documentation 
   "Dummy class created so that swank.lisp will compile and load."))

(defun named-by-gensym-p (c)
  (null (symbol-package (class-name c))))

(deftype swank-mop:eql-specializer ()
  '(satisfies named-by-gensym-p))

(defun swank-mop:eql-specializer-object (specializer)
  (with-hash-table-iterator (next-entry cl::*clos-singleton-specializers*)
    (loop (multiple-value-bind (more key value)
              (next-entry)
            (unless more (return nil))
            (when (eq specializer value)
              (return key))))))

(defun swank-mop:class-finalized-p (class)
  (declare (ignore class))
  t)

(defun swank-mop:class-prototype (class)
  (make-instance class))

(defun swank-mop:specializer-direct-methods (obj)
  (declare (ignore obj))
  nil)

(defun swank-mop:generic-function-argument-precedence-order (gf)
  (generic-function-lambda-list gf))

(defun swank-mop:generic-function-method-combination (gf)
  (declare (ignore gf))
  :standard)

(defun swank-mop:generic-function-declarations (gf)
  (declare (ignore gf))
  nil)

(defun swank-mop:slot-definition-documentation (slot)
  (declare (ignore slot))
  (getf slot :documentation nil))

(defun swank-mop:slot-definition-type (slot)
  (declare (ignore slot))
  t)

(import-swank-mop-symbols :cl '(;; classes
                                :standard-slot-definition
                                :eql-specializer
                                :eql-specializer-object
                                ;; standard class readers
                                :class-default-initargs
                                :class-direct-default-initargs
                                :class-finalized-p
                                :class-prototype
                                :specializer-direct-methods
                                ;; gf readers
                                :generic-function-argument-precedence-order
                                :generic-function-declarations
                                :generic-function-method-combination
                                ;; method readers
                                ;; slot readers
                                :slot-definition-documentation
                                :slot-definition-type))

;;;; swank implementations

;;; Debugger

(defvar *stack-trace* nil)
(defvar *frame-trace* nil)

(defstruct frame
  name function address debug-info variables)

(defimplementation call-with-debugging-environment (fn)
  (let* ((real-stack-trace (cl::stack-trace))
         (*stack-trace* (cdr (member 'cl:invoke-debugger real-stack-trace
                                     :key #'car)))
         (*frame-trace*
          (let* ((db::*debug-level*         (1+ db::*debug-level*))
                 (db::*debug-frame-pointer* (db::stash-ebp
                                             (ct:create-foreign-ptr)))
                 (db::*debug-max-level*     (length real-stack-trace))
                 (db::*debug-min-level*     1))
            (cdr (member #'cl:invoke-debugger
                         (cons
                          (make-frame :function nil)
                          (loop for i from db::*debug-min-level*
                             upto db::*debug-max-level*
                             until (eq (db::get-frame-function i) 
				       cl::*top-level*)
                             collect
                               (make-frame 
				:function (db::get-frame-function i)
				:address (db::get-frame-address i))))
                         :key #'frame-function)))))
    (funcall fn)))

(defimplementation compute-backtrace (start end)
  (loop for f in (subseq *stack-trace* start (min end (length *stack-trace*)))
	collect f))

(defimplementation print-frame (frame stream)
  (format stream "~S" frame))

(defun get-frame-debug-info (frame)
  (or (frame-debug-info frame)
      (setf (frame-debug-info frame)
	    (db::prepare-frame-debug-info (frame-function frame)
					  (frame-address frame)))))

(defimplementation frame-locals (frame-number)
  (let* ((frame (elt *frame-trace* frame-number))
         (info (get-frame-debug-info frame)))
    (let ((var-list
           (loop for i from 4 below (length info) by 2
              collect `(list :name ',(svref info i) :id 0
                             :value (db::debug-filter ,(svref info i))))))
      (let ((vars (eval-in-frame `(list ,@var-list) frame-number)))
        (setf (frame-variables frame) vars)))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (elt *frame-trace* frame-number)))
    (let ((cl::*compiler-environment* (get-frame-debug-info frame)))
      (eval form))))

(defimplementation frame-var-value (frame-number var)
  (let ((vars (frame-variables (elt *frame-trace* frame-number))))
    (when vars
      (second (elt vars var)))))

(defimplementation frame-source-location (frame-number)
  (fspec-location (frame-function (elt *frame-trace* frame-number))))

(defun break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ();(*debugger-hook* nil))
      (let ((condition 
	     (make-condition 'simple-condition
			     :format-control format-control
			     :format-arguments format-arguments)))
	;;(format *debug-io* ";;; User break: ~A~%" condition)
	(invoke-debugger condition))))
  nil)

;;; Socket communication

(defimplementation create-socket (host port &key backlog)
  (sockets:start-sockets)
  (sockets:make-server-socket :host host :port port))

(defimplementation local-port (socket)
  (sockets:socket-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket
				      &key external-format buffering timeout)
  (declare (ignore buffering timeout external-format))
  (sockets:make-socket-stream (sockets:accept-socket socket)))

;;; Misc

(defimplementation preferred-communication-style ()
  nil)

(defimplementation getpid ()
  ccl:*current-process-id*)

(defimplementation lisp-implementation-type-name ()
  "cormanlisp")

(defimplementation quit-lisp ()
  (sockets:stop-sockets)
  (win32:exitprocess 0))

(defimplementation set-default-directory (directory)
  (setf (ccl:current-directory) directory)
  (directory-namestring (setf *default-pathname-defaults* 
                              (truename (merge-pathnames directory)))))

(defimplementation default-directory ()
  (directory-namestring (ccl:current-directory)))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (ccl:macroexpand-all form))

;;; Documentation

(defun fspec-location (fspec)
  (when (symbolp fspec)
    (setq fspec (symbol-function fspec)))
  (let ((file (ccl::function-source-file fspec)))
    (if file
        (handler-case
            (let ((truename (truename
                             (merge-pathnames file
                                              ccl:*cormanlisp-directory*))))
              (make-location (list :file (namestring truename))
                             (if (ccl::function-source-line fspec)
                                 (list :line 
				       (1+ (ccl::function-source-line fspec)))
                                 (list :function-name 
				       (princ-to-string
					(function-name fspec))))))
          (error (c) (list :error (princ-to-string c))))
        (list :error (format nil "No source information available for ~S"
                             fspec)))))

(defimplementation find-definitions (name)
  (list (list name (fspec-location name))))

(defimplementation arglist (name)
  (handler-case
      (cond ((and (symbolp name)
                  (macro-function name))
             (ccl::macro-lambda-list (symbol-function name)))
            (t
             (when (symbolp name)
               (setq name (symbol-function name)))
             (if (eq (class-of name) cl::the-class-standard-gf)
                 (generic-function-lambda-list name)
                 (ccl:function-lambda-list name))))
    (error () :not-available)))

(defimplementation function-name (fn)
  (handler-case (getf (cl::function-info-list fn) 'cl::function-name)
    (error () nil)))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil)
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

;;; Compiler 

(defvar *buffer-name* nil)
(defvar *buffer-position*)
(defvar *buffer-string*)
(defvar *compile-filename* nil)

;; FIXME
(defimplementation call-with-compilation-hooks (FN)
  (handler-bind ((error (lambda (c)
                          (signal 'compiler-condition
                                  :original-condition c
                                  :severity :warning
                                  :message (format nil "~A" c)
                                  :location
                                  (cond (*buffer-name*
                                         (make-location
                                          (list :buffer *buffer-name*)
                                          (list :offset *buffer-position* 0)))
                                        (*compile-filename*
                                         (make-location
                                          (list :file *compile-filename*)
                                          (list :position 1)))
                                        (t
                                         (list :error "No location")))))))
    (funcall fn)))

(defimplementation swank-compile-file (input-file output-file 
				       load-p external-format
                                       &key policy)
  (declare (ignore external-format policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil)
	  (*compile-filename* input-file))
      (multiple-value-bind (output-file warnings? failure?)
	  (compile-file input-file :output-file output-file)
	(values output-file warnings?
		(or failure? (and load-p (load output-file))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore filename line column policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-position* position)
          (*buffer-string* string))
      (funcall (compile nil (read-from-string
                             (format nil "(~S () ~A)" 'lambda string))))
      t)))

;;;; Inspecting

;; Hack to make swank.lisp load, at least
(defclass file-stream ())

(defun comma-separated (list &optional (callback (lambda (v)
                                                   `(:value ,v))))
  (butlast (loop for e in list
              collect (funcall callback e)
              collect ", ")))

(defmethod emacs-inspect ((class standard-class))
  `("Name: " 
    (:value ,(class-name class))
    (:newline)
    "Super classes: "
    ,@(comma-separated (swank-mop:class-direct-superclasses class))
    (:newline)
    "Direct Slots: "
    ,@(comma-separated
       (swank-mop:class-direct-slots class)
       (lambda (slot)
	 `(:value ,slot 
		  ,(princ-to-string 
		    (swank-mop:slot-definition-name slot)))))
    (:newline)
    "Effective Slots: "
    ,@(if (swank-mop:class-finalized-p class)
	  (comma-separated
	   (swank-mop:class-slots class)
	   (lambda (slot)
	     `(:value ,slot ,(princ-to-string
			      (swank-mop:slot-definition-name slot)))))
	  '("#<N/A (class not finalized)>"))
    (:newline)
    ,@(when (documentation class t)
	    `("Documentation:" (:newline) ,(documentation class t) (:newline)))
    "Sub classes: "
    ,@(comma-separated (swank-mop:class-direct-subclasses class)
		       (lambda (sub)
			 `(:value ,sub ,(princ-to-string (class-name sub)))))
    (:newline)
    "Precedence List: "
    ,@(if (swank-mop:class-finalized-p class)
	  (comma-separated 
	   (swank-mop:class-precedence-list class)
	   (lambda (class)
	     `(:value ,class 
		      ,(princ-to-string (class-name class)))))
	  '("#<N/A (class not finalized)>"))
    (:newline)))

(defmethod emacs-inspect ((slot cons))
  ;; Inspects slot definitions
  (if (eq (car slot) :name)
      `("Name: " (:value ,(swank-mop:slot-definition-name slot))
		 (:newline)
		 ,@(when (swank-mop:slot-definition-documentation slot)
			 `("Documentation:"  
			   (:newline)
			   (:value 
			    ,(swank-mop:slot-definition-documentation slot))
			   (:newline)))
		 "Init args: " (:value 
				,(swank-mop:slot-definition-initargs slot))
		 (:newline)
		 "Init form: "
		 ,(if (swank-mop:slot-definition-initfunction slot)
		      `(:value ,(swank-mop:slot-definition-initform slot))
		      "#<unspecified>") (:newline)
		      "Init function: " 
		      (:value ,(swank-mop:slot-definition-initfunction slot))
		      (:newline))
      (call-next-method)))
  
(defmethod emacs-inspect ((pathname pathnames::pathname-internal))
  (list*  (if (wild-pathname-p pathname)
              "A wild pathname."
              "A pathname.")
	  '(:newline)
          (append (label-value-line*
                   ("Namestring" (namestring pathname))
                   ("Host"       (pathname-host pathname))
                   ("Device"     (pathname-device pathname))
                   ("Directory"  (pathname-directory pathname))
                   ("Name"       (pathname-name pathname))
                   ("Type"       (pathname-type pathname))
                   ("Version"    (pathname-version pathname)))
                  (unless (or (wild-pathname-p pathname)
                              (not (probe-file pathname)))
                    (label-value-line "Truename" (truename pathname))))))

(defmethod emacs-inspect ((o t))
  (cond ((cl::structurep o) (inspect-structure o))
	(t (call-next-method))))

(defun inspect-structure (o)
   (let* ((template (cl::uref o 1))
	  (num-slots (cl::struct-template-num-slots template)))
     (cond ((symbolp template)
	    (loop for i below num-slots
		  append (label-value-line i (cl::uref o (+ 2 i)))))
	   (t
	    (loop for i below num-slots
		  append (label-value-line (elt template (+ 6 (* i 5)))
					   (cl::uref o (+ 2 i))))))))


;;; Threads

(require 'threads)

(defstruct (mailbox (:conc-name mailbox.)) 
  thread
  (lock (make-instance 'threads:critical-section))
  (queue '() :type list))

(defvar *mailbox-lock* (make-instance 'threads:critical-section))
(defvar *mailboxes* (list))

(defmacro with-lock  (lock &body body)
  `(threads:with-synchronization (threads:cs ,lock)
    ,@body))

(defimplementation spawn (fun &key name)
  (declare (ignore name))
  (th:create-thread 
   (lambda ()
     (handler-bind ((serious-condition #'invoke-debugger))
       (unwind-protect (funcall fun)
	 (with-lock *mailbox-lock*
	   (setq *mailboxes* (remove cormanlisp:*current-thread-id*
				     *mailboxes* :key #'mailbox.thread))))))))

(defimplementation thread-id (thread)
  thread)

(defimplementation find-thread (thread)
  (if (thread-alive-p thread)
      thread))

(defimplementation thread-alive-p (thread)
  (if (threads:thread-handle thread) t nil))

(defimplementation current-thread ()
  cormanlisp:*current-thread-id*)

;; XXX implement it
(defimplementation all-threads ()
  '())

;; XXX something here is broken
(defimplementation kill-thread (thread)
  (threads:terminate-thread thread 'killed))

(defun mailbox (thread)
  (with-lock *mailbox-lock*
    (or (find thread *mailboxes* :key #'mailbox.thread)
	(let ((mb (make-mailbox :thread thread)))
	  (push mb *mailboxes*)
	  mb))))

(defimplementation send (thread message)
  (let ((mbox (mailbox thread)))
    (with-lock (mailbox.lock mbox)
      (setf (mailbox.queue mbox)
	    (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive ()
  (let ((mbox (mailbox cormanlisp:*current-thread-id*)))
    (loop 
     (with-lock (mailbox.lock mbox)
       (when (mailbox.queue mbox)
	 (return (pop (mailbox.queue mbox)))))
     (sleep 0.1))))


;;; This is probably not good, but it WFM
(in-package :common-lisp)

(defvar *old-documentation* #'documentation)
(defun documentation (thing &optional (type 'function))
  (if (symbolp thing)
      (funcall *old-documentation* thing type)
      (values)))

(defmethod print-object ((restart restart) stream)
  (if (or *print-escape*
          *print-readably*)
      (print-unreadable-object (restart stream :type t :identity t)
        (princ (restart-name restart) stream))
      (when (functionp (restart-report-function restart))
        (funcall (restart-report-function restart) stream))))
