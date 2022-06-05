;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-mkcl.lisp --- SLIME backend for MKCL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(defpackage swank/mkcl
  (:use cl swank/backend))

(in-package swank/mkcl)

;;(declaim (optimize (debug 3)))

(defvar *tmp*)

(defimplementation gray-package-name ()
  '#:gray)

(eval-when (:compile-toplevel :load-toplevel)

  (swank/backend::import-swank-mop-symbols :clos
    ;;  '(:eql-specializer
    ;;    :eql-specializer-object
    ;;    :generic-function-declarations
    ;;    :specializer-direct-methods
    ;;    :compute-applicable-methods-using-classes)
    nil
    ))


;;; UTF8

(defimplementation string-to-utf8 (string)
  (mkcl:octets (si:utf-8 string)))

(defimplementation utf8-to-string (octets)
  (string (si:utf-8 octets)))


;;;; TCP Server

(eval-when (:compile-toplevel :load-toplevel)
  ;; At compile-time we need access to the sb-bsd-sockets package for the
  ;; the following code to be read properly.
  ;; It is a bit a shame we have to load the entire module to get that.
  (require 'sockets))


(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

(defimplementation create-socket (host port &key backlog)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket (or backlog 5))
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(defun accept (socket)
  "Like socket-accept, but retry on EINTR."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore timeout))
  (sb-bsd-sockets:socket-make-stream (accept socket)
                                     :output t ;; bogus
                                     :input t ;; bogus
                                     :buffering buffering ;; bogus
                                     :element-type (if external-format
                                                       'character 
                                                     '(unsigned-byte 8))
                                     :external-format external-format
                                     ))

(defimplementation preferred-communication-style ()
  :spawn
  )

(defvar *external-format-to-coding-system*
  '((:iso-8859-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defun external-format (coding-system)
  (or (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                      *external-format-to-coding-system*))
      (find coding-system (si:all-encodings) :test #'string-equal)))

(defimplementation find-external-format (coding-system)
  #+unicode (external-format coding-system)
  ;; Without unicode support, MKCL uses the one-byte encoding of the
  ;; underlying OS, and will barf on anything except :DEFAULT.  We
  ;; return NIL here for known multibyte encodings, so
  ;; SWANK:CREATE-SERVER will barf.
  #-unicode (let ((xf (external-format coding-system)))
              (if (member xf '(:utf-8))
                  nil
                :default)))



;;;; Unix signals

(defimplementation install-sigint-handler (handler)
  (let ((old-handler (symbol-function 'si:terminal-interrupt)))
    (setf (symbol-function 'si:terminal-interrupt)
          (if (consp handler)
              (car handler)
              (lambda (&rest args)
                (declare (ignore args))
                (funcall handler)
                (continue))))
    (list old-handler)))


(defimplementation getpid ()
  (mkcl:getpid))

(defimplementation set-default-directory (directory)
  (mk-ext::chdir (namestring directory))
  (default-directory))

(defimplementation default-directory ()
  (namestring (mk-ext:getcwd)))

(defmacro progf (plist &rest forms)
  `(let (_vars _vals)
     (do ((p ,plist (cddr p)))
         ((endp p))
         (push (car p) _vars)
         (push (cadr p) _vals))
     (progv _vars _vals ,@forms)
     )
  )

(defvar *inferior-lisp-sleeping-post* nil)

(defimplementation quit-lisp ()
  (progf (ignore-errors (eval (read-from-string "swank::*saved-global-streams*"))) ;; restore original IO streams.
         (when *inferior-lisp-sleeping-post* (mt:semaphore-signal *inferior-lisp-sleeping-post*))
         ;;(mk-ext:quit :verbose t)
         ))


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defun signal-compiler-condition (&rest args)
  (signal (apply #'make-condition 'compiler-condition args)))

#|
(defun handle-compiler-warning (condition)
  (signal-compiler-condition
   :original-condition condition
   :message (format nil "~A" condition)
   :severity :warning
   :location
   (if *buffer-name*
       (make-location (list :buffer *buffer-name*)
                      (list :offset *buffer-start-position* 0))
       ;; ;; compiler::*current-form*
       ;; (if compiler::*current-function*
       ;;     (make-location (list :file *compile-filename*)
       ;;                    (list :function-name   
       ;;                          (symbol-name
       ;;                           (slot-value compiler::*current-function*
       ;;                                       'compiler::name))))
       (list :error "No location found.")
           ;; )
       )))
|#

#|
(defun condition-location (condition)
  (let ((file     (compiler:compiler-message-file condition))
        (position (compiler:compiler-message-file-position condition)))
    (if (and position (not (minusp position)))
        (if *buffer-name*
            (make-buffer-location *buffer-name*
                                  *buffer-start-position*
                                  position)
            (make-file-location file position))
        (make-error-location "No location found."))))
|#

(defun condition-location (condition)
  (if *buffer-name*
      (make-location (list :buffer *buffer-name*)
                     (list :offset *buffer-start-position* 0))
       ;; ;; compiler::*current-form*   ;
       ;; (if compiler::*current-function* ;
       ;;     (make-location (list :file *compile-filename*) ;
       ;;                    (list :function-name ;
       ;;                          (symbol-name ;
       ;;                           (slot-value compiler::*current-function* ;
       ;;                                       'compiler::name)))) ;
    (if (typep condition 'compiler::compiler-message)
        (make-location (list :file (namestring (compiler:compiler-message-file condition)))
                       (list :end-position (compiler:compiler-message-file-end-position condition)))
      (list :error "No location found."))
    )
  )

(defun handle-compiler-message (condition)
  (unless (typep condition 'compiler::compiler-note)
    (signal-compiler-condition
     :original-condition condition
     :message (princ-to-string condition)
     :severity (etypecase condition
                 (compiler:compiler-fatal-error :error)
                 (compiler:compiler-error       :error)
                 (error                  :error)
                 (style-warning          :style-warning)
                 (warning                :warning))
     :location (condition-location condition))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((compiler:compiler-message #'handle-compiler-message))
    (funcall function)))

(defimplementation swank-compile-file (input-file output-file
                                                  load-p external-format
                                                  &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil)
          (*compile-filename* input-file))
      (handler-bind (#|
                     (compiler::compiler-note
                      #'(lambda (n)
                          (format t "~%swank saw a compiler note: ~A~%" n) (finish-output) nil))
                     (compiler::compiler-warning
                      #'(lambda (w)
                          (format t "~%swank saw a compiler warning: ~A~%" w) (finish-output) nil))
                     (compiler::compiler-error
                      #'(lambda (e)
                          (format t "~%swank saw a compiler error: ~A~%" e) (finish-output) nil))
                     |#
                     )
        (multiple-value-bind (output-truename warnings-p failure-p)
             (compile-file input-file :output-file output-file :external-format external-format)
           (values output-truename warnings-p
                   (or failure-p
                       (and load-p (not (load output-truename))))))))))

(defimplementation swank-compile-string (string &key buffer position filename line column policy)
  (declare (ignore filename line column policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (with-input-from-string (s string)
        (when position (file-position position))
        (compile-from-stream s)))))

(defun compile-from-stream (stream)
  (let ((file (mkcl:mkstemp "TMP:MKCL-SWANK-TMPXXXXXX"))
        output-truename
        warnings-p
        failure-p
        )
    (with-open-file (s file :direction :output :if-exists :overwrite)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((not line))
	(write-line line s)))
    (unwind-protect
        (progn
          (multiple-value-setq (output-truename warnings-p failure-p)
               (compile-file file))
          (and (not failure-p) (load output-truename)))
      (when (probe-file file) (delete-file file))
      (when (probe-file output-truename) (delete-file output-truename)))))


;;;; Documentation

(defun grovel-docstring-for-arglist (name type)
  (flet ((compute-arglist-offset (docstring)
           (when docstring
             (let ((pos1 (search "Args: " docstring)))
               (if pos1
                   (+ pos1 6)
                   (let ((pos2 (search "Syntax: " docstring)))
                     (when pos2
                       (+ pos2 8))))))))
    (let* ((docstring (si::get-documentation name type))
           (pos (compute-arglist-offset docstring)))
      (if pos
          (multiple-value-bind (arglist errorp)
              (ignore-errors
                (values (read-from-string docstring t nil :start pos)))
            (if (or errorp (not (listp arglist)))
                :not-available
                arglist
                ))
          :not-available ))))

(defimplementation arglist (name)
  (cond ((and (symbolp name) (special-operator-p name))
         (let ((arglist (grovel-docstring-for-arglist name 'function)))
           (if (consp arglist) (cdr arglist) arglist)))
        ((and (symbolp name) (macro-function name))
         (let ((arglist (grovel-docstring-for-arglist name 'function)))
           (if (consp arglist) (cdr arglist) arglist)))
        ((or (functionp name) (fboundp name))
         (multiple-value-bind (name fndef)
           (if (functionp name)
               (values (function-name name) name)
             (values name (fdefinition name)))
           (let ((fle (function-lambda-expression fndef)))
             (case (car fle)
                   (si:lambda-block (caddr fle))
                   (t (typecase fndef
                        (generic-function (clos::generic-function-lambda-list fndef))
                        (compiled-function (grovel-docstring-for-arglist name 'function))
                        (function :not-available)))))))
        (t :not-available)))

(defimplementation function-name (f)
  (si:compiled-function-name f)
  )

(eval-when (:compile-toplevel :load-toplevel)
  ;; At compile-time we need access to the walker package for the
  ;; the following code to be read properly.
  ;; It is a bit a shame we have to load the entire module to get that.
  (require 'walker))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (walker:macroexpand-all form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (dolist (type '(:VARIABLE :FUNCTION :CLASS))
      (let ((doc (describe-definition symbol type)))
        (when doc
          (setf result (list* type doc result)))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))

;;; Debugging

(eval-when (:compile-toplevel :load-toplevel)
  (import
   '(si::*break-env*
     si::*ihs-top*
     si::*ihs-current*
     si::*ihs-base*
     si::*frs-base*
     si::*frs-top*
     si::*tpl-commands*
     si::*tpl-level*
     si::frs-top
     si::ihs-top
     si::ihs-fun
     si::ihs-env
     si::sch-frs-base
     si::set-break-env
     si::set-current-ihs
     si::tpl-commands)))

(defvar *backtrace* '())

(defun in-swank-package-p (x)
  (and
   (symbolp x)
   (member (symbol-package x)
           (list #.(find-package :swank)
                 #.(find-package :swank/backend)
                 #.(ignore-errors (find-package :swank-mop))
                 #.(ignore-errors (find-package :swank-loader))))
   t))

(defun is-swank-source-p (name)
  (setf name (pathname name))
  #+(or)
  (pathname-match-p
   name
   (make-pathname :defaults swank-loader::*source-directory*
                  :name (pathname-name name)
                  :type (pathname-type name)
                  :version (pathname-version name)))
  nil)

(defun is-ignorable-fun-p (x)
  (or
   (in-swank-package-p (frame-name x))
   (multiple-value-bind (file position)
       (ignore-errors (si::compiled-function-file (car x)))
     (declare (ignore position))
     (if file (is-swank-source-p file)))))

(defmacro find-ihs-top (x)
  (declare (ignore x))
  '(si::ihs-top))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* (;;(*tpl-commands* si::tpl-commands)
         (*ihs-base* 0)
         (*ihs-top* (find-ihs-top 'call-with-debugging-environment))
         (*ihs-current* *ihs-top*)
         (*frs-base* (or (sch-frs-base 0 #|*frs-top*|# *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*read-suppress* nil)
         ;;(*tpl-level* (1+ *tpl-level*))
         (*backtrace* (loop for ihs from 0 below *ihs-top*
                            collect (list (si::ihs-fun ihs)
                                          (si::ihs-env ihs)
                                          nil))))
    (declare (special *ihs-current*))
    (loop for f from *frs-base* to *frs-top*
          do (let ((i (- (si::frs-ihs f) *ihs-base* 1)))
               (when (plusp i)
                 (let* ((x (elt *backtrace* i))
                        (name (si::frs-tag f)))
                   (unless (mkcl:fixnump name)
                     (push name (third x)))))))
    (setf *backtrace* (remove-if #'is-ignorable-fun-p (nreverse *backtrace*)))
    (setf *tmp* *backtrace*)
    (set-break-env)
    (set-current-ihs)
    (let ((*ihs-base* *ihs-top*))
      (funcall debugger-loop-fn))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (*ihs-base* (find-ihs-top 'call-with-debugger-hook)))
    (funcall fun)))

(defimplementation compute-backtrace (start end)
  (when (numberp end)
    (setf end (min end (length *backtrace*))))
  (loop for f in (subseq *backtrace* start end)
        collect f))

(defimplementation format-sldb-condition (condition)
  "Format a condition for display in SLDB."
  ;;(princ-to-string condition)
  (format nil "~A~%In thread: ~S" condition mt:*thread*)
  )

(defun frame-name (frame)
  (let ((x (first frame)))
    (if (symbolp x)
      x
      (function-name x))))

(defun function-position (fun)
  (multiple-value-bind (file position)
      (si::compiled-function-file fun)
    (and file (make-location
               `(:file ,(if (stringp file) file (namestring file)))
               ;;`(:position ,position)
               `(:end-position , position)))))

(defun frame-function (frame)
  (let* ((x (first frame))
         fun position)
    (etypecase x
      (symbol (and (fboundp x)
                   (setf fun (fdefinition x)
                         position (function-position fun))))
      (function (setf fun x position (function-position x))))
    (values fun position)))

(defun frame-decode-env (frame)
  (let ((functions '())
        (blocks '())
        (variables '()))
    (setf frame (si::decode-ihs-env (second frame)))
    (dolist (record frame)
      (let* ((record0 (car record))
	     (record1 (cdr record)))
	(cond ((or (symbolp record0) (stringp record0))
	       (setq variables (acons record0 record1 variables)))
	      ((not (mkcl:fixnump record0))
	       (push record1 functions))
	      ((symbolp record1)
	       (push record1 blocks))
	      (t
	       ))))
    (values functions blocks variables)))

(defimplementation print-frame (frame stream)
  (let ((function (first frame)))
    (let ((fname
;;;           (cond ((symbolp function) function)
;;;                 ((si:instancep function) (slot-value function 'name))
;;;                 ((compiled-function-p function)
;;;                  (or (si::compiled-function-name function) 'lambda))
;;;                 (t :zombi))
           (si::get-fname function)
           ))
      (if (eq fname 'si::bytecode)
          (format stream "~A [Evaluation of: ~S]"
                  fname (function-lambda-expression function))
        (format stream "~A" fname)
        )
      (when (si::closurep function)
        (format stream
                ", closure generated from ~A"
                (si::get-fname (si:closure-producer function)))
        )
      )
    )
  )

(defimplementation frame-source-location (frame-number)
  (nth-value 1 (frame-function (elt *backtrace* frame-number))))

(defimplementation frame-catch-tags (frame-number)
  (third (elt *backtrace* frame-number)))

(defimplementation frame-locals (frame-number)
  (loop for (name . value) in (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
        with i = 0
        collect (list :name name :id (prog1 i (incf i)) :value value)))

(defimplementation frame-var-value (frame-number var-id)
  (cdr (elt (nth-value 2 (frame-decode-env (elt *backtrace* frame-number))) var-id)))

(defimplementation disassemble-frame (frame-number)
  (let ((fun (frame-fun (elt *backtrace* frame-number))))
    (disassemble fun)))

(defimplementation eval-in-frame (form frame-number)
  (let ((env (second (elt *backtrace* frame-number))))
    (si:eval-in-env form env)))

#|
(defimplementation gdb-initial-commands ()
  ;; These signals are used by the GC.
  #+linux '("handle SIGPWR  noprint nostop"
            "handle SIGXCPU noprint nostop"))

(defimplementation command-line-args ()
  (loop for n from 0 below (si:argc) collect (si:argv n)))
|#

;;;; Inspector

(defmethod emacs-inspect ((o t))
  ; ecl clos support leaves some to be desired
  (cond
    ((streamp o)
     (list*
      (format nil "~S is an ordinary stream~%" o)
      (append
       (list
        "Open for "
        (cond
          ((ignore-errors (interactive-stream-p o)) "Interactive")
          ((and (input-stream-p o) (output-stream-p o)) "Input and output")
          ((input-stream-p o) "Input")
          ((output-stream-p o) "Output"))
        `(:newline) `(:newline))
       (label-value-line*
        ("Element type" (stream-element-type o))
        ("External format" (stream-external-format o)))
       (ignore-errors (label-value-line*
                       ("Broadcast streams" (broadcast-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Concatenated streams" (concatenated-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Echo input stream" (echo-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Echo output stream" (echo-stream-output-stream o))))
       (ignore-errors (label-value-line*
                       ("Output String" (get-output-stream-string o))))
       (ignore-errors (label-value-line*
                       ("Synonym symbol" (synonym-stream-symbol o))))
       (ignore-errors (label-value-line*
                       ("Input stream" (two-way-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Output stream" (two-way-stream-output-stream o)))))))
    ((si:instancep o) ;;t
     (let* ((cl (si:instance-class o))
            (slots (clos::class-slots cl)))
       (list* (format nil "~S is an instance of class ~A~%"
                       o (clos::class-name cl))
               (loop for x in slots append
                    (let* ((name (clos::slot-definition-name x))
                           (value (if (slot-boundp o name)
                                      (clos::slot-value o name)
                                      "Unbound"
                                      )))
                      (list
                       (format nil "~S: " name)
                       `(:value ,value)
                       `(:newline)))))))
    (t (list (format nil "~A" o)))))

;;;; Definitions

(defimplementation find-definitions (name)
  (if (fboundp name)
      (let ((tmp (find-source-location (symbol-function name))))
        `(((defun ,name) ,tmp)))))

(defimplementation find-source-location (obj)
  (setf *tmp* obj)
  (or
   (typecase obj
     (function
      (multiple-value-bind (file pos) (ignore-errors (si::compiled-function-file obj))
        (if (and file pos) 
            (make-location
              `(:file ,(if (stringp file) file (namestring file)))
              `(:end-position ,pos) ;; `(:position ,pos)
              `(:snippet
                ,(with-open-file (s file)
                                 (file-position s pos)
                                 (skip-comments-and-whitespace s)
                                 (read-snippet s))))))))
   `(:error (format nil "Source definition of ~S not found" obj))))

;;;; Profiling


(eval-when (:compile-toplevel :load-toplevel)
  ;; At compile-time we need access to the profile package for the
  ;; the following code to be read properly.
  ;; It is a bit a shame we have to load the entire module to get that.
  (require 'profile))


(defimplementation profile (fname)
  (when fname (eval `(profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (profile:unprofile-all)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (profile:report))

(defimplementation profile-reset ()
  (profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (profile:profile))

(defimplementation profile-package (package callers methods)
  (declare (ignore callers methods))
  (eval `(profile:profile ,(package-name (find-package package)))))


;;;; Threads

(defvar *thread-id-counter* 0)

(defvar *thread-id-counter-lock*
  (mt:make-lock :name "thread id counter lock"))

(defun next-thread-id ()
  (mt:with-lock (*thread-id-counter-lock*)
    (incf *thread-id-counter*))
  )

(defparameter *thread-id-map* (make-hash-table))
(defparameter *id-thread-map* (make-hash-table))

(defvar *thread-id-map-lock*
  (mt:make-lock :name "thread id map lock"))

(defparameter +default-thread-local-variables+
  '(*macroexpand-hook*
    *default-pathname-defaults*
    *readtable*
    *random-state*
    *compile-print*
    *compile-verbose*
    *load-print*
    *load-verbose*
    *print-array*
    *print-base*
    *print-case*
    *print-circle*
    *print-escape*
    *print-gensym*
    *print-length*
    *print-level*
    *print-lines*
    *print-miser-width*
    *print-pprint-dispatch*
    *print-pretty*
    *print-radix*
    *print-readably*
    *print-right-margin*
    *read-base*
    *read-default-float-format*
    *read-eval*
    *read-suppress*
    ))
  
(defun thread-local-default-bindings ()
  (let (local)
    (dolist (var +default-thread-local-variables+ local)
      (setq local (acons var (symbol-value var) local))
      )))

;; mkcl doesn't have weak pointers
(defimplementation spawn (fn &key name initial-bindings)
  (let* ((local-defaults (thread-local-default-bindings))
         (thread 
          ;;(mt:make-thread :name name)
          (mt:make-thread :name name
                          :initial-bindings (nconc initial-bindings 
                                                   local-defaults))
           )
         (id (next-thread-id)))
    (mt:with-lock (*thread-id-map-lock*)
      (setf (gethash id *thread-id-map*) thread)
      (setf (gethash thread *id-thread-map*) id))
    (mt:thread-preset
     thread
     #'(lambda ()
         (unwind-protect
              (progn
                ;;(format t "~&Starting thread: ~S.~%" name) (finish-output)
                (mt:thread-detach nil)
                (funcall fn))
           (progn
             ;;(format t "~&Wrapping up thread: ~S.~%" name) (finish-output)
             (mt:with-lock (*thread-id-map-lock*)
               (remhash thread *id-thread-map*)
               (remhash id *thread-id-map*))
             ;;(format t "~&Finished thread: ~S~%" name) (finish-output)
             ))))
    (mt:thread-enable thread)
    (mt:thread-yield)
    thread
    ))

(defimplementation thread-id (thread)
  (block thread-id
    (mt:with-lock (*thread-id-map-lock*)
      (or (gethash thread *id-thread-map*)
          (let ((id (next-thread-id)))
            (setf (gethash id *thread-id-map*) thread)
            (setf (gethash thread *id-thread-map*) id)
            id)))))

(defimplementation find-thread (id)
  (mt:with-lock (*thread-id-map-lock*)
    (gethash id *thread-id-map*)))

(defimplementation thread-name (thread)
  (mt:thread-name thread))

(defimplementation thread-status (thread)
  (if (mt:thread-active-p thread)
      "RUNNING"
      "STOPPED"))

(defimplementation make-lock (&key name)
  (mt:make-lock :name name :recursive t))

(defimplementation call-with-lock-held (lock function)
  (declare (type function function))
  (mt:with-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mt:*thread*)

(defimplementation all-threads ()
  (mt:all-threads))

(defimplementation interrupt-thread (thread fn)
  (mt:interrupt-thread thread fn))

(defimplementation kill-thread (thread)
  (mt:interrupt-thread thread #'mt:terminate-thread)
  )

(defimplementation thread-alive-p (thread)
  (mt:thread-active-p thread))

(defvar *mailbox-lock* (mt:make-lock :name "mailbox lock"))
(defvar *mailboxes* (list))
(declaim (type list *mailboxes*))

(defstruct (mailbox (:conc-name mailbox.))
  thread
  locked-by
  (mutex (mt:make-lock :name "thread mailbox"))
  (semaphore (mt:make-semaphore))
  (queue '() :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (mt:with-lock (*mailbox-lock*)
    (or (find thread *mailboxes* :key #'mailbox.thread)
        (let ((mb (make-mailbox :thread thread)))
          (push mb *mailboxes*)
          mb))))

(defimplementation send (thread message)
  (handler-case
      (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
;;     (mt:interrupt-thread
;;      thread
;;      (lambda ()
;;        (mt:with-lock (mutex)
;;          (setf (mailbox.queue mbox)
;;                (nconc (mailbox.queue mbox) (list message))))))

;;     (format t "~&! thread = ~S~% thread = ~S~% message = ~S~%"
;;             mt:*thread* thread message) (finish-output)
    (mt:with-lock (mutex)
      (setf (mailbox.locked-by mbox) mt:*thread*)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      ;;(format t "*") (finish-output)
      (handler-case
          (mt:semaphore-signal (mailbox.semaphore mbox))
        (condition (condition)
          (format t "Something went bad with semaphore-signal ~A" condition) (finish-output)
          ;;(break)
          ))
      (setf (mailbox.locked-by mbox) nil)
      )
    ;;(format t "+") (finish-output)
    )
    (condition (condition)
      (format t "~&Error in send: ~S~%" condition) (finish-output))
    )
  )

;; (defimplementation receive ()
;;   (block got-mail
;;     (let* ((mbox (mailbox mt:*thread*))
;;            (mutex (mailbox.mutex mbox)))
;;       (loop
;;          (mt:with-lock (mutex)
;;            (if (mailbox.queue mbox)
;;                (return-from got-mail (pop (mailbox.queue mbox)))))
;;          ;;interrupt-thread will halt this if it takes longer than 1sec
;;          (sleep 1)))))


(defimplementation receive-if (test &optional timeout)
  (handler-case
  (let* ((mbox (mailbox (current-thread)))
         (mutex (mailbox.mutex mbox))
         got-one)
    (assert (or (not timeout) (eq timeout t)))
    (loop
       (check-slime-interrupts)
       ;;(format t "~&: ~S~%" mt:*thread*) (finish-output)
       (handler-case
        (setq got-one (mt:semaphore-wait (mailbox.semaphore mbox) 2))
        (condition (condition)
           (format t "~&In (swank-mkcl) receive-if: Something went bad with semaphore-wait ~A~%" condition)
           (finish-output)
           nil
           )
        )
       (mt:with-lock (mutex)
         (setf (mailbox.locked-by mbox) mt:*thread*)
         (let* ((q (mailbox.queue mbox))
                (tail (member-if test q)))
           (when tail 
             (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
             (setf (mailbox.locked-by mbox) nil)
             ;;(format t "~&thread ~S received: ~S~%" mt:*thread* (car tail))
             (return (car tail))))
         (setf (mailbox.locked-by mbox) nil)
         )

       ;;(format t "/ ~S~%" mt:*thread*) (finish-output)
       (when (eq timeout t) (return (values nil t)))
;;        (unless got-one
;;          (format t "~&In (swank-mkcl) receive-if: semaphore-wait timed out!~%"))
       )
    )
    (condition (condition)
      (format t "~&Error in (swank-mkcl) receive-if: ~S, ~A~%" condition condition) (finish-output)
      nil
      )
    )
  )


(defmethod stream-finish-output ((stream stream))
  (finish-output stream))


;;

;;#+windows
(defimplementation doze-in-repl ()
  (setq *inferior-lisp-sleeping-post* (mt:make-semaphore))
  ;;(loop (sleep 1))
  (mt:semaphore-wait *inferior-lisp-sleeping-post*)
  (mk-ext:quit :verbose t)
  )

