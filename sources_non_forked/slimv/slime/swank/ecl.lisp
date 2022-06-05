;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ecl.lisp --- SLIME backend for ECL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(defpackage swank/ecl
  (:use cl swank/backend))

(in-package swank/ecl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ecl-version ()
    (let ((version (find-symbol "+ECL-VERSION-NUMBER+" :EXT)))
      (if version
          (symbol-value version)
          0)))
  (when (< (ecl-version) 100301)
    (error "~&IMPORTANT:~%  ~
              The version of ECL you're using (~A) is too old.~%  ~
              Please upgrade to at least 10.3.1.~%  ~
              Sorry for the inconvenience.~%~%"
           (lisp-implementation-version))))

;; Hard dependencies.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sockets))

;; Soft dependencies.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (probe-file "sys:profile.fas")
    (require :profile)
    (pushnew :profile *features*))
  (when (probe-file "sys:serve-event.fas")
    (require :serve-event)
    (pushnew :serve-event *features*)))

(declaim (optimize (debug 3)))

;;; Swank-mop

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import-swank-mop-symbols
   :clos
   (and (< (ecl-version) 121201)
        `(:eql-specializer
          :eql-specializer-object
          :generic-function-declarations
          :specializer-direct-methods
          ,@(unless (fboundp 'clos:compute-applicable-methods-using-classes)
              '(:compute-applicable-methods-using-classes))))))

(defimplementation gray-package-name ()
  "GRAY")


;;;; UTF8

;;; Convert the string STRING to a (simple-array (unsigned-byte 8)).
;;;
;;;   string-to-utf8 (string)

;;; Convert the (simple-array (unsigned-byte 8)) OCTETS to a string.
;;;
;;;   utf8-to-string (octets)


;;;; TCP Server

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
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore timeout))
  (sb-bsd-sockets:socket-make-stream (accept socket)
                                     :output t
                                     :input t
                                     :buffering (ecase buffering
                                                  ((t) :full)
                                                  ((nil) :none)
                                                  (:line :line))
                                     :element-type (if external-format
                                                       'character 
                                                       '(unsigned-byte 8))
                                     :external-format external-format))

;;; Call FN whenever SOCKET is readable.
;;;
;;;   add-sigio-handler (socket fn)

;;; Remove all sigio handlers for SOCKET.
;;;
;;;   remove-sigio-handlers (socket)

;;; Call FN when Lisp is waiting for input and SOCKET is readable.
;;;
;;;   add-fd-handler (socket fn)

;;; Remove all fd-handlers for SOCKET.
;;;
;;;   remove-fd-handlers (socket)

(defimplementation preferred-communication-style ()
  (cond
    ((member :threads *features*) :spawn)
    ((member :windows *features*) nil)
    (t #|:fd-handler|# nil)))

;;; Set the 'stream 'timeout.  The timeout is either the real number
;;; specifying the timeout in seconds or 'nil for no timeout.
;;;
;;;   set-stream-timeout (stream timeout)


;;; Hook called when the first connection from Emacs is established.
;;; Called from the INIT-FN of the socket server that accepts the
;;; connection.
;;;
;;; This is intended for setting up extra context, e.g. to discover
;;; that the calling thread is the one that interacts with Emacs.
;;;
;;;   emacs-connected ()


;;;; Unix Integration

(defimplementation getpid ()
  (si:getpid))

;;; Call FUNCTION on SIGINT (instead of invoking the debugger).
;;; Return old signal handler.
;;;
;;;   install-sigint-handler (function)

;;; XXX!
;;; If ECL is built with thread support, it'll spawn a helper thread
;;; executing the SIGINT handler. We do not want to BREAK into that
;;; helper but into the main thread, though. This is coupled with the
;;; current choice of NIL as communication-style in so far as ECL's
;;; main-thread is also the Slime's REPL thread.

(defun make-interrupt-handler (real-handler)
  #+threads
  (let ((main-thread (find 'si:top-level (mp:all-processes)
                           :key #'mp:process-name)))
    #'(lambda (&rest args)
        (declare (ignore args))
        (mp:interrupt-process main-thread real-handler)))
  #-threads
  #'(lambda (&rest args)
      (declare (ignore args))
      (funcall real-handler)))

(defimplementation call-with-user-break-handler (real-handler function)
  (let ((old-handler #'si:terminal-interrupt))
    (setf (symbol-function 'si:terminal-interrupt)
          (make-interrupt-handler real-handler))
    (unwind-protect (funcall function)
      (setf (symbol-function 'si:terminal-interrupt) old-handler))))

(defimplementation quit-lisp ()
  (ext:quit))

;;; Default implementation is fine.
;;;
;;;   lisp-implementation-type-name
;;;   lisp-implementation-program

(defimplementation socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (two-way-stream (socket-fd (two-way-stream-input-stream socket)))
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (si:file-stream-fd socket))))

;;; Create a character stream for the file descriptor FD. This
;;; interface implementation requires either `ffi:c-inline' or has to
;;; wait for the exported interface.
;;;
;;;   make-fd-stream (socket-stream)

;;; Duplicate a file descriptor. If the syscall fails, signal a
;;; condition. See dup(2). This interface requiers `ffi:c-inline' or
;;; has to wait for the exported interface.
;;;
;;;   dup (fd)

;;; Does not apply to ECL which doesn't dump images.
;;;
;;;   exec-image (image-file args)

(defimplementation command-line-args ()
  (ext:command-args))


;;;; pathnames

;;; Return a pathname for FILENAME.
;;; A filename in Emacs may for example contain asterisks which should not
;;; be translated to wildcards.
;;;
;;;   filename-to-pathname (filename)

;;; Return the filename for PATHNAME.
;;;
;;;   pathname-to-filename (pathname)

(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation set-default-directory (directory)
  (ext:chdir (namestring directory)) ; adapts *DEFAULT-PATHNAME-DEFAULTS*.
  (default-directory))


;;; Call FN with hooks to handle special syntax. Can we use it for
;;; `ffi:c-inline' to be handled as C/C++ code?
;;;
;;;   call-with-syntax-hooks

;;; Return a suitable initial value for SWANK:*READTABLE-ALIST*.
;;;
;;;   default-readtable-alist


;;;; Packages

#+package-local-nicknames
(defimplementation package-local-nicknames (package)
  (ext:package-local-nicknames package))


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)

(defun signal-compiler-condition (&rest args)
  (apply #'signal 'compiler-condition args))

#-ecl-bytecmp
(defun handle-compiler-message (condition)
  ;; ECL emits lots of noise in compiler-notes, like "Invoking
  ;; external command".
  (unless (typep condition 'c::compiler-note)
    (signal-compiler-condition
     :original-condition condition
     :message (princ-to-string condition)
     :severity (etypecase condition
                 (c:compiler-fatal-error :error)
                 (c:compiler-error       :error)
                 (error                  :error)
                 (style-warning          :style-warning)
                 (warning                :warning))
     :location (condition-location condition))))

#-ecl-bytecmp
(defun condition-location (condition)
  (let ((file     (c:compiler-message-file condition))
        (position (c:compiler-message-file-position condition)))
    (if (and position (not (minusp position)))
        (if *buffer-name*
            (make-buffer-location *buffer-name*
                                  *buffer-start-position*
                                  position)
            (make-file-location file position))
        (make-error-location "No location found."))))

(defimplementation call-with-compilation-hooks (function)
  #+ecl-bytecmp
  (funcall function)
  #-ecl-bytecmp
  (handler-bind ((c:compiler-message #'handle-compiler-message))
    (funcall function)))

(defvar *tmpfile-map* (make-hash-table :test #'equal))

(defun note-buffer-tmpfile (tmp-file buffer-name)
  ;; EXT:COMPILED-FUNCTION-FILE below will return a namestring.
  (let ((tmp-namestring (namestring (truename tmp-file))))
    (setf (gethash tmp-namestring *tmpfile-map*) buffer-name)
    tmp-namestring))

(defun tmpfile-to-buffer (tmp-file)
  (gethash tmp-file *tmpfile-map*))

(defimplementation swank-compile-string
    (string &key buffer position filename line column policy)
  (declare (ignore line column policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)        ; for compilation hooks
          (*buffer-start-position* position))
      (let ((tmp-file (si:mkstemp "TMP:ecl-swank-tmpfile-"))
            (fasl-file)
            (warnings-p)
            (failure-p))
        (unwind-protect
             (with-open-file (tmp-stream tmp-file :direction :output
                                         :if-exists :supersede)
               (write-string string tmp-stream)
               (finish-output tmp-stream)
               (multiple-value-setq (fasl-file warnings-p failure-p)
                 (compile-file tmp-file
                               :load t
                               :source-truename (or filename
                                                    (note-buffer-tmpfile tmp-file buffer))
                               :source-offset (1- position))))
          (when (probe-file tmp-file)
            (delete-file tmp-file))
          (when fasl-file
            (delete-file fasl-file)))
        (not failure-p)))))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (compile-file input-file :output-file output-file
                  :load load-p
                  :external-format external-format)))

(defvar *external-format-to-coding-system*
  '((:latin-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix"
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defun external-format (coding-system)
  (or (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                      *external-format-to-coding-system*))
      (find coding-system (ext:all-encodings) :test #'string-equal)))

(defimplementation find-external-format (coding-system)
  #+unicode (external-format coding-system)
  ;; Without unicode support, ECL uses the one-byte encoding of the
  ;; underlying OS, and will barf on anything except :DEFAULT.  We
  ;; return NIL here for known multibyte encodings, so
  ;; SWANK:CREATE-SERVER will barf.
  #-unicode (let ((xf (external-format coding-system)))
              (if (member xf '(:utf-8))
                  nil
                  :default)))


;;; Default implementation is fine
;;;
;;;   guess-external-format


;;;; Streams

;;; Implemented in `gray'
;;;
;;;   make-output-stream
;;;   make-input-stream


;;;; Documentation

(defimplementation arglist (name)
  (multiple-value-bind (arglist foundp)
      (ext:function-lambda-list name)
    (if foundp arglist :not-available)))

(defimplementation type-specifier-p (symbol)
  (or (subtypep nil symbol)
      (not (eq (type-specifier-arglist symbol) :not-available))))

(defimplementation function-name (f)
  (typecase f
    (generic-function (clos:generic-function-name f))
    (function (si:compiled-function-name f))))

;;; Default implementation is fine (CL).
;;; 
;;; valid-function-name-p (form)

#+walker
(defimplementation macroexpand-all (form &optional env)
  (walker:macroexpand-all form env))

;;; Default implementation is fine.
;;;
;;;   compiler-macroexpand-1
;;;   compiler-macroexpand

(defimplementation collect-macro-forms (form &optional env)
  ;; Currently detects only normal macros, not compiler macros.
  (declare (ignore env))
  (with-collected-macro-forms (macro-forms)
    (handler-bind ((warning #'muffle-warning))
      (ignore-errors
        (compile nil `(lambda () ,form))))
    (values macro-forms nil)))

;;; Expand the format string CONTROL-STRING.
;;; Default implementation is fine.
;;;
;;;   format-string-expand

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((frob (type boundp)
             (when (funcall boundp symbol)
               (let ((doc (describe-definition symbol type)))
                 (setf result (list* type doc result))))))
      (frob :VARIABLE #'boundp)
      (frob :FUNCTION #'fboundp)
      (frob :CLASS (lambda (x) (find-class x nil))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))


;;;; Debugging

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(defun make-invoke-debugger-hook (hook)
  (when hook
    #'(lambda (condition old-hook)
        ;; Regard *debugger-hook* if set by user.
        (if *debugger-hook*
            nil         ; decline, *DEBUGGER-HOOK* will be tried next.
            (funcall hook condition old-hook)))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq ext:*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (ext:*invoke-debugger-hook* (make-invoke-debugger-hook hook)))
    (funcall fun)))

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
  (pathname-match-p
   name
   (make-pathname :defaults swank-loader::*source-directory*
                  :name (pathname-name name)
                  :type (pathname-type name)
                  :version (pathname-version name))))

(defun is-ignorable-fun-p (x)
  (or
   (in-swank-package-p (frame-name x))
   (multiple-value-bind (file position)
       (ignore-errors (si::bc-file (car x)))
     (declare (ignore position))
     (if file (is-swank-source-p file)))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*ihs-top* (ihs-top))
         (*ihs-current* *ihs-top*)
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*tpl-level* (1+ *tpl-level*))
         (*backtrace* (loop for ihs from 0 below *ihs-top*
                            collect (list (si::ihs-fun ihs)
                                          (si::ihs-env ihs)
                                          nil))))
    (declare (special *ihs-current*))
    (loop for f from *frs-base* until *frs-top*
          do (let ((i (- (si::frs-ihs f) *ihs-base* 1)))
               (when (plusp i)
                 (let* ((x (elt *backtrace* i))
                        (name (si::frs-tag f)))
                   (unless (si::fixnump name)
                     (push name (third x)))))))
    (setf *backtrace* (remove-if #'is-ignorable-fun-p (nreverse *backtrace*)))
    (set-break-env)
    (set-current-ihs)
    (let ((*ihs-base* *ihs-top*))
      (funcall debugger-loop-fn))))

(defimplementation compute-backtrace (start end)
  (subseq *backtrace* start
          (and (numberp end)
               (min end (length *backtrace*)))))

(defun frame-name (frame)
  (let ((x (first frame)))
    (if (symbolp x)
        x
        (function-name x))))

(defun function-position (fun)
  (multiple-value-bind (file position)
      (si::bc-file fun)
    (when file
      (make-file-location file position))))

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
    (dolist (record (remove-if-not #'consp frame))
      (let* ((record0 (car record))
	     (record1 (cdr record)))
	(cond ((or (symbolp record0) (stringp record0))
	       (setq variables (acons record0 record1 variables)))
	      ((not (si::fixnump record0))
	       (push record1 functions))
	      ((symbolp record1)
	       (push record1 blocks))
	      (t
	       ))))
    (values functions blocks variables)))

(defimplementation print-frame (frame stream)
  (format stream "~A" (first frame)))

;;; Is the frame FRAME restartable?.
;;; Return T if `restart-frame' can safely be called on the frame.
;;;
;;; frame-restartable-p (frame)

(defimplementation frame-source-location (frame-number)
  (let ((frame (elt *backtrace* frame-number)))
    (or (nth-value 1 (frame-function frame))
        (make-error-location "Unknown source location for ~A." (car frame)))))

(defimplementation frame-catch-tags (frame-number)
  (third (elt *backtrace* frame-number)))

(defimplementation frame-locals (frame-number)
  (loop for (name . value) in (nth-value 2 (frame-decode-env
                                            (elt *backtrace* frame-number)))
        collect (list :name name :id 0 :value value)))

(defimplementation frame-var-value (frame-number var-number)
  (destructuring-bind (name . value)
      (elt
       (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
       var-number)
    (declare (ignore name))
    value))

(defimplementation disassemble-frame (frame-number)
  (let ((fun (frame-function (elt *backtrace* frame-number))))
    (disassemble fun)))

(defimplementation eval-in-frame (form frame-number)
  (let ((env (second (elt *backtrace* frame-number))))
    (si:eval-with-env form env)))

;;; frame-package
;;; frame-call
;;; return-from-frame
;;; restart-frame
;;; print-condition
;;; condition-extras

(defimplementation gdb-initial-commands ()
  ;; These signals are used by the GC.
  #+linux '("handle SIGPWR  noprint nostop"
            "handle SIGXCPU noprint nostop"))

;;; active-stepping
;;; sldb-break-on-return
;;; sldb-break-at-start
;;; sldb-stepper-condition-p
;;; sldb-setp-into
;;; sldb-step-next
;;; sldb-step-out


;;;; Definition finding

(defvar +TAGS+ (namestring
                (merge-pathnames "TAGS" (translate-logical-pathname "SYS:"))))

(defun make-file-location (file file-position)
  ;; File positions in CL start at 0, but Emacs' buffer positions
  ;; start at 1. We specify (:ALIGN T) because the positions comming
  ;; from ECL point at right after the toplevel form appearing before
  ;; the actual target toplevel form; (:ALIGN T) will DTRT in that case.
  (make-location `(:file ,(namestring (translate-logical-pathname file)))
                 `(:position ,(1+ file-position))
                 `(:align t)))

(defun make-buffer-location (buffer-name start-position &optional (offset 0))
  (make-location `(:buffer ,buffer-name)
                 `(:offset ,start-position ,offset)
                 `(:align t)))

(defun make-TAGS-location (&rest tags)
  (make-location `(:etags-file ,+TAGS+)
                 `(:tag ,@tags)))

(defimplementation find-definitions (name)
  (let ((annotations (ext:get-annotation name 'si::location :all)))
    (cond (annotations
           (loop for annotation in annotations
                 collect (destructuring-bind (dspec file . pos) annotation
                           `(,dspec ,(make-file-location file pos)))))
          (t
           (mapcan #'(lambda (type) (find-definitions-by-type name type))
                   (classify-definition-name name))))))

(defun classify-definition-name (name)
  (let ((types '()))
    (when (fboundp name)
      (cond ((special-operator-p name)
             (push :special-operator types))
            ((macro-function name)
             (push :macro types))
            ((typep (fdefinition name) 'generic-function)
             (push :generic-function types))
            ((si:mangle-name name t)
             (push :c-function types))
            (t
             (push :lisp-function types))))
    (when (boundp name)
      (cond ((constantp name)
             (push :constant types))
            (t
             (push :global-variable types))))
    types))

(defun find-definitions-by-type (name type)
  (ecase type
    (:lisp-function
     (when-let (loc (source-location (fdefinition name)))
       (list `((defun ,name) ,loc))))
    (:c-function
     (when-let (loc (source-location (fdefinition name)))
       (list `((c-source ,name) ,loc))))
    (:generic-function
     (loop for method in (clos:generic-function-methods (fdefinition name))
           for specs = (clos:method-specializers method)
           for loc   = (source-location method)
           when loc
             collect `((defmethod ,name ,specs) ,loc)))
    (:macro
     (when-let (loc (source-location (macro-function name)))
       (list `((defmacro ,name) ,loc))))
    (:constant
     (when-let (loc (source-location name))
       (list `((defconstant ,name) ,loc))))
    (:global-variable
     (when-let (loc (source-location name))
       (list `((defvar ,name) ,loc))))
    (:special-operator)))

;;; FIXME: There ought to be a better way.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c-function-name-p (name)
    (and (symbolp name) (si:mangle-name name t) t))
  (defun c-function-p (object)
    (and (functionp object)
         (let ((fn-name (function-name object)))
           (and fn-name (c-function-name-p fn-name))))))

(deftype c-function ()
  `(satisfies c-function-p))

(defun assert-source-directory ()
  (unless (probe-file #P"SRC:")
    (error "ECL's source directory ~A does not exist. ~
            You can specify a different location via the environment ~
            variable `ECLSRCDIR'."
           (namestring (translate-logical-pathname #P"SYS:"))))) 

(defun assert-TAGS-file ()
  (unless (probe-file +TAGS+)
    (error "No TAGS file ~A found. It should have been installed with ECL."
           +TAGS+)))

(defun package-names (package)
  (cons (package-name package) (package-nicknames package)))

(defun source-location (object)
  (converting-errors-to-error-location
   (typecase object
     (c-function
      (assert-source-directory)
      (assert-TAGS-file)
      (let ((lisp-name (function-name object)))
        (assert lisp-name)
        (multiple-value-bind (flag c-name) (si:mangle-name lisp-name t)
          (assert flag)
          ;; In ECL's code base sometimes the mangled name is used
          ;; directly, sometimes ECL's DPP magic of @SI::SYMBOL or
          ;; @EXT::SYMBOL is used. We cannot predict here, so we just
          ;; provide several candidates.
          (apply #'make-TAGS-location
                 c-name
                 (loop with s = (symbol-name lisp-name)
                       for p in (package-names (symbol-package lisp-name))
                       collect (format nil "~A::~A" p s)
                       collect (format nil "~(~A::~A~)" p s))))))
     (function
      (multiple-value-bind (file pos) (ext:compiled-function-file object)
        (cond ((not file)
               (return-from source-location nil))
              ((tmpfile-to-buffer file)
               (make-buffer-location (tmpfile-to-buffer file) pos))
              (t
               (assert (probe-file file))
               (assert (not (minusp pos)))
               (make-file-location file pos)))))
     (method
      ;; FIXME: This will always return NIL at the moment; ECL does not
      ;; store debug information for methods yet.
      (source-location (clos:method-function object)))
     ((member nil t)
      (multiple-value-bind (flag c-name) (si:mangle-name object)
        (assert flag)
        (make-TAGS-location c-name))))))

(defimplementation find-source-location (object)
  (or (source-location object)
      (make-error-location "Source definition of ~S not found." object)))

;;; buffer-first-change


;;;; XREF

;;; who-calls
;;; calls-who
;;; who-references
;;; who-binds
;;; who-sets
;;; who-macroexpands
;;; who-specializes
;;; list-callers
;;; list-callees


;;;; Profiling

;;; XXX: use monitor.lisp (ccl,clisp)

#+profile
(progn

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
) ; #+profile (progn ...


;;;; Trace

;;; Toggle tracing of the function(s) given with SPEC.
;;; SPEC can be:
;;;  (setf NAME)                            ; a setf function
;;;  (:defmethod NAME QUALIFIER... (SPECIALIZER...)) ; a specific method
;;;  (:defgeneric NAME)                     ; a generic function with all methods
;;;  (:call CALLER CALLEE)                  ; trace calls from CALLER to CALLEE.
;;;  (:labels TOPLEVEL LOCAL)
;;;  (:flet TOPLEVEL LOCAL) 
;;;
;;;   toggle-trace (spec)


;;;; Inspector

;;; FIXME: Would be nice if it was possible to inspect objects
;;; implemented in C.

;;; Return a list of bindings corresponding to OBJECT's slots.
;;;   eval-context (object)

;;; Return a string describing the primitive type of object.
;;;   describe-primitive-type (object)


;;;; Multithreading

;;; Not needed in ECL
;;;
;;;   initialize-multiprocessing

#+threads
(progn
  (defvar *thread-id-counter* 0)

  (defparameter *thread-id-map* (make-hash-table))

  (defvar *thread-id-map-lock*
    (mp:make-lock :name "thread id map lock"))

  (defimplementation spawn (fn &key name)
    (mp:process-run-function name fn))

  (defimplementation thread-id (target-thread)
    (block thread-id
      (mp:with-lock (*thread-id-map-lock*)
        ;; Does TARGET-THREAD have an id already?
        (maphash (lambda (id thread-pointer)
                   (let ((thread (si:weak-pointer-value thread-pointer)))
                     (cond ((not thread)
                            (remhash id *thread-id-map*))
                           ((eq thread target-thread)
                            (return-from thread-id id)))))
                 *thread-id-map*)
        ;; TARGET-THREAD not found in *THREAD-ID-MAP*
        (let ((id (incf *thread-id-counter*))
              (thread-pointer (si:make-weak-pointer target-thread)))
          (setf (gethash id *thread-id-map*) thread-pointer)
          id))))

  (defimplementation find-thread (id)
    (mp:with-lock (*thread-id-map-lock*)
      (let* ((thread-ptr (gethash id *thread-id-map*))
             (thread (and thread-ptr (si:weak-pointer-value thread-ptr))))
        (unless thread
          (remhash id *thread-id-map*))
        thread)))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (if (mp:process-active-p thread)
        "RUNNING"
        "STOPPED"))

  ;; thread-attributes

  (defimplementation current-thread ()
    mp:*current-process*)

  (defimplementation all-threads ()
    (mp:all-processes))

  (defimplementation thread-alive-p (thread)
    (mp:process-active-p thread))

  (defimplementation interrupt-thread (thread fn)
    (mp:interrupt-process thread fn))

  (defimplementation kill-thread (thread)
    (mp:process-kill thread))

  (defvar *mailbox-lock* (mp:make-lock :name "mailbox lock"))
  (defvar *mailboxes* (list))
  (declaim (type list *mailboxes*))

  (defstruct (mailbox (:conc-name mailbox.))
    thread
    (mutex (mp:make-lock))
    (cvar  (mp:make-condition-variable))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (mp:with-lock (mutex)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (mp:condition-variable-broadcast (mailbox.cvar mbox)))))

  ;; receive

  (defimplementation receive-if (test &optional timeout)
    (let* ((mbox (mailbox (current-thread)))
           (mutex (mailbox.mutex mbox)))
      (assert (or (not timeout) (eq timeout t)))
      (loop
         (check-slime-interrupts)
         (mp:with-lock (mutex)
           (let* ((q (mailbox.queue mbox))
                  (tail (member-if test q)))
             (when tail
               (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
               (return (car tail))))
           (when (eq timeout t) (return (values nil t)))
           (mp:condition-variable-wait (mailbox.cvar mbox) mutex)))))

  ;; Trigger a call to CHECK-SLIME-INTERRUPTS in THREAD without using
  ;; asynchronous interrupts.
  ;;
  ;; Doesn't have to implement this if RECEIVE-IF periodically calls
  ;; CHECK-SLIME-INTERRUPTS, but that's energy inefficient.
  ;;
  ;;   wake-thread (thread)

  ;; Copied from sbcl.lisp and adjusted to ECL.
  (let ((alist '())
        (mutex (mp:make-lock :name "register-thread")))

    (defimplementation register-thread (name thread)
      (declare (type symbol name))
      (mp:with-lock (mutex)
        (etypecase thread
          (null
           (setf alist (delete name alist :key #'car)))
          (mp:process
           (let ((probe (assoc name alist)))
             (cond (probe (setf (cdr probe) thread))
                   (t (setf alist (acons name thread alist))))))))
      nil)

    (defimplementation find-registered (name)
      (mp:with-lock (mutex)
        (cdr (assoc name alist)))))

  ;; Not needed in ECL (?).
  ;;
  ;;   set-default-initial-binding (var form)

  ) ; #+threads

;;; Instead of busy waiting with communication-style NIL, use select()
;;; on the sockets' streams.
#+serve-event
(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (flet ((poll-streams (streams timeout)
           (let* ((serve-event::*descriptor-handlers*
                   (copy-list serve-event::*descriptor-handlers*))
                  (active-fds '())
                  (fd-stream-alist
                   (loop for s in streams
                      for fd = (socket-fd s)
                      collect (cons fd s)
                      do (serve-event:add-fd-handler fd :input
                                                     #'(lambda (fd)
                                                         (push fd active-fds))))))
             (serve-event:serve-event timeout)
             (loop for fd in active-fds collect (cdr (assoc fd fd-stream-alist))))))
    (loop
       (cond ((check-slime-interrupts) (return :interrupt))
             (timeout (return (poll-streams streams 0)))
             (t
              (when-let (ready (poll-streams streams 0.2))
                (return ready)))))))

#-serve-event
(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (loop
   (cond ((check-slime-interrupts) (return :interrupt))
         (timeout (return (remove-if-not #'listen streams)))
         (t
          (let ((ready (remove-if-not #'listen streams)))
            (if ready (return ready))
            (sleep 0.1))))))


;;;; Locks

#+threads
(defimplementation make-lock (&key name)
  (mp:make-lock :name name :recursive t))

(defimplementation call-with-lock-held (lock function)
  (declare (type function function))
  (mp:with-lock (lock) (funcall function)))


;;;; Weak datastructures

;;; XXX: this should work but causes SLIME REPL hang at some point of time. May
;;; be ECL or SLIME bug - disabling for now.
#+(and ecl-weak-hash (or))
(progn
  (defimplementation make-weak-key-hash-table (&rest args)
    (apply #'make-hash-table :weakness :key args))

  (defimplementation make-weak-value-hash-table (&rest args)
    (apply #'make-hash-table :weakness :value args))

  (defimplementation hash-table-weakness (hashtable)
    (ext:hash-table-weakness hashtable)))


;;;; Character names

;;; Default implementation is fine.
;;;
;;;   character-completion-set (prefix matchp)


;;;; Heap dumps

;;; Doesn't apply to ECL.
;;;
;;;   save-image (filename &optional restart-function)
;;;   background-save-image (filename &key restart-function completion-function)


;;;; Wrapping

;;; Intercept future calls to SPEC and surround them in callbacks.
;;; Very much similar to so-called advices for normal functions.
;;;
;;;   wrap (spec indicator &key before after replace)
;;;   unwrap (spec indicator)
;;;   wrapped-p (spec indicator)
