;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;+" -*-
;;;
;;; Scieneer Common Lisp code for SLIME.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage swank/scl
  (:use cl swank/backend swank/source-path-parser swank/source-file-cache))

(in-package swank/scl)



;;; swank-mop

(import-swank-mop-symbols :clos '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))


;;;; TCP server
;;;
;;; SCL only supports the :spawn communication style.
;;;

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port &key backlog)
  (let ((addr (resolve-hostname host)))
    (ext:create-inet-listener port :stream :host addr :reuse-address t
                              :backlog (or backlog 5))))

(defimplementation local-port (socket)
  (nth-value 1 (ext::get-socket-host-and-port (socket-fd socket))))

(defimplementation close-socket (socket)
  (ext:close-socket (socket-fd socket)))

(defimplementation accept-connection (socket 
                                      &key external-format buffering timeout)
  (let ((buffering (or buffering :full))
        (fd (socket-fd socket)))
      (loop
       (let ((ready (sys:wait-until-fd-usable fd :input timeout)))
         (unless ready
           (error "Timeout accepting connection on socket: ~S~%" socket)))
       (let ((new-fd (ignore-errors (ext:accept-tcp-connection fd))))
         (when new-fd
           (return (make-socket-io-stream new-fd external-format 
                                          (ecase buffering
                                            ((t) :full)
                                            ((nil) :none)
                                            (:line :line)))))))))

(defimplementation set-stream-timeout (stream timeout)
  (check-type timeout (or null real))
  (if (fboundp 'ext::stream-timeout)
      (setf (ext::stream-timeout stream) timeout)
      (setf (slot-value (slot-value stream 'lisp::stream) 'lisp::timeout)
            timeout)))

;;;;; Sockets

(defun socket-fd (socket)
  "Return the file descriptor for the socket represented by 'socket."
  (etypecase socket
    (fixnum socket)
    (stream (sys:fd-stream-fd socket))))

(defun resolve-hostname (hostname)
  "Return the IP address of 'hostname as an integer (in host byte-order)."
  (let ((hostent (ext:lookup-host-entry hostname)))
    (car (ext:host-entry-addr-list hostent))))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")
    (:euc-jp "euc-jp" "euc-jp-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

(defun make-socket-io-stream (fd external-format buffering)
  "Create a new input/output fd-stream for 'fd."
  (cond ((not external-format)
         (sys:make-fd-stream fd :input t :output t :buffering buffering
                             :element-type '(unsigned-byte 8)))
        (t
         (let* ((stream (sys:make-fd-stream fd :input t :output t
                                            :element-type 'base-char
                                            :buffering buffering
                                            :external-format external-format)))
           ;; Ignore character conversion errors.  Without this the
           ;; communication channel is prone to lockup if a character
           ;; conversion error occurs.
           (setf (lisp::character-conversion-stream-input-error-value stream)
                 #\?)
           (setf (lisp::character-conversion-stream-output-error-value stream)
                 #\?)
           stream))))


;;;; Stream handling

(defimplementation gray-package-name ()
  '#:ext)


;;;; Compilation Commands

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defvar *previous-context* nil
  "Previous compiler error context.")

(defvar *buffer-name* nil
  "The name of the Emacs buffer we are compiling from.
  Nil if we aren't compiling from a buffer.")

(defvar *buffer-start-position* nil)
(defvar *buffer-substring* nil)

(defimplementation call-with-compilation-hooks (function)
  (let ((*previous-compiler-condition* nil)
        (*previous-context* nil)
        (*print-readably* nil))
    (handler-bind ((c::compiler-error #'handle-notification-condition)
                   (c::style-warning  #'handle-notification-condition)
                   (c::warning        #'handle-notification-condition))
      (funcall function))))

(defimplementation swank-compile-file (input-file output-file 
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* nil)
          (ext:*ignore-extra-close-parentheses* nil))
      (multiple-value-bind (output-file warnings-p failure-p)
          (compile-file input-file 
                        :output-file output-file
                        :external-format external-format)
        (values output-file warnings-p
                (or failure-p
                    (when load-p
                      ;; Cache the latest source file for definition-finding.
                      (source-cache-get input-file 
                                        (file-write-date input-file))
                      (not (load output-file)))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore filename line column policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-substring* string))
      (with-input-from-string (stream string)
        (ext:compile-from-stream 
         stream 
         :source-info `(:emacs-buffer ,buffer 
                        :emacs-buffer-offset ,position
                        :emacs-buffer-string ,string))))))


;;;;; Trapping notes
;;;
;;; We intercept conditions from the compiler and resignal them as
;;; `swank:compiler-condition's.

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (unless (eq condition *previous-compiler-condition*)
    (let ((context (c::find-error-context nil)))
      (setq *previous-compiler-condition* condition)
      (setq *previous-context* context)
      (signal-compiler-condition condition context))))

(defun signal-compiler-condition (condition context)
  (signal 'compiler-condition
          :original-condition condition
          :severity (severity-for-emacs condition)
          :message (brief-compiler-message-for-emacs condition)
          :source-context (compiler-error-context context)
          :location (if (read-error-p condition)
                        (read-error-location condition)
                        (compiler-note-location context))))

(defun severity-for-emacs (condition)
  "Return the severity of 'condition."
  (etypecase condition
    ((satisfies read-error-p) :read-error)
    (c::compiler-error :error)
    (c::style-warning :note)
    (c::warning :warning)))

(defun read-error-p (condition)
  (eq (type-of condition) 'c::compiler-read-error))

(defun brief-compiler-message-for-emacs (condition)
  "Briefly describe a compiler error for Emacs.
  When Emacs presents the message it already has the source popped up
  and the source form highlighted. This makes much of the information in
  the error-context redundant."
  (princ-to-string condition))

(defun compiler-error-context (error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (c::compiler-error-context-enclosing-source error-context)
                  (c::compiler-error-context-source error-context)))
    (if (and enclosing source)
        (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~^~%~}~]"
                enclosing source))))

(defun read-error-location (condition)
  (let* ((finfo (car (c::source-info-current-file c::*source-info*)))
         (file (c::file-info-name finfo))
         (pos (c::compiler-read-error-position condition)))
    (cond ((and (eq file :stream) *buffer-name*)
           (make-location (list :buffer *buffer-name*)
                          (list :offset *buffer-start-position* pos)))
          ((and (pathnamep file) (not *buffer-name*))
           (make-location (list :file (unix-truename file))
                          (list :position (1+ pos))))
          (t (break)))))

(defun compiler-note-location (context)
  "Derive the location of a complier message from its context.
  Return a `location' record, or (:error <reason>) on failure."
  (if (null context)
      (note-error-location)
      (let ((file (c::compiler-error-context-file-name context))
            (source (c::compiler-error-context-original-source context))
            (path
             (reverse 
              (c::compiler-error-context-original-source-path context))))
        (or (locate-compiler-note file source path)
            (note-error-location)))))

(defun note-error-location ()
  "Pseudo-location for notes that can't be located."
  (list :error "No error location available."))

(defun locate-compiler-note (file source source-path)
  (cond ((and (eq file :stream) *buffer-name*)
         ;; Compiling from a buffer
	 (make-location (list :buffer *buffer-name*)
			(list :offset *buffer-start-position*
			      (source-path-string-position
			       source-path *buffer-substring*))))
        ((and (pathnamep file) (null *buffer-name*))
         ;; Compiling from a file
         (make-location (list :file (unix-truename file))
                        (list :position (1+ (source-path-file-position
					     source-path file)))))
        ((and (eq file :lisp) (stringp source))
         ;; No location known, but we have the source form.
         ;; XXX How is this case triggered?  -luke (16/May/2004) 
         ;; This can happen if the compiler needs to expand a macro
         ;; but the macro-expander is not yet compiled.  Calling the
         ;; (interpreted) macro-expander triggers IR1 conversion of
         ;; the lambda expression for the expander and invokes the
         ;; compiler recursively.
         (make-location (list :source-form source)
                        (list :position 1)))))

(defun unix-truename (pathname)
  (ext:unix-namestring (truename pathname)))



;;; TODO
(defimplementation who-calls (name) nil)
(defimplementation who-references (name) nil)
(defimplementation who-binds (name) nil)
(defimplementation who-sets (name) nil)
(defimplementation who-specializes (symbol) nil)
(defimplementation who-macroexpands (name) nil)


;;;; Find callers and callees
;;;
;;; Find callers and callees by looking at the constant pool of
;;; compiled code objects.  We assume every fdefn object in the
;;; constant pool corresponds to a call to that function.  A better
;;; strategy would be to use the disassembler to find actual
;;; call-sites.

(declaim (inline map-code-constants))
(defun map-code-constants (code fn)
  "Call 'fn for each constant in 'code's constant pool."
  (check-type code kernel:code-component)
  (loop for i from vm:code-constants-offset below (kernel:get-header-data code)
	do (funcall fn (kernel:code-header-ref code i))))

(defun function-callees (function)
  "Return 'function's callees as a list of functions."
  (let ((callees '()))
    (map-code-constants 
     (vm::find-code-object function)
     (lambda (obj)
       (when (kernel:fdefn-p obj)
	 (push (kernel:fdefn-function obj) callees))))
    callees))

(declaim (ext:maybe-inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of 'spaces.  FN
  receives the object as argument.  'spaces should be a list of the
  symbols :dynamic, :static, or :read-only."
  (dolist (space spaces)
    (declare (inline vm::map-allocated-objects)
             (optimize (ext:inhibit-warnings 3)))
    (vm::map-allocated-objects
     (lambda (obj header size)
       (declare (type fixnum size) (ignore size))
       (when (= vm:code-header-type header)
	 (funcall fn obj)))
     space)))

(declaim (ext:maybe-inline map-caller-code-components))
(defun map-caller-code-components (function spaces fn)
  "Call 'fn for each code component with a fdefn for 'function in its
  constant pool."
  (let ((function (coerce function 'function)))
    (declare (inline map-allocated-code-components))
    (map-allocated-code-components
     spaces 
     (lambda (obj)
       (map-code-constants 
	obj 
	(lambda (constant)
	  (when (and (kernel:fdefn-p constant)
		     (eq (kernel:fdefn-function constant)
			 function))
	    (funcall fn obj))))))))

(defun function-callers (function &optional (spaces '(:read-only :static 
						      :dynamic)))
  "Return 'function's callers.  The result is a list of code-objects."
  (let ((referrers '()))
    (declare (inline map-caller-code-components))
    (map-caller-code-components function spaces 
                                (lambda (code) (push code referrers)))
    referrers))

(defun debug-info-definitions (debug-info)
  "Return the defintions for a debug-info.  This should only be used
  for code-object without entry points, i.e., byte compiled
  code (are theree others?)"
  ;; This mess has only been tested with #'ext::skip-whitespace, a
  ;; byte-compiled caller of #'read-char .
  (check-type debug-info (and (not c::compiled-debug-info) c::debug-info))
  (let ((name (c::debug-info-name debug-info))
        (source (c::debug-info-source debug-info)))
    (destructuring-bind (first) source 
      (ecase (c::debug-source-from first)
        (:file 
         (list (list name
                     (make-location 
                      (list :file (unix-truename (c::debug-source-name first)))
                      (list :function-name (string name))))))))))

(defun valid-function-name-p (name)
  (or (symbolp name) (and (consp name)
                          (eq (car name) 'setf)
                          (symbolp (cadr name))
                          (not (cddr name)))))

(defun code-component-entry-points (code)
  "Return a list ((name location) ...) of function definitons for
  the code omponent 'code."
  (let ((names '()))
    (do ((f (kernel:%code-entry-points code) (kernel::%function-next f)))
        ((not f))
      (let ((name (kernel:%function-name f)))
        (when (valid-function-name-p name)
          (push (list name (function-location f)) names))))
    names))

(defimplementation list-callers (symbol)
  "Return a list ((name location) ...) of callers."
  (let ((components (function-callers symbol))
        (xrefs '()))
    (dolist (code components)
      (let* ((entry (kernel:%code-entry-points code))
             (defs (if entry
                       (code-component-entry-points code)
                       ;; byte compiled stuff
                       (debug-info-definitions 
                        (kernel:%code-debug-info code)))))
        (setq xrefs (nconc defs xrefs))))
    xrefs))

(defimplementation list-callees (symbol)
  (let ((fns (function-callees symbol)))
    (mapcar (lambda (fn)
              (list (kernel:%function-name fn)
                    (function-location fn)))
            fns)))


;;;; Resolving source locations
;;;
;;; Our mission here is to "resolve" references to code locations into
;;; actual file/buffer names and character positions. The references
;;; we work from come out of the compiler's statically-generated debug
;;; information, such as `code-location''s and `debug-source''s. For
;;; more details, see the "Debugger Programmer's Interface" section of
;;; the SCL manual.
;;;
;;; The first step is usually to find the corresponding "source-path"
;;; for the location. Once we have the source-path we can pull up the
;;; source file and `READ' our way through to the right position. The
;;; main source-code groveling work is done in
;;; `source-path-parser.lisp'.

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
  This is useful when debugging the definition-finding code.")

(defmacro safe-definition-finding (&body body)
  "Execute 'body and return the source-location it returns.
  If an error occurs and `*debug-definition-finding*' is false, then
  return an error pseudo-location.

  The second return value is 'nil if no error occurs, otherwise it is the
  condition object."
  `(flet ((body () ,@body))
    (if *debug-definition-finding*
        (body)
        (handler-case (values (progn ,@body) nil)
          (error (c) (values (list :error (princ-to-string c)) c))))))

(defun code-location-source-location (code-location)
  "Safe wrapper around `code-location-from-source-location'."
  (safe-definition-finding
   (source-location-from-code-location code-location)))

(defun source-location-from-code-location (code-location)
  "Return the source location for 'code-location."
  (let ((debug-fun (di:code-location-debug-function code-location)))
    (when (di::bogus-debug-function-p debug-fun)
      ;; Those lousy cheapskates! They've put in a bogus debug source
      ;; because the code was compiled at a low debug setting.
      (error "Bogus debug function: ~A" debug-fun)))
  (let* ((debug-source (di:code-location-debug-source code-location))
         (from (di:debug-source-from debug-source))
         (name (di:debug-source-name debug-source)))
    (ecase from
      (:file 
       (location-in-file name code-location debug-source))
      (:stream
       (location-in-stream code-location debug-source))
      (:lisp
       ;; The location comes from a form passed to `compile'.
       ;; The best we can do is return the form itself for printing.
       (make-location
        (list :source-form (with-output-to-string (*standard-output*)
                             (debug::print-code-location-source-form 
                              code-location 100 t)))
        (list :position 1))))))

(defun location-in-file (filename code-location debug-source)
  "Resolve the source location for 'code-location in 'filename."
  (let* ((code-date (di:debug-source-created debug-source))
         (source-code (get-source-code filename code-date)))
    (with-input-from-string (s source-code)
      (make-location (list :file (unix-truename filename))
                     (list :position (1+ (code-location-stream-position
					  code-location s)))
                     `(:snippet ,(read-snippet s))))))

(defun location-in-stream (code-location debug-source)
  "Resolve the source location for a 'code-location from a stream.
  This only succeeds if the code was compiled from an Emacs buffer."
  (unless (debug-source-info-from-emacs-buffer-p debug-source)
    (error "The code is compiled from a non-SLIME stream."))
  (let* ((info (c::debug-source-info debug-source))
         (string (getf info :emacs-buffer-string))
         (position (code-location-string-offset 
                    code-location
                    string)))
    (make-location
     (list :buffer (getf info :emacs-buffer))
     (list :offset (getf info :emacs-buffer-offset) position)
     (list :snippet (with-input-from-string (s string)
                      (file-position s position)
                      (read-snippet s))))))

;;;;; Function-name locations
;;;
(defun debug-info-function-name-location (debug-info)
  "Return a function-name source-location for 'debug-info.
  Function-name source-locations are a fallback for when precise
  positions aren't available."
  (with-struct (c::debug-info- (fname name) source) debug-info
    (with-struct (c::debug-source- info from name) (car source)
      (ecase from
        (:file 
         (make-location (list :file (namestring (truename name)))
                        (list :function-name (string fname))))
        (:stream
         (assert (debug-source-info-from-emacs-buffer-p (car source)))
         (make-location (list :buffer (getf info :emacs-buffer))
                        (list :function-name (string fname))))
        (:lisp
         (make-location (list :source-form (princ-to-string (aref name 0)))
                        (list :position 1)))))))

(defun debug-source-info-from-emacs-buffer-p (debug-source)
  "Does the `info' slot of 'debug-source contain an Emacs buffer location?
  This is true for functions that were compiled directly from buffers."
  (info-from-emacs-buffer-p (c::debug-source-info debug-source)))

(defun info-from-emacs-buffer-p (info)
  (and info 
       (consp info)
       (eq :emacs-buffer (car info))))


;;;;; Groveling source-code for positions

(defun code-location-stream-position (code-location stream)
  "Return the byte offset of 'code-location in 'stream.  Extract the
  toplevel-form-number and form-number from 'code-location and use that
  to find the position of the corresponding form.

  Finish with 'stream positioned at the start of the code location."
  (let* ((location (debug::maybe-block-start-location code-location))
	 (tlf-offset (di:code-location-top-level-form-offset location))
	 (form-number (di:code-location-form-number location)))
    (let ((pos (form-number-stream-position tlf-offset form-number stream)))
      (file-position stream pos)
      pos)))

(defun form-number-stream-position (tlf-number form-number stream)
  "Return the starting character position of a form in 'stream.
  'tlf-number is the top-level-form number.
  'form-number is an index into a source-path table for the TLF."
  (multiple-value-bind (tlf position-map) (read-source-form tlf-number stream)
    (let* ((path-table (di:form-number-translations tlf 0))
           (source-path
            (if (<= (length path-table) form-number) ; source out of sync?
                (list 0)                ; should probably signal a condition
                (reverse (cdr (aref path-table form-number))))))
      (source-path-source-position source-path tlf position-map))))
  
(defun code-location-string-offset (code-location string)
  "Return the byte offset of 'code-location in 'string.
  See 'code-location-stream-position."
  (with-input-from-string (s string)
    (code-location-stream-position code-location s)))


;;;; Finding definitions

;;; There are a great many different types of definition for us to
;;; find. We search for definitions of every kind and return them in a
;;; list.

(defimplementation find-definitions (name)
  (append (function-definitions name)
          (setf-definitions name)
          (variable-definitions name)
          (class-definitions name)
          (type-definitions name)
          (compiler-macro-definitions name)
          (source-transform-definitions name)
          (function-info-definitions name)
          (ir1-translator-definitions name)))

;;;;; Functions, macros, generic functions, methods
;;;
;;; We make extensive use of the compile-time debug information that
;;; SCL records, in particular "debug functions" and "code
;;; locations." Refer to the "Debugger Programmer's Interface" section
;;; of the SCL manual for more details.

(defun function-definitions (name)
  "Return definitions for 'name in the \"function namespace\", i.e.,
  regular functions, generic functions, methods and macros.
  'name can any valid function name (e.g, (setf car))."
  (let ((macro?    (and (symbolp name) (macro-function name)))
        (special?  (and (symbolp name) (special-operator-p name)))
        (function? (and (valid-function-name-p name)
                        (ext:info :function :definition name)
                        (if (symbolp name) (fboundp name) t))))
    (cond (macro? 
           (list `((defmacro ,name)
                   ,(function-location (macro-function name)))))
          (special?
           (list `((:special-operator ,name) 
                   (:error ,(format nil "Special operator: ~S" name)))))
          (function?
           (let ((function (fdefinition name)))
             (if (genericp function)
                 (generic-function-definitions name function)
                 (list (list `(function ,name)
                             (function-location function)))))))))

;;;;;; Ordinary (non-generic/macro/special) functions
;;;
;;; First we test if FUNCTION is a closure created by defstruct, and
;;; if so extract the defstruct-description (`dd') from the closure
;;; and find the constructor for the struct.  Defstruct creates a
;;; defun for the default constructor and we use that as an
;;; approximation to the source location of the defstruct.
;;;
;;; For an ordinary function we return the source location of the
;;; first code-location we find.
;;;
(defun function-location (function)
  "Return the source location for FUNCTION."
  (cond ((struct-closure-p function)
         (struct-closure-location function))
        ((c::byte-function-or-closure-p function)
         (byte-function-location function))
        (t
         (compiled-function-location function))))

(defun compiled-function-location (function)
  "Return the location of a regular compiled function."
  (multiple-value-bind (code-location error)
      (safe-definition-finding (function-first-code-location function))
    (cond (error (list :error (princ-to-string error)))
          (t (code-location-source-location code-location)))))

(defun function-first-code-location (function)
  "Return the first code-location we can find for 'function."
  (and (function-has-debug-function-p function)
       (di:debug-function-start-location
        (di:function-debug-function function))))

(defun function-has-debug-function-p (function)
  (di:function-debug-function function))

(defun function-code-object= (closure function)
  (and (eq (vm::find-code-object closure)
	   (vm::find-code-object function))
       (not (eq closure function))))


(defun byte-function-location (fn)
  "Return the location of the byte-compiled function 'fn."
  (etypecase fn
    ((or c::hairy-byte-function c::simple-byte-function)
     (let* ((component (c::byte-function-component fn))
            (debug-info (kernel:%code-debug-info component)))
       (debug-info-function-name-location debug-info)))
    (c::byte-closure
     (byte-function-location (c::byte-closure-function fn)))))

;;; Here we deal with structure accessors. Note that `dd' is a
;;; "defstruct descriptor" structure in SCL. A `dd' describes a
;;; `defstruct''d structure.

(defun struct-closure-p (function)
  "Is 'function a closure created by defstruct?"
  (or (function-code-object= function #'kernel::structure-slot-accessor)
      (function-code-object= function #'kernel::structure-slot-setter)
      (function-code-object= function #'kernel::%defstruct)))

(defun struct-closure-location (function)
  "Return the location of the structure that 'function belongs to."
  (assert (struct-closure-p function))
  (safe-definition-finding
    (dd-location (struct-closure-dd function))))

(defun struct-closure-dd (function)
  "Return the defstruct-definition (dd) of FUNCTION."
  (assert (= (kernel:get-type function) vm:closure-header-type))
  (flet ((find-layout (function)
	   (sys:find-if-in-closure 
	    (lambda (x) 
	      (let ((value (if (di::indirect-value-cell-p x)
			       (c:value-cell-ref x) 
			       x)))
		(when (kernel::layout-p value)
		  (return-from find-layout value))))
	    function)))
    (kernel:layout-info (find-layout function))))

(defun dd-location (dd)
  "Return the location of a `defstruct'."
  ;; Find the location in a constructor.
  (function-location (struct-constructor dd)))

(defun struct-constructor (dd)
  "Return a constructor function from a defstruct definition.
Signal an error if no constructor can be found."
  (let ((constructor (or (kernel:dd-default-constructor dd)
                         (car (kernel::dd-constructors dd)))))
    (when (or (null constructor)
              (and (consp constructor) (null (car constructor))))
      (error "Cannot find structure's constructor: ~S"
             (kernel::dd-name dd)))
    (coerce (if (consp constructor) (first constructor) constructor)
            'function)))

;;;;;; Generic functions and methods

(defun generic-function-definitions (name function)
  "Return the definitions of a generic function and its methods."
  (cons (list `(defgeneric ,name) (gf-location function))
        (gf-method-definitions function)))

(defun gf-location (gf)
  "Return the location of the generic function GF."
  (definition-source-location gf (clos:generic-function-name gf)))

(defun gf-method-definitions (gf)
  "Return the locations of all methods of the generic function GF."
  (mapcar #'method-definition (clos:generic-function-methods gf)))

(defun method-definition (method)
  (list (method-dspec method)
        (method-location method)))

(defun method-dspec (method)
  "Return a human-readable \"definition specifier\" for METHOD."
  (let* ((gf (clos:method-generic-function method))
         (name (clos:generic-function-name gf))
         (specializers (clos:method-specializers method))
         (qualifiers (clos:method-qualifiers method)))
    `(method ,name ,@qualifiers ,specializers 
             #+nil (clos::unparse-specializers specializers))))

;; XXX maybe special case setters/getters
(defun method-location (method)
  (function-location (clos:method-function method)))

(defun genericp (fn)
  (typep fn 'generic-function))

;;;;;; Types and classes

(defun type-definitions (name)
  "Return `deftype' locations for type NAME."
  (maybe-make-definition (ext:info :type :expander name) 'deftype name))

(defun maybe-make-definition (function kind name)
  "If FUNCTION is non-nil then return its definition location."
  (if function
      (list (list `(,kind ,name) (function-location function)))))

(defun class-definitions (name)
  "Return the definition locations for the class called NAME."
  (if (symbolp name)
      (let ((class (find-class name nil)))
        (etypecase class
          (null '())
          (structure-class
           (list (list `(defstruct ,name)
                       (dd-location (find-dd name)))))
          (standard-class
           (list (list `(defclass ,name) 
                       (class-location (find-class name)))))
          ((or built-in-class 
               kernel:funcallable-structure-class)
           (list (list `(kernel::define-type-class ,name)
                       `(:error 
                         ,(format nil "No source info for ~A" name)))))))))

(defun class-location (class)
  "Return the `defclass' location for CLASS."
  (definition-source-location class (class-name class)))

(defun find-dd (name)
  "Find the defstruct-definition by the name of its structure-class."
  (let ((layout (ext:info :type :compiler-layout name)))
    (if layout 
        (kernel:layout-info layout))))

(defun condition-class-location (class)
  (let ((name (class-name class)))
    `(:error ,(format nil "No location info for condition: ~A" name))))

(defun make-name-in-file-location (file string)
  (multiple-value-bind (filename c)
      (ignore-errors 
        (unix-truename (merge-pathnames (make-pathname :type "lisp")
                                        file)))
    (cond (filename (make-location `(:file ,filename)
                                   `(:function-name ,(string string))))
          (t (list :error (princ-to-string c))))))

(defun definition-source-location (object name)
  `(:error ,(format nil "No source info for: ~A" object)))

(defun setf-definitions (name)
  (let ((function (or (ext:info :setf :inverse name)
                      (ext:info :setf :expander name))))
    (if function
        (list (list `(setf ,name) 
                    (function-location (coerce function 'function)))))))


(defun variable-location (symbol)
  `(:error ,(format nil "No source info for variable ~S" symbol)))

(defun variable-definitions (name)
  (if (symbolp name)
      (multiple-value-bind (kind recorded-p) (ext:info :variable :kind name)
        (if recorded-p
            (list (list `(variable ,kind ,name)
                        (variable-location name)))))))

(defun compiler-macro-definitions (symbol)
  (maybe-make-definition (compiler-macro-function symbol)
                         'define-compiler-macro
                         symbol))

(defun source-transform-definitions (name)
  (maybe-make-definition (ext:info :function :source-transform name)
                         'c:def-source-transform
                         name))

(defun function-info-definitions (name)
  (let ((info (ext:info :function :info name)))
    (if info
        (append (loop for transform in (c::function-info-transforms info)
                      collect (list `(c:deftransform ,name 
                                      ,(c::type-specifier 
                                        (c::transform-type transform)))
                                    (function-location (c::transform-function 
                                                        transform))))
                (maybe-make-definition (c::function-info-derive-type info)
                                       'c::derive-type name)
                (maybe-make-definition (c::function-info-optimizer info)
                                       'c::optimizer name)
                (maybe-make-definition (c::function-info-ltn-annotate info)
                                       'c::ltn-annotate name)
                (maybe-make-definition (c::function-info-ir2-convert info)
                                       'c::ir2-convert name)
                (loop for template in (c::function-info-templates info)
                      collect (list `(c::vop ,(c::template-name template))
                                    (function-location 
                                     (c::vop-info-generator-function 
                                      template))))))))

(defun ir1-translator-definitions (name)
  (maybe-make-definition (ext:info :function :ir1-convert name)
                         'c:def-ir1-translator name))


;;;; Documentation.

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (ext:info variable kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (when (fboundp symbol)
	(maybe-push
	 (cond ((macro-function symbol)     :macro)
	       ((special-operator-p symbol) :special-operator)
	       ((genericp (fdefinition symbol)) :generic-function)
	       (t :function))
	 (doc 'function)))
      (maybe-push
       :setf (if (or (ext:info setf inverse symbol)
		     (ext:info setf expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (ext:info type kind symbol)
		 (doc 'type)))
      (maybe-push
       :class (if (find-class symbol nil) 
		  (doc 'class)))
      (maybe-push
       :alien-type (if (not (eq (ext:info alien-type kind symbol) :unknown))
		       (doc 'alien-type)))
      (maybe-push
       :alien-struct (if (ext:info alien-type struct symbol)
			 (doc nil)))
      (maybe-push
       :alien-union (if (ext:info alien-type union symbol)
			 (doc nil)))
      (maybe-push
       :alien-enum (if (ext:info alien-type enum symbol)
		       (doc nil)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (describe (ecase namespace
              (:variable
               symbol)
              ((:function :generic-function)
               (symbol-function symbol))
              (:setf
               (or (ext:info setf inverse symbol)
                   (ext:info setf expander symbol)))
              (:type
               (kernel:values-specifier-type symbol))
              (:class
               (find-class symbol))
              (:alien-struct
               (ext:info :alien-type :struct symbol))
              (:alien-union
               (ext:info :alien-type :union symbol))
              (:alien-enum
               (ext:info :alien-type :enum symbol))
              (:alien-type
               (ecase (ext:info :alien-type :kind symbol)
                 (:primitive
                  (let ((alien::*values-type-okay* t))
                    (funcall (ext:info :alien-type :translator symbol) 
                             (list symbol))))
                 ((:defined)
                  (ext:info :alien-type :definition symbol))
                 (:unknown :unknown))))))

;;;;; Argument lists

(defimplementation arglist (fun)
  (multiple-value-bind (args winp)
      (ext:function-arglist fun)
    (if winp args :not-available)))

(defimplementation function-name (function)
  (cond ((eval:interpreted-function-p function)
         (eval:interpreted-function-name function))
        ((typep function 'generic-function)
         (clos:generic-function-name function))
        ((c::byte-function-or-closure-p function)
         (c::byte-function-name function))
        (t (kernel:%function-name (kernel:%function-self function)))))


;;; A harder case: an approximate arglist is derived from available
;;; debugging information.

(defun debug-function-arglist (debug-function)
  "Derive the argument list of DEBUG-FUNCTION from debug info."
  (let ((args (di::debug-function-lambda-list debug-function))
        (required '())
        (optional '())
        (rest '())
        (key '()))
    ;; collect the names of debug-vars
    (dolist (arg args)
      (etypecase arg
        (di::debug-variable 
         (push (di::debug-variable-symbol arg) required))
        ((member :deleted)
         (push ':deleted required))
        (cons
         (ecase (car arg)
           (:keyword 
            (push (second arg) key))
           (:optional
            (push (debug-variable-symbol-or-deleted (second arg)) optional))
           (:rest 
            (push (debug-variable-symbol-or-deleted (second arg)) rest))))))
    ;; intersperse lambda keywords as needed
    (append (nreverse required)
            (if optional (cons '&optional (nreverse optional)))
            (if rest (cons '&rest (nreverse rest)))
            (if key (cons '&key (nreverse key))))))

(defun debug-variable-symbol-or-deleted (var)
  (etypecase var
    (di:debug-variable
     (di::debug-variable-symbol var))
    ((member :deleted)
     '#:deleted)))

(defun symbol-debug-function-arglist (fname)
  "Return FNAME's debug-function-arglist and %function-arglist.
  A utility for debugging DEBUG-FUNCTION-ARGLIST."
  (let ((fn (fdefinition fname)))
    (values (debug-function-arglist (di::function-debug-function fn))
            (kernel:%function-arglist (kernel:%function-self fn)))))


;;;; Miscellaneous.

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (macroexpand form))

(defimplementation set-default-directory (directory)
  (setf (ext:default-directory) (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (pathname (ext:default-directory)))
  (default-directory))

(defimplementation default-directory ()
  (namestring (ext:default-directory)))

(defimplementation pathname-to-filename (pathname)
  (ext:unix-namestring pathname nil))

(defimplementation getpid ()
  (unix:unix-getpid))

(defimplementation lisp-implementation-type-name ()
  (if (eq ext:*case-mode* :upper) "scl" "scl-lower"))

(defimplementation quit-lisp ()
  (ext:quit))

;;; source-path-{stream,file,string,etc}-position moved into 
;;; source-path-parser


;;;; Debugging

(defvar *sldb-stack-top*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((*sldb-stack-top* (or debug:*stack-top-hint* (di:top-frame)))
	 (debug:*stack-top-hint* nil)
         (kernel:*current-level* 0))
    (handler-bind ((di::unhandled-condition
		    (lambda (condition)
                      (error 'sldb-condition
                             :original-condition condition))))
      (funcall debugger-loop-fn))))

(defun frame-down (frame)
  (handler-case (di:frame-down frame)
    (di:no-debug-info () nil)))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (frame-down f)
	  for i from start below end
	  while f collect f)))

(defimplementation print-frame (frame stream)
  (let ((*standard-output* stream))
    (handler-case 
        (debug::print-frame-call frame :verbosity 1 :number nil)
      (error (e)
        (ignore-errors (princ e stream))))))

(defimplementation frame-source-location (index)
  (code-location-source-location (di:frame-code-location (nth-frame index))))

(defimplementation eval-in-frame (form index)
  (di:eval-in-frame (nth-frame index) form))

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (di::debug-function-debug-variables (di:frame-debug-function frame)))

(defun debug-var-value (var frame location)
  (let ((validity (di:debug-variable-validity var location)))
    (ecase validity
      (:valid (di:debug-variable-value var frame))
      ((:invalid :unknown) (make-symbol (string validity))))))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (loc (di:frame-code-location frame))
	 (vars (frame-debug-vars frame)))
    (loop for v across vars collect
          (list :name (di:debug-variable-symbol v)
                :id (di:debug-variable-id v)
                :value (debug-var-value v frame loc)))))

(defimplementation frame-var-value (frame var)
  (let* ((frame (nth-frame frame))
         (dvar (aref (frame-debug-vars frame) var)))
    (debug-var-value dvar frame (di:frame-code-location frame))))

(defimplementation frame-catch-tags (index)
  (mapcar #'car (di:frame-catches (nth-frame index))))

(defimplementation return-from-frame (index form)
  (let ((sym (find-symbol (symbol-name '#:find-debug-tag-for-frame)
                          :debug-internals)))
    (if sym
        (let* ((frame (nth-frame index))
               (probe (funcall sym frame)))
          (cond (probe (throw (car probe) (eval-in-frame form index)))
                (t (format nil "Cannot return from frame: ~S" frame))))
        "return-from-frame is not implemented in this version of SCL.")))

(defimplementation activate-stepping (frame)
  (set-step-breakpoints (nth-frame frame)))

(defimplementation sldb-break-on-return (frame)
  (break-on-return (nth-frame frame)))

;;; We set the breakpoint in the caller which might be a bit confusing.
;;;
(defun break-on-return (frame)
  (let* ((caller (di:frame-down frame))
         (cl (di:frame-code-location caller)))
    (flet ((hook (frame bp)
             (when (frame-pointer= frame caller)
               (di:delete-breakpoint bp)
               (signal-breakpoint bp frame))))
      (let* ((info (ecase (di:code-location-kind cl)
                     ((:single-value-return :unknown-return) nil)
                     (:known-return (debug-function-returns 
                                     (di:frame-debug-function frame)))))
             (bp (di:make-breakpoint #'hook cl :kind :code-location
                                     :info info)))
        (di:activate-breakpoint bp)
        `(:ok ,(format nil "Set breakpoint in ~A" caller))))))

(defun frame-pointer= (frame1 frame2)
  "Return true if the frame pointers of FRAME1 and FRAME2 are the same."
  (sys:sap= (di::frame-pointer frame1) (di::frame-pointer frame2)))

;;; The PC in escaped frames at a single-return-value point is
;;; actually vm:single-value-return-byte-offset bytes after the
;;; position given in the debug info.  Here we try to recognize such
;;; cases.
;;;
(defun next-code-locations (frame code-location)
  "Like `debug::next-code-locations' but be careful in escaped frames."
  (let ((next (debug::next-code-locations code-location)))
    (flet ((adjust-pc ()
             (let ((cl (di::copy-compiled-code-location code-location)))
               (incf (di::compiled-code-location-pc cl) 
                     vm:single-value-return-byte-offset)
               cl)))
      (cond ((and (di::compiled-frame-escaped frame)
                  (eq (di:code-location-kind code-location)
                      :single-value-return)
                  (= (length next) 1)
                  (di:code-location= (car next) (adjust-pc)))
             (debug::next-code-locations (car next)))
            (t
             next)))))

(defun set-step-breakpoints (frame)
  (let ((cl (di:frame-code-location frame)))
    (when (di:debug-block-elsewhere-p (di:code-location-debug-block cl))
      (error "Cannot step in elsewhere code"))
    (let* ((debug::*bad-code-location-types*
            (remove :call-site debug::*bad-code-location-types*))
           (next (next-code-locations frame cl)))
      (cond (next
             (let ((steppoints '()))
               (flet ((hook (bp-frame bp)
                        (signal-breakpoint bp bp-frame)
                        (mapc #'di:delete-breakpoint steppoints)))
                 (dolist (code-location next)
                   (let ((bp (di:make-breakpoint #'hook code-location
                                                 :kind :code-location)))
                     (di:activate-breakpoint bp)
                     (push bp steppoints))))))
            (t
             (break-on-return frame))))))


;; XXX the return values at return breakpoints should be passed to the
;; user hooks. debug-int.lisp should be changed to do this cleanly.

;;; The sigcontext and the PC for a breakpoint invocation are not
;;; passed to user hook functions, but we need them to extract return
;;; values. So we advice di::handle-breakpoint and bind the values to
;;; special variables.  
;;;
(defvar *breakpoint-sigcontext*)
(defvar *breakpoint-pc*)

(defun sigcontext-object (sc index)
  "Extract the lisp object in sigcontext SC at offset INDEX."
  (kernel:make-lisp-obj (vm:ucontext-register sc index)))

(defun known-return-point-values (sigcontext sc-offsets)
  (let ((fp (system:int-sap (vm:ucontext-register sigcontext
                                                  vm::cfp-offset))))
    (system:without-gcing
     (loop for sc-offset across sc-offsets
           collect (di::sub-access-debug-var-slot fp sc-offset sigcontext)))))

;;; SCL returns the first few values in registers and the rest on
;;; the stack. In the multiple value case, the number of values is
;;; stored in a dedicated register. The values of the registers can be
;;; accessed in the sigcontext for the breakpoint.  There are 3 kinds
;;; of return conventions: :single-value-return, :unknown-return, and
;;; :known-return.
;;;
;;; The :single-value-return convention returns the value in a
;;; register without setting the nargs registers.  
;;;
;;; The :unknown-return variant is used for multiple values. A
;;; :unknown-return point consists actually of 2 breakpoints: one for
;;; the single value case and one for the general case.  The single
;;; value breakpoint comes vm:single-value-return-byte-offset after
;;; the multiple value breakpoint.
;;;
;;; The :known-return convention is used by local functions.
;;; :known-return is currently not supported because we don't know
;;; where the values are passed.
;;;
(defun breakpoint-values (breakpoint)
  "Return the list of return values for a return point."
  (flet ((1st (sc) (sigcontext-object sc (car vm::register-arg-offsets))))
    (let ((sc (locally (declare (optimize (ext:inhibit-warnings 3)))
                (alien:sap-alien *breakpoint-sigcontext* (* unix:ucontext))))
          (cl (di:breakpoint-what breakpoint)))
      (ecase (di:code-location-kind cl)
        (:single-value-return
         (list (1st sc)))
        (:known-return
         (let ((info (di:breakpoint-info breakpoint)))
           (if (vectorp info)
               (known-return-point-values sc info)
               (progn 
                 ;;(break)
                 (list "<<known-return convention not supported>>" info)))))
        (:unknown-return
         (let ((mv-return-pc (di::compiled-code-location-pc cl)))
           (if (= mv-return-pc *breakpoint-pc*)
               (mv-function-end-breakpoint-values sc)
               (list (1st sc)))))))))

(defun mv-function-end-breakpoint-values (sigcontext)
  (let ((sym (find-symbol 
              (symbol-name '#:function-end-breakpoint-values/standard)
              :debug-internals)))
    (cond (sym (funcall sym sigcontext))
          (t (di::get-function-end-breakpoint-values sigcontext)))))

(defun debug-function-returns (debug-fun)
  "Return the return style of DEBUG-FUN."
  (let* ((cdfun (di::compiled-debug-function-compiler-debug-fun debug-fun)))
    (c::compiled-debug-function-returns cdfun)))

(define-condition breakpoint (simple-condition) 
  ((message :initarg :message :reader breakpoint.message)
   (values  :initarg :values  :reader breakpoint.values))
  (:report (lambda (c stream) (princ (breakpoint.message c) stream))))

#+nil
(defimplementation condition-extras ((c breakpoint))
  ;; simply pop up the source buffer
  `((:short-frame-source 0)))

(defun signal-breakpoint (breakpoint frame)
  "Signal a breakpoint condition for BREAKPOINT in FRAME.
Try to create a informative message."
  (flet ((brk (values fstring &rest args)
           (let ((msg (apply #'format nil fstring args))
                 (debug:*stack-top-hint* frame))
             (break 'breakpoint :message msg :values values))))
    (with-struct (di::breakpoint- kind what) breakpoint
      (case kind
        (:code-location
         (case (di:code-location-kind what)
           ((:single-value-return :known-return :unknown-return)
            (let ((values (breakpoint-values breakpoint)))
              (brk values "Return value: ~{~S ~}" values)))
           (t
            #+(or)
            (when (eq (di:code-location-kind what) :call-site)
              (call-site-function breakpoint frame))
            (brk nil "Breakpoint: ~S ~S" 
                 (di:code-location-kind what)
                 (di::compiled-code-location-pc what)))))
        (:function-start
         (brk nil "Function start breakpoint"))
        (t (brk nil "Breakpoint: ~A in ~A" breakpoint frame))))))

#+nil
(defimplementation sldb-break-at-start (fname)
  (let ((debug-fun (di:function-debug-function (coerce fname 'function))))
    (cond ((not debug-fun)
           `(:error ,(format nil "~S has no debug-function" fname)))
          (t
           (flet ((hook (frame bp &optional args cookie)
                    (declare (ignore args cookie))
                    (signal-breakpoint bp frame)))
             (let ((bp (di:make-breakpoint #'hook debug-fun
                                           :kind :function-start)))
               (di:activate-breakpoint bp)
               `(:ok ,(format nil "Set breakpoint in ~S" fname))))))))

(defun frame-cfp (frame)
  "Return the Control-Stack-Frame-Pointer for FRAME."
  (etypecase frame
    (di::compiled-frame (di::frame-pointer frame))
    ((or di::interpreted-frame null) -1)))

(defun frame-ip (frame)
  "Return the (absolute) instruction pointer and the relative pc of FRAME."
  (if (not frame)
      -1
      (let ((debug-fun (di::frame-debug-function frame)))
        (etypecase debug-fun
          (di::compiled-debug-function 
           (let* ((code-loc (di:frame-code-location frame))
                  (component (di::compiled-debug-function-component debug-fun))
                  (pc (di::compiled-code-location-pc code-loc))
                  (ip (sys:without-gcing
                       (sys:sap-int
                        (sys:sap+ (kernel:code-instructions component) pc)))))
             (values ip pc)))
          ((or di::bogus-debug-function di::interpreted-debug-function)
           -1)))))

(defun frame-registers (frame)
  "Return the lisp registers CSP, CFP, IP, OCFP, LRA for FRAME-NUMBER."
  (let* ((cfp (frame-cfp frame))
         (csp (frame-cfp (di::frame-up frame)))
         (ip (frame-ip frame))
         (ocfp (frame-cfp (di::frame-down frame)))
         (lra (frame-ip (di::frame-down frame))))
    (values csp cfp ip ocfp lra)))

(defun print-frame-registers (frame-number)
  (let ((frame (di::frame-real-frame (nth-frame frame-number))))
    (flet ((fixnum (p) (etypecase p
                         (integer p)
                         (sys:system-area-pointer (sys:sap-int p)))))
      (apply #'format t "~
CSP  =  ~X
CFP  =  ~X
IP   =  ~X
OCFP =  ~X
LRA  =  ~X~%" (mapcar #'fixnum 
                      (multiple-value-list (frame-registers frame)))))))


(defimplementation disassemble-frame (frame-number)
  "Return a string with the disassembly of frames code."
  (print-frame-registers frame-number)
  (terpri)
  (let* ((frame (di::frame-real-frame (nth-frame frame-number)))
         (debug-fun (di::frame-debug-function frame)))
    (etypecase debug-fun
      (di::compiled-debug-function
       (let* ((component (di::compiled-debug-function-component debug-fun))
              (fun (di:debug-function-function debug-fun)))
         (if fun
             (disassemble fun)
             (disassem:disassemble-code-component component))))
      (di::bogus-debug-function
       (format t "~%[Disassembling bogus frames not implemented]")))))


;;;; Inspecting

(defconstant +lowtag-symbols+ 
  '(vm:even-fixnum-type
    vm:instance-pointer-type
    vm:other-immediate-0-type
    vm:list-pointer-type
    vm:odd-fixnum-type
    vm:function-pointer-type
    vm:other-immediate-1-type
    vm:other-pointer-type)
  "Names of the constants that specify type tags.
The `symbol-value' of each element is a type tag.")

(defconstant +header-type-symbols+
  (labels ((suffixp (suffix string)
             (and (>= (length string) (length suffix))
                  (string= string suffix :start1 (- (length string) 
                                                    (length suffix)))))
           (header-type-symbol-p (x)
             (and (suffixp (symbol-name '#:-type) (symbol-name x))
                  (not (member x +lowtag-symbols+))
                  (boundp x)
                  (typep (symbol-value x) 'fixnum))))
    (remove-if-not #'header-type-symbol-p
                   (append (apropos-list (symbol-name '#:-type) :vm)
                           (apropos-list (symbol-name '#:-type) :bignum))))
  "A list of names of the type codes in boxed objects.")

(defimplementation describe-primitive-type (object)
  (with-output-to-string (*standard-output*)
    (let* ((lowtag (kernel:get-lowtag object))
	   (lowtag-symbol (find lowtag +lowtag-symbols+ :key #'symbol-value)))
      (format t "lowtag: ~A" lowtag-symbol)
      (when (member lowtag (list vm:other-pointer-type
                                 vm:function-pointer-type
                                 vm:other-immediate-0-type
                                 vm:other-immediate-1-type
                                 ))
        (let* ((type (kernel:get-type object))
               (type-symbol (find type +header-type-symbols+
                                  :key #'symbol-value)))
          (format t ", type: ~A" type-symbol))))))

(defmethod emacs-inspect ((o t))
  (cond ((di::indirect-value-cell-p o)
                 `("Value: " (:value ,(c:value-cell-ref o))))
        ((alien::alien-value-p o)
         (inspect-alien-value o))
	(t
         (scl-inspect o))))

(defun scl-inspect (o)
  (destructuring-bind (text labeledp . parts)
      (inspect::describe-parts o)
    (list*  (format nil "~A~%" text)
            (if labeledp
                (loop for (label . value) in parts
                      append (label-value-line label value))
                (loop for value in parts  for i from 0 
                      append (label-value-line i value))))))

(defmethod emacs-inspect ((o function))
  (let ((header (kernel:get-type o)))
    (cond ((= header vm:function-header-type)
           (list*  (format nil "~A is a function.~%" o)
                   (append (label-value-line*
                            ("Self" (kernel:%function-self o))
                            ("Next" (kernel:%function-next o))
                            ("Name" (kernel:%function-name o))
                            ("Arglist" (kernel:%function-arglist o))
                            ("Type" (kernel:%function-type o))
                            ("Code" (kernel:function-code-header o)))
                           (list 
                            (with-output-to-string (s)
                              (disassem:disassemble-function o :stream s))))))
          ((= header vm:closure-header-type)
           (list* (format nil "~A is a closure.~%" o)
                   (append 
                    (label-value-line "Function" (kernel:%closure-function o))
                    `("Environment:" (:newline))
                    (loop for i from 0 below (- (kernel:get-closure-length o)
                                                (1- vm:closure-info-offset))
                          append (label-value-line 
                                  i (kernel:%closure-index-ref o i))))))
          ((eval::interpreted-function-p o)
           (scl-inspect o))
          (t
           (call-next-method)))))


(defmethod emacs-inspect ((o kernel:code-component))
          (append 
           (label-value-line* 
            ("code-size" (kernel:%code-code-size o))
            ("entry-points" (kernel:%code-entry-points o))
            ("debug-info" (kernel:%code-debug-info o))
            ("trace-table-offset" (kernel:code-header-ref 
                                   o vm:code-trace-table-offset-slot)))
           `("Constants:" (:newline))
           (loop for i from vm:code-constants-offset 
                 below (kernel:get-header-data o)
                 append (label-value-line i (kernel:code-header-ref o i)))
           `("Code:" (:newline)
             , (with-output-to-string (s)
                 (cond ((kernel:%code-debug-info o)
                        (disassem:disassemble-code-component o :stream s))
                       (t
                        (disassem:disassemble-memory 
                         (disassem::align 
                          (+ (logandc2 (kernel:get-lisp-obj-address o)
                                       vm:lowtag-mask)
                             (* vm:code-constants-offset vm:word-bytes))
                          (ash 1 vm:lowtag-bits))
                         (ash (kernel:%code-code-size o) vm:word-shift)
                         :stream s)))))))

(defmethod emacs-inspect ((o kernel:fdefn))
  (label-value-line*
           ("name" (kernel:fdefn-name o))
           ("function" (kernel:fdefn-function o))
           ("raw-addr" (sys:sap-ref-32
                        (sys:int-sap (kernel:get-lisp-obj-address o))
                        (* vm:fdefn-raw-addr-slot vm:word-bytes)))))

(defmethod emacs-inspect ((o array))
  (cond ((kernel:array-header-p o)
         (list*  (format nil "~A is an array.~%" o)
                 (label-value-line*
                  (:header (describe-primitive-type o))
                  (:rank (array-rank o))
                  (:fill-pointer (kernel:%array-fill-pointer o))
                  (:fill-pointer-p (kernel:%array-fill-pointer-p o))
                  (:elements (kernel:%array-available-elements o))           
                  (:data (kernel:%array-data-vector o))
                  (:displacement (kernel:%array-displacement o))
                  (:displaced-p (kernel:%array-displaced-p o))
                  (:dimensions (array-dimensions o)))))
        (t
         (list*  (format nil "~A is an simple-array.~%" o)
                 (label-value-line*
                  (:header (describe-primitive-type o))
                  (:length (length o)))))))

(defmethod emacs-inspect ((o simple-vector))
  (list*  (format nil "~A is a vector.~%" o)
          (append 
           (label-value-line*
            (:header (describe-primitive-type o))
            (:length (c::vector-length o)))
           (unless (eq (array-element-type o) 'nil)
             (loop for i below (length o)
                   append (label-value-line i (aref o i)))))))

(defun inspect-alien-record (alien)
   (with-struct (alien::alien-value- sap type) alien
     (with-struct (alien::alien-record-type- kind name fields) type
       (append
        (label-value-line*
         (:sap sap)
         (:kind kind)
         (:name name))
        (loop for field in fields 
              append (let ((slot (alien::alien-record-field-name field)))
                       (label-value-line slot (alien:slot alien slot))))))))

(defun inspect-alien-pointer (alien)
  (with-struct (alien::alien-value- sap type) alien
     (label-value-line* 
      (:sap sap)
      (:type type)
      (:to (alien::deref alien)))))
  
(defun inspect-alien-value (alien)
  (typecase (alien::alien-value-type alien)
    (alien::alien-record-type (inspect-alien-record alien))
    (alien::alien-pointer-type (inspect-alien-pointer alien))
    (t (scl-inspect alien))))

;;;; Profiling
(defimplementation profile (fname)
  (eval `(profile:profile ,fname)))

(defimplementation unprofile (fname)
  (eval `(profile:unprofile ,fname)))

(defimplementation unprofile-all ()
  (eval `(profile:unprofile))
  "All functions unprofiled.")

(defimplementation profile-report ()
  (eval `(profile:report-time)))

(defimplementation profile-reset ()
  (eval `(profile:reset-time))
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  profile:*timed-functions*)

(defimplementation profile-package (package callers methods)
  (profile:profile-all :package package
                       :callers-p callers
                       #+nil :methods #+nil methods))


;;;; Multiprocessing

(defimplementation spawn (fn &key name)
  (thread:thread-create fn :name (or name "Anonymous")))

(defvar *thread-id-counter* 0)
(defvar *thread-id-counter-lock* (thread:make-lock "Thread ID counter"))

(defimplementation thread-id (thread)
  (thread:with-lock-held (*thread-id-counter-lock*)
    (or (getf (thread:thread-plist thread) 'id)
        (setf (getf (thread:thread-plist thread) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (block find-thread
    (thread:map-over-threads
     #'(lambda (thread)
         (when (eql (getf (thread:thread-plist thread) 'id) id)
           (return-from find-thread thread))))))

(defimplementation thread-name (thread)
  (princ-to-string (thread:thread-name thread)))

(defimplementation thread-status (thread)
  (let ((dynamic-values (thread::thread-dynamic-values thread)))
    (if (zerop dynamic-values) "Exited" "Running")))

(defimplementation make-lock (&key name)
  (thread:make-lock name))

(defimplementation call-with-lock-held (lock function)
  (declare (type function function))
  (thread:with-lock-held (lock) (funcall function)))

(defimplementation current-thread ()
  thread:*thread*)

(defimplementation all-threads ()
  (let ((all-threads nil))
    (thread:map-over-threads #'(lambda (thread) (push thread all-threads)))
    all-threads))

(defimplementation interrupt-thread (thread fn)
  (thread:thread-interrupt thread #'(lambda ()
                                      (sys:with-interrupts
                                        (funcall fn)))))

(defimplementation kill-thread (thread)
  (thread:destroy-thread thread))

(defimplementation thread-alive-p (thread)
  (not (zerop (thread::thread-dynamic-values thread))))

(defvar *mailbox-lock* (thread:make-lock "Mailbox lock" :interruptible nil))
  
(defstruct (mailbox)
  (lock (thread:make-lock "Thread mailbox" :type :error-check
                          :interruptible nil)
        :type thread:error-check-lock)
  (queue '() :type list))

(defun mailbox (thread)
  "Return 'thread's mailbox."
  (sys:without-interrupts
    (thread:with-lock-held (*mailbox-lock*)
      (or (getf (thread:thread-plist thread) 'mailbox)
          (setf (getf (thread:thread-plist thread) 'mailbox)
                (make-mailbox))))))
  
(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (lock (mailbox-lock mbox)))
    (sys:without-interrupts
      (thread:with-lock-held (lock "Mailbox Send")
        (setf (mailbox-queue mbox) (nconc (mailbox-queue mbox)
                                          (list message)))))
    (mp:process-wakeup thread)))

#+nil
(defimplementation receive ()
  (receive-if (constantly t)))

(defimplementation receive-if (test &optional timeout)
  (let ((mbox (mailbox thread:*thread*)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
     (check-slime-interrupts)
     (sys:without-interrupts
       (mp:with-lock-held ((mailbox-lock mbox))
         (let* ((q (mailbox-queue mbox))
                (tail (member-if test q)))
           (when tail
             (setf (mailbox-queue mbox) 
                   (nconc (ldiff q tail) (cdr tail)))
             (return (car tail))))))
     (when (eq timeout t) (return (values nil t)))
     (mp:process-wait-with-timeout
      "Mailbox read wait" 0.5 (lambda () (some test (mailbox-queue mbox)))))))



(defimplementation emacs-connected ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Trace implementations
;; In SCL, we have:
;;  (trace <name>)
;;  (trace (method <name> <qualifier>? (<specializer>+)))
;;  (trace :methods t '<name>) ;;to trace all methods of the gf <name>
;;  <name> can be a normal name or a (setf name)

(defun tracedp (spec)
  (member spec (eval '(trace)) :test #'equal))

(defun toggle-trace-aux (spec &rest options)
  (cond ((tracedp spec)
         (eval `(untrace ,spec))
         (format nil "~S is now untraced." spec))
        (t
         (eval `(trace ,spec ,@options))
         (format nil "~S is now traced." spec))))

(defimplementation toggle-trace (spec)
  (ecase (car spec)
    ((setf)
     (toggle-trace-aux spec))
    ((:defgeneric)
     (let ((name (second spec)))
       (toggle-trace-aux name :methods name)))
    ((:defmethod)
     nil)
    ((:call)
     (destructuring-bind (caller callee) (cdr spec)
       (toggle-trace-aux (process-fspec callee) 
                         :wherein (list (process-fspec caller)))))))

(defun process-fspec (fspec)
  (cond ((consp fspec)
         (ecase (first fspec)
           ((:defun :defgeneric) (second fspec))
           ((:defmethod) 
            `(method ,(second fspec) ,@(third fspec) ,(fourth fspec)))
           ;; this isn't actually supported
           ((:labels) `(labels ,(process-fspec (second fspec)) ,(third fspec)))
           ((:flet) `(flet ,(process-fspec (second fspec)) ,(third fspec)))))
        (t
         fspec)))

;;; Weak datastructures

;;; Not implemented in SCL.
(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak-p t args))
