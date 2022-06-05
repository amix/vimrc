;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;;*" -*-
;;;
;;; slime-backend.lisp --- SLIME backend interface.
;;;
;;; Created by James Bielman in 2003. Released into the public domain.
;;;
;;;; Frontmatter
;;;
;;; This file defines the functions that must be implemented
;;; separately for each Lisp. Each is declared as a generic function
;;; for which swank-<implementation>.lisp provides methods.

(in-package swank/backend)


;;;; Metacode

(defparameter *debug-swank-backend* nil
  "If this is true, backends should not catch errors but enter the
debugger where appropriate. Also, they should not perform backtrace
magic but really show every frame including SWANK related ones.")

(defparameter *interface-functions* '()
  "The names of all interface functions.")

(defparameter *unimplemented-interfaces* '()
  "List of interface functions that are not implemented.
DEFINTERFACE adds to this list and DEFIMPLEMENTATION removes.")

(defvar *log-output* nil)            ; should be nil for image dumpers

(defmacro definterface (name args documentation &rest default-body)
  "Define an interface function for the backend to implement.
A function is defined with NAME, ARGS, and DOCUMENTATION.  This
function first looks for a function to call in NAME's property list
that is indicated by 'IMPLEMENTATION; failing that, it looks for a
function indicated by 'DEFAULT. If neither is present, an error is
signaled.

If a DEFAULT-BODY is supplied, then a function with the same body and
ARGS will be added to NAME's property list as the property indicated
by 'DEFAULT.

Backends implement these functions using DEFIMPLEMENTATION."
  (check-type documentation string "a documentation string")
  (assert (every #'symbolp args) ()
          "Complex lambda-list not supported: ~S ~S" name args)
  (labels ((gen-default-impl ()
             `(setf (get ',name 'default) (lambda ,args ,@default-body)))
           (args-as-list (args)
             (destructuring-bind (req opt key rest) (parse-lambda-list args)
               `(,@req ,@opt
                       ,@(loop for k in key append `(,(kw k) ,k))
                       ,@(or rest '(())))))
           (parse-lambda-list (args)
             (parse args '(&optional &key &rest)
                    (make-array 4 :initial-element nil)))
           (parse (args keywords vars)
             (cond ((null args)
                    (reverse (map 'list #'reverse vars)))
                   ((member (car args) keywords)
                    (parse (cdr args) (cdr (member (car args) keywords)) vars))
                   (t (push (car args) (aref vars (length keywords)))
                      (parse (cdr args) keywords vars))))
           (kw (s) (intern (string s) :keyword)))
    `(progn
       (defun ,name ,args
         ,documentation
         (let ((f (or (get ',name 'implementation)
                      (get ',name 'default))))
           (cond (f (apply f ,@(args-as-list args)))
                 (t (error "~S not implemented" ',name)))))
       (pushnew ',name *interface-functions*)
       ,(if (null default-body)
            `(pushnew ',name *unimplemented-interfaces*)
            (gen-default-impl))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name :swank/backend))
       ',name)))

(defmacro defimplementation (name args &body body)
  (assert (every #'symbolp args) ()
          "Complex lambda-list not supported: ~S ~S" name args)
  `(progn
     (setf (get ',name 'implementation)
           ;; For implicit BLOCK. FLET because of interplay w/ decls.
           (flet ((,name ,args ,@body)) #',name))
     (if (member ',name *interface-functions*)
         (setq *unimplemented-interfaces*
               (remove ',name *unimplemented-interfaces*))
         (warn "DEFIMPLEMENTATION of undefined interface (~S)" ',name))
     ',name))

(defun warn-unimplemented-interfaces ()
  "Warn the user about unimplemented backend features.
The portable code calls this function at startup."
  (let ((*print-pretty* t))
    (warn "These Swank interfaces are unimplemented:~% ~:<~{~A~^ ~:_~}~:>"
          (list (sort (copy-list *unimplemented-interfaces*) #'string<)))))

(defun import-to-swank-mop (symbol-list)
  (dolist (sym symbol-list)
    (let* ((swank-mop-sym (find-symbol (symbol-name sym) :swank-mop)))
      (when swank-mop-sym
        (unintern swank-mop-sym :swank-mop))
      (import sym :swank-mop)
      (export sym :swank-mop))))

(defun import-swank-mop-symbols (package except)
  "Import the mop symbols from PACKAGE to SWANK-MOP.
EXCEPT is a list of symbol names which should be ignored."
  (do-symbols (s :swank-mop)
    (unless (member s except :test #'string=)
      (let ((real-symbol (find-symbol (string s) package)))
        (assert real-symbol () "Symbol ~A not found in package ~A" s package)
        (unintern s :swank-mop)
        (import real-symbol :swank-mop)
        (export real-symbol :swank-mop)))))

(definterface gray-package-name ()
  "Return a package-name that contains the Gray stream symbols.
This will be used like so:
  (defpackage foo
    (:import-from #.(gray-package-name) . #.*gray-stream-symbols*)")


;;;; Utilities

(defmacro with-struct ((conc-name &rest names) obj &body body)
  "Like with-slots but works only for structs."
  (check-type conc-name symbol)
  (flet ((reader (slot)
           (intern (concatenate 'string
                                (symbol-name conc-name)
                                (symbol-name slot))
                   (symbol-package conc-name))))
    (let ((tmp (gensym "OO-")))
      ` (let ((,tmp ,obj))
          (symbol-macrolet
              ,(loop for name in names collect
                     (typecase name
                       (symbol `(,name (,(reader name) ,tmp)))
                       (cons `(,(first name) (,(reader (second name)) ,tmp)))
                       (t (error "Malformed syntax in WITH-STRUCT: ~A" name))))
            ,@body)))))

(defmacro when-let ((var value) &body body)
  `(let ((,var ,value))
     (when ,var ,@body)))

(defun boolean-to-feature-expression (value)
  "Converts a boolean VALUE to a form suitable for testing with #+."
  (if value
      '(:and)
      '(:or)))

(defun with-symbol (name package)
  "Check if a symbol with a given NAME exists in PACKAGE and returns a
form suitable for testing with #+."
  (boolean-to-feature-expression
   (and (find-package package)
        (find-symbol (string name) package))))

(defun choose-symbol (package name alt-package alt-name)
  "If symbol package:name exists return that symbol, otherwise alt-package:alt-name.
  Suitable for use with #."
  (or (and (find-package package)
           (find-symbol (string name) package))
      (find-symbol (string alt-name) alt-package)))


;;;; UFT8

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))

;; Helper function.  Decode the next N bytes starting from INDEX.
;; Return the decoded char and the new index.
(defun utf8-decode-aux (buffer index limit byte0 n)
  (declare (type octets buffer) (fixnum index limit byte0 n))
  (if (< (- limit index) n)
      (values nil index)
      (do ((i 0 (1+ i))
           (code byte0 (let ((byte (aref buffer (+ index i))))
                         (cond ((= (ldb (byte 2 6) byte) #b10)
                                (+ (ash code 6) (ldb (byte 6 0) byte)))
                               (t
                                #xFFFD))))) ;; Replacement_Character
          ((= i n)
           (values (cond ((<= code #xff) (code-char code))
                         ((<= #xd800 code #xdfff)
                          (code-char #xFFFD)) ;; Replacement_Character
                         ((and (< code char-code-limit)
                               (code-char code)))
                         (t
                          (code-char #xFFFD))) ;; Replacement_Character
                   (+ index n))))))

;; Decode one character in BUFFER starting at INDEX.
;; Return 2 values: the character and the new index.
;; If there aren't enough bytes between INDEX and LIMIT return nil.
(defun utf8-decode (buffer index limit)
  (declare (type octets buffer) (fixnum index limit))
  (if (= index limit)
      (values nil index)
      (let ((b (aref buffer index)))
        (if (<= b #x7f)
            (values (code-char b) (1+ index))
            (macrolet ((try (marker else)
                         (let* ((l (integer-length marker))
                                (n (- l 2)))
                           `(if (= (ldb (byte ,l ,(- 8 l)) b) ,marker)
                                (utf8-decode-aux buffer (1+ index) limit
                                                 (ldb (byte ,(- 8 l) 0) b)
                                                 ,n)
                                ,else))))
              (try #b110
                   (try #b1110
                        (try #b11110
                             (try #b111110
                                  (try #b1111110
                                       (error "Invalid encoding")))))))))))

;; Decode characters from BUFFER and write them to STRING.
;; Return 2 values: LASTINDEX and LASTSTART where
;; LASTINDEX is the last index in BUFFER that was not decoded
;; and LASTSTART is the last index in STRING not written.
(defun utf8-decode-into (buffer index limit string start end)
  (declare (string string) (fixnum index limit start end) (type octets buffer))
  (loop
   (cond ((= start end)
          (return (values index start)))
         (t
          (multiple-value-bind (c i) (utf8-decode buffer index limit)
            (cond (c
                   (setf (aref string start) c)
                   (setq index i)
                   (setq start (1+ start)))
                  (t
                   (return (values index start)))))))))

(defun default-utf8-to-string (octets)
  (let* ((limit (length octets))
         (str (make-string limit)))
    (multiple-value-bind (i s) (utf8-decode-into octets 0 limit str 0 limit)
      (if (= i limit)
          (if (= limit s)
              str
              (adjust-array str s))
          (loop
           (let ((end (+ (length str) (- limit i))))
             (setq str (adjust-array str end))
             (multiple-value-bind (i2 s2)
                 (utf8-decode-into octets i limit str s end)
               (cond ((= i2 limit)
                      (return (adjust-array str s2)))
                     (t
                      (setq i i2)
                      (setq s s2))))))))))

(defmacro utf8-encode-aux (code buffer start end n)
  `(cond ((< (- ,end ,start) ,n)
          ,start)
         (t
          (setf (aref ,buffer ,start)
                (dpb (ldb (byte ,(- 7 n) ,(* 6 (1- n))) ,code)
                     (byte ,(- 7 n) 0)
                     ,(dpb 0 (byte 1 (- 7 n)) #xff)))
          ,@(loop for i from 0 upto (- n 2) collect
                  `(setf (aref ,buffer (+ ,start ,(- n 1 i)))
                         (dpb (ldb (byte 6 ,(* 6 i)) ,code)
                              (byte 6 0)
                              #b10111111)))
          (+ ,start ,n))))

(defun %utf8-encode (code buffer start end)
  (declare (type (unsigned-byte 31) code) (type octets buffer)
           (type (and fixnum unsigned-byte) start end))
  (cond ((<= code #x7f)
         (cond ((< start end)
                (setf (aref buffer start) code)
                (1+ start))
               (t start)))
        ((<= code #x7ff) (utf8-encode-aux code buffer start end 2))
        ((<= #xd800 code #xdfff)
         (%utf8-encode (code-char #xFFFD) ;; Replacement_Character
                       buffer start end))
        ((<= code #xffff) (utf8-encode-aux code buffer start end 3))
        ((<= code #x1fffff) (utf8-encode-aux code buffer start end 4))
        ((<= code #x3ffffff) (utf8-encode-aux code buffer start end 5))
        (t (utf8-encode-aux code buffer start end 6))))

(defun utf8-encode (char buffer start end)
  (declare (type character char) (type octets buffer)
           (type (and fixnum unsigned-byte) start end))
  (%utf8-encode (char-code char) buffer start end))

(defun utf8-encode-into (string start end buffer index limit)
  (declare (string string) (type octets buffer) (fixnum start end index limit))
  (loop
   (cond ((= start end)
          (return (values start index)))
         ((= index limit)
          (return (values start index)))
         (t
          (let ((i2 (utf8-encode (char string start) buffer index limit)))
            (cond ((= i2 index)
                   (return (values start index)))
                  (t
                   (setq index i2)
                   (incf start))))))))

(defun default-string-to-utf8 (string)
  (let* ((len (length string))
         (b (make-array len :element-type 'octet)))
    (multiple-value-bind (s i) (utf8-encode-into string 0 len b 0 len)
      (if (= s len)
          b
          (loop
           (let ((limit (+ (length b) (- len s))))
             (setq b (coerce (adjust-array b limit) 'octets))
             (multiple-value-bind (s2 i2)
                 (utf8-encode-into string s len b i limit)
               (cond ((= s2 len)
                      (return (coerce (adjust-array b i2) 'octets)))
                     (t
                      (setq i i2)
                      (setq s s2))))))))))

(definterface string-to-utf8 (string)
  "Convert the string STRING to a (simple-array (unsigned-byte 8))"
  (default-string-to-utf8 string))

(definterface utf8-to-string (octets)
  "Convert the (simple-array (unsigned-byte 8)) OCTETS to a string."
  (default-utf8-to-string octets))


;;;; TCP server

(definterface create-socket (host port &key backlog)
  "Create a listening TCP socket on interface HOST and port PORT.
BACKLOG queue length for incoming connections.")

(definterface local-port (socket)
  "Return the local port number of SOCKET.")

(definterface close-socket (socket)
  "Close the socket SOCKET.")

(definterface accept-connection (socket &key external-format
                                        buffering timeout)
   "Accept a client connection on the listening socket SOCKET.
Return a stream for the new connection.
If EXTERNAL-FORMAT is nil return a binary stream
otherwise create a character stream.
BUFFERING can be one of:
  nil   ... no buffering
  t     ... enable buffering
  :line ... enable buffering with automatic flushing on eol.")

(definterface add-sigio-handler (socket fn)
  "Call FN whenever SOCKET is readable.")

(definterface remove-sigio-handlers (socket)
  "Remove all sigio handlers for SOCKET.")

(definterface add-fd-handler (socket fn)
  "Call FN when Lisp is waiting for input and SOCKET is readable.")

(definterface remove-fd-handlers (socket)
  "Remove all fd-handlers for SOCKET.")

(definterface preferred-communication-style ()
  "Return one of the symbols :spawn, :sigio, :fd-handler, or NIL."
  nil)

(definterface set-stream-timeout (stream timeout)
  "Set the 'stream 'timeout.  The timeout is either the real number
  specifying the timeout in seconds or 'nil for no timeout."
  (declare (ignore stream timeout))
  nil)

;;; Base condition for networking errors.
(define-condition network-error (simple-error) ())

(definterface emacs-connected ()
   "Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs."
   nil)


;;;; Unix signals

(defconstant +sigint+ 2)

(definterface getpid ()
  "Return the (Unix) process ID of this superior Lisp.")

(definterface install-sigint-handler (function)
  "Call FUNCTION on SIGINT (instead of invoking the debugger).
Return old signal handler."
  (declare (ignore function))
  nil)

(definterface call-with-user-break-handler (handler function)
  "Install the break handler HANDLER while executing FUNCTION."
  (let ((old-handler (install-sigint-handler handler)))
    (unwind-protect (funcall function)
      (install-sigint-handler old-handler))))

(definterface quit-lisp ()
  "Exit the current lisp image.")

(definterface lisp-implementation-type-name ()
  "Return a short name for the Lisp implementation."
  (lisp-implementation-type))

(definterface lisp-implementation-program ()
  "Return the argv[0] of the running Lisp process, or NIL."
  (let ((file (car (command-line-args))))
    (when (and file (probe-file file))
      (namestring (truename file)))))

(definterface socket-fd (socket-stream)
  "Return the file descriptor for SOCKET-STREAM.")

(definterface make-fd-stream (fd external-format)
  "Create a character stream for the file descriptor FD.")

(definterface dup (fd)
  "Duplicate a file descriptor.
If the syscall fails, signal a condition.
See dup(2).")

(definterface exec-image (image-file args)
  "Replace the current process with a new process image.
The new image is created by loading the previously dumped
core file IMAGE-FILE.
ARGS is a list of strings passed as arguments to
the new image.
This is thin wrapper around exec(3).")

(definterface command-line-args ()
  "Return a list of strings as passed by the OS."
  nil)


;; pathnames are sooo useless

(definterface filename-to-pathname (filename)
  "Return a pathname for FILENAME.
A filename in Emacs may for example contain asterisks which should not
be translated to wildcards."
  (parse-namestring filename))

(definterface pathname-to-filename (pathname)
  "Return the filename for PATHNAME."
  (namestring pathname))

(definterface default-directory ()
  "Return the default directory."
  (directory-namestring (truename *default-pathname-defaults*)))

(definterface set-default-directory (directory)
  "Set the default directory.
This is used to resolve filenames without directory component."
  (setf *default-pathname-defaults* (truename (merge-pathnames directory)))
  (default-directory))


(definterface call-with-syntax-hooks (fn)
  "Call FN with hooks to handle special syntax."
  (funcall fn))

(definterface default-readtable-alist ()
  "Return a suitable initial value for SWANK:*READTABLE-ALIST*."
  '())


;;;; Packages

(definterface package-local-nicknames (package)
  "Returns an alist of (local-nickname . actual-package) describing the
nicknames local to the designated package."
  (declare (ignore package))
  nil)

(definterface find-locally-nicknamed-package (name base-package)
  "Return the package whose local nickname in BASE-PACKAGE matches NAME.
Return NIL if local nicknames are not implemented or if there is no
such package."
  (cdr (assoc name (package-local-nicknames base-package) :test #'string-equal)))


;;;; Compilation

(definterface call-with-compilation-hooks (func)
  "Call FUNC with hooks to record compiler conditions.")

(defmacro with-compilation-hooks ((&rest ignore) &body body)
  "Execute BODY as in CALL-WITH-COMPILATION-HOOKS."
  (declare (ignore ignore))
  `(call-with-compilation-hooks (lambda () (progn ,@body))))

(definterface swank-compile-string (string &key buffer position filename
                                           line column policy)
  "Compile source from STRING.
During compilation, compiler conditions must be trapped and
resignalled as COMPILER-CONDITIONs.

If supplied, BUFFER and POSITION specify the source location in Emacs.

Additionally, if POSITION is supplied, it must be added to source
positions reported in compiler conditions.

If FILENAME is specified it may be used by certain implementations to
rebind *DEFAULT-PATHNAME-DEFAULTS* which may improve the recording of
source information.

If POLICY is supplied, and non-NIL, it may be used by certain
implementations to compile with optimization qualities of its
value.

If LINE and COLUMN are supplied, and non-NIL, they may be used
by certain implementations as the line and column of the start of
the string in FILENAME. Both LINE and COLUMN are 1-based.

Should return T on successful compilation, NIL otherwise.
")

(definterface swank-compile-file (input-file output-file load-p
                                             external-format
                                             &key policy)
   "Compile INPUT-FILE signalling COMPILE-CONDITIONs.
If LOAD-P is true, load the file after compilation.
EXTERNAL-FORMAT is a value returned by find-external-format or
:default.

If POLICY is supplied, and non-NIL, it may be used by certain
implementations to compile with optimization qualities of its
value.

Should return OUTPUT-TRUENAME, WARNINGS-P and FAILURE-p
like `compile-file'")

(deftype severity ()
  '(member :error :read-error :warning :style-warning :note :redefinition))

;; Base condition type for compiler errors, warnings and notes.
(define-condition compiler-condition (condition)
  ((original-condition
    ;; The original condition thrown by the compiler if appropriate.
    ;; May be NIL if a compiler does not report using conditions.
    :type (or null condition)
    :initarg :original-condition
    :accessor original-condition)

   (severity :type severity
             :initarg :severity
             :accessor severity)

   (message :initarg :message
            :accessor message)

   ;; Macro expansion history etc. which may be helpful in some cases
   ;; but is often very verbose.
   (source-context :initarg :source-context
                   :type (or null string)
                   :initform nil
                   :accessor source-context)

   (references :initarg :references
               :initform nil
               :accessor references)

   (location :initarg :location
             :accessor location)))

(definterface find-external-format (coding-system)
  "Return a \"external file format designator\" for CODING-SYSTEM.
CODING-SYSTEM is Emacs-style coding system name (a string),
e.g. \"latin-1-unix\"."
  (if (equal coding-system "iso-latin-1-unix")
      :default
      nil))

(definterface guess-external-format (pathname)
  "Detect the external format for the file with name pathname.
Return nil if the file contains no special markers."
  ;; Look for a Emacs-style -*- coding: ... -*- or Local Variable: section.
  (with-open-file (s pathname :if-does-not-exist nil
                     :external-format (or (find-external-format "latin-1-unix")
                                          :default))
    (if s
        (or (let* ((line (read-line s nil))
                   (p (search "-*-" line)))
              (when p
                (let* ((start (+ p (length "-*-")))
                       (end (search "-*-" line :start2 start)))
                  (when end
                    (%search-coding line start end)))))
            (let* ((len (file-length s))
                   (buf (make-string (min len 3000))))
              (file-position s (- len (length buf)))
              (read-sequence buf s)
              (let ((start (search "Local Variables:" buf :from-end t))
                    (end (search "End:" buf :from-end t)))
                (and start end (< start end)
                     (%search-coding buf start end))))))))

(defun %search-coding (str start end)
  (let ((p (search "coding:" str :start2 start :end2 end)))
    (when p
      (incf p (length "coding:"))
      (loop while (and (< p end)
                       (member (aref str p) '(#\space #\tab)))
            do (incf p))
      (let ((end (position-if (lambda (c) (find c '(#\space #\tab #\newline #\;)))
                              str :start p)))
        (find-external-format (subseq str p end))))))


;;;; Streams

(definterface make-output-stream (write-string)
  "Return a new character output stream.
The stream calls WRITE-STRING when output is ready.")

(definterface make-input-stream (read-string)
  "Return a new character input stream.
The stream calls READ-STRING when input is needed.")

(defvar *auto-flush-interval* 0.2)

(defun auto-flush-loop (stream interval &optional receive)
  (loop
   (when (not (and (open-stream-p stream)
                   (output-stream-p stream)))
     (return nil))
   (force-output stream)
   (when receive
     (receive-if #'identity))
   (sleep interval)))

(definterface make-auto-flush-thread (stream)
  "Make an auto-flush thread"
  (spawn (lambda () (auto-flush-loop stream *auto-flush-interval* nil))
         :name "auto-flush-thread"))


;;;; Documentation

(definterface arglist (name)
   "Return the lambda list for the symbol NAME. NAME can also be
a lisp function object, on lisps which support this.

The result can be a list or the :not-available keyword if the
arglist cannot be determined."
   (declare (ignore name))
   :not-available)

(defgeneric declaration-arglist (decl-identifier)
  (:documentation
   "Return the argument list of the declaration specifier belonging to the
declaration identifier DECL-IDENTIFIER. If the arglist cannot be determined,
the keyword :NOT-AVAILABLE is returned.

The different SWANK backends can specialize this generic function to
include implementation-dependend declaration specifiers, or to provide
additional information on the specifiers defined in ANSI Common Lisp.")
  (:method (decl-identifier)
    (case decl-identifier
      (dynamic-extent '(&rest variables))
      (ignore         '(&rest variables))
      (ignorable      '(&rest variables))
      (special        '(&rest variables))
      (inline         '(&rest function-names))
      (notinline      '(&rest function-names))
      (declaration    '(&rest names))
      (optimize       '(&any compilation-speed debug safety space speed))
      (type           '(type-specifier &rest args))
      (ftype          '(type-specifier &rest function-names))
      (otherwise
       (flet ((typespec-p (symbol)
                (member :type (describe-symbol-for-emacs symbol))))
         (cond ((and (symbolp decl-identifier) (typespec-p decl-identifier))
                '(&rest variables))
               ((and (listp decl-identifier)
                     (typespec-p (first decl-identifier)))
                '(&rest variables))
               (t :not-available)))))))

(defgeneric type-specifier-arglist (typespec-operator)
  (:documentation
   "Return the argument list of the type specifier belonging to
TYPESPEC-OPERATOR.. If the arglist cannot be determined, the keyword
:NOT-AVAILABLE is returned.

The different SWANK backends can specialize this generic function to
include implementation-dependend declaration specifiers, or to provide
additional information on the specifiers defined in ANSI Common Lisp.")
  (:method (typespec-operator)
    (declare (special *type-specifier-arglists*)) ; defined at end of file.
    (typecase typespec-operator
      (symbol (or (cdr (assoc typespec-operator *type-specifier-arglists*))
                  :not-available))
      (t :not-available))))

(definterface type-specifier-p (symbol)
  "Determine if SYMBOL is a type-specifier."
  (or (documentation symbol 'type)
      (not (eq (type-specifier-arglist symbol) :not-available))))

(definterface function-name (function)
  "Return the name of the function object FUNCTION.

The result is either a symbol, a list, or NIL if no function name is
available."
  (declare (ignore function))
  nil)

(definterface valid-function-name-p (form)
  "Is FORM syntactically valid to name a function?
   If true, FBOUNDP should not signal a type-error for FORM."
  (flet ((length=2 (list)
           (and (not (null (cdr list))) (null (cddr list)))))
    (or (symbolp form)
        (and (consp form) (length=2 form)
             (eq (first form) 'setf) (symbolp (second form))))))

(definterface macroexpand-all (form &optional env)
   "Recursively expand all macros in FORM.
Return the resulting form.")

(definterface compiler-macroexpand-1 (form &optional env)
  "Call the compiler-macro for form.
If FORM is a function call for which a compiler-macro has been
defined, invoke the expander function using *macroexpand-hook* and
return the results and T.  Otherwise, return the original form and
NIL."
  (let ((fun (and (consp form)
                  (valid-function-name-p (car form))
                  (compiler-macro-function (car form) env))))
    (if fun
	(let ((result (funcall *macroexpand-hook* fun form env)))
          (values result (not (eq result form))))
	(values form nil))))

(definterface compiler-macroexpand (form &optional env)
  "Repetitively call `compiler-macroexpand-1'."
  (labels ((frob (form expanded)
             (multiple-value-bind (new-form newly-expanded)
                 (compiler-macroexpand-1 form env)
               (if newly-expanded
                   (frob new-form t)
                   (values new-form expanded)))))
    (frob form env)))

(defmacro with-collected-macro-forms
    ((forms &optional result) instrumented-form &body body)
  "Collect macro forms by locally binding *MACROEXPAND-HOOK*.

Evaluates INSTRUMENTED-FORM and collects any forms which undergo
macro-expansion into a list.  Then evaluates BODY with FORMS bound to
the list of forms, and RESULT (optionally) bound to the value of
INSTRUMENTED-FORM."
  (assert (and (symbolp forms) (not (null forms))))
  (assert (symbolp result))
  (let ((result-symbol (or result (gensym))))
   `(call-with-collected-macro-forms
     (lambda (,forms ,result-symbol)
       (declare (ignore ,@(and (not result)
                               `(,result-symbol))))
       ,@body)
     (lambda () ,instrumented-form))))

(defun call-with-collected-macro-forms (body-fn instrumented-fn)
  (let ((return-value nil)
        (collected-forms '()))
    (let* ((real-macroexpand-hook *macroexpand-hook*)
           (*macroexpand-hook*
            (lambda (macro-function form environment)
              (let ((result (funcall real-macroexpand-hook
                                     macro-function form environment)))
                (unless (eq result form)
                  (push form collected-forms))
                result))))
      (setf return-value (funcall instrumented-fn)))
    (funcall body-fn collected-forms return-value)))

(definterface collect-macro-forms (form &optional env)
  "Collect subforms of FORM which undergo (compiler-)macro expansion.
Returns two values: a list of macro forms and a list of compiler macro
forms."
  (with-collected-macro-forms (macro-forms expansion)
      (ignore-errors (macroexpand-all form env))
    (with-collected-macro-forms (compiler-macro-forms)
        (handler-bind ((warning #'muffle-warning))
          (ignore-errors
            (compile nil `(lambda () ,expansion))))
      (values macro-forms compiler-macro-forms))))

(definterface format-string-expand (control-string)
  "Expand the format string CONTROL-STRING."
  (macroexpand `(formatter ,control-string)))

(definterface describe-symbol-for-emacs (symbol)
   "Return a property list describing SYMBOL.

The property list has an entry for each interesting aspect of the
symbol. The recognised keys are:

  :VARIABLE :FUNCTION :SETF :SPECIAL-OPERATOR :MACRO :COMPILER-MACRO
  :TYPE :CLASS :ALIEN-TYPE :ALIEN-STRUCT :ALIEN-UNION :ALIEN-ENUM

The value of each property is the corresponding documentation string,
or NIL (or the obsolete :NOT-DOCUMENTED). It is legal to include keys
not listed here (but slime-print-apropos in Emacs must know about
them).

Properties should be included if and only if they are applicable to
the symbol. For example, only (and all) fbound symbols should include
the :FUNCTION property.

Example:
\(describe-symbol-for-emacs 'vector)
  => (:CLASS :NOT-DOCUMENTED
      :TYPE :NOT-DOCUMENTED
      :FUNCTION \"Constructs a simple-vector from the given objects.\")")

(definterface describe-definition (name type)
  "Describe the definition NAME of TYPE.
TYPE can be any value returned by DESCRIBE-SYMBOL-FOR-EMACS.

Return a documentation string, or NIL if none is available.")


;;;; Debugging

(definterface install-debugger-globally (function)
  "Install FUNCTION as the debugger for all threads/processes. This
usually involves setting *DEBUGGER-HOOK* and, if the implementation
permits, hooking into BREAK as well."
  (setq *debugger-hook* function))

(definterface call-with-debugging-environment (debugger-loop-fn)
   "Call DEBUGGER-LOOP-FN in a suitable debugging environment.

This function is called recursively at each debug level to invoke the
debugger loop. The purpose is to setup any necessary environment for
other debugger callbacks that will be called within the debugger loop.

For example, this is a reasonable place to compute a backtrace, switch
to safe reader/printer settings, and so on.")

(definterface call-with-debugger-hook (hook fun)
  "Call FUN and use HOOK as debugger hook. HOOK can be NIL.

HOOK should be called for both BREAK and INVOKE-DEBUGGER."
  (let ((*debugger-hook* hook))
    (funcall fun)))

(define-condition sldb-condition (condition)
  ((original-condition
    :initarg :original-condition
    :accessor original-condition))
  (:report (lambda (condition stream)
             (format stream "Condition in debugger code~@[: ~A~]"
                     (original-condition condition))))
  (:documentation
   "Wrapper for conditions that should not be debugged.

When a condition arises from the internals of the debugger, it is not
desirable to debug it -- we'd risk entering an endless loop trying to
debug the debugger! Instead, such conditions can be reported to the
user without (re)entering the debugger by wrapping them as
`sldb-condition's."))

;;; The following functions in this section are supposed to be called
;;; within the dynamic contour of CALL-WITH-DEBUGGING-ENVIRONMENT only.

(definterface compute-backtrace (start end)
   "Returns a backtrace of the condition currently being debugged,
that is an ordered list consisting of frames. ``Ordered list''
means that an integer I can be mapped back to the i-th frame of this
backtrace.

START and END are zero-based indices constraining the number of frames
returned. Frame zero is defined as the frame which invoked the
debugger. If END is nil, return the frames from START to the end of
the stack.")

(definterface print-frame (frame stream)
  "Print frame to stream.")

(definterface frame-restartable-p (frame)
  "Is the frame FRAME restartable?.
Return T if `restart-frame' can safely be called on the frame."
  (declare (ignore frame))
  nil)

(definterface frame-source-location (frame-number)
  "Return the source location for the frame associated to FRAME-NUMBER.")

(definterface frame-catch-tags (frame-number)
  "Return a list of catch tags for being printed in a debugger stack
frame."
  (declare (ignore frame-number))
  '())

(definterface frame-locals (frame-number)
  "Return a list of ((&key NAME ID VALUE) ...) where each element of
the list represents a local variable in the stack frame associated to
FRAME-NUMBER.

NAME, a symbol; the name of the local variable.

ID, an integer; used as primary key for the local variable, unique
relatively to the frame under operation.

value, an object; the value of the local variable.")

(definterface frame-var-value (frame-number var-id)
  "Return the value of the local variable associated to VAR-ID
relatively to the frame associated to FRAME-NUMBER.")

(definterface disassemble-frame (frame-number)
  "Disassemble the code for the FRAME-NUMBER.
The output should be written to standard output.
FRAME-NUMBER is a non-negative integer.")

(definterface eval-in-frame (form frame-number)
   "Evaluate a Lisp form in the lexical context of a stack frame
in the debugger.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger.

The return value is the result of evaulating FORM in the
appropriate context.")

(definterface frame-package (frame-number)
  "Return the package corresponding to the frame at FRAME-NUMBER.
Return nil if the backend can't figure it out."
  (declare (ignore frame-number))
  nil)

(definterface frame-call (frame-number)
  "Return a string representing a call to the entry point of a frame.")

(definterface return-from-frame (frame-number form)
  "Unwind the stack to the frame FRAME-NUMBER and return the value(s)
produced by evaluating FORM in the frame context to its caller.

Execute any clean-up code from unwind-protect forms above the frame
during unwinding.

Return a string describing the error if it's not possible to return
from the frame.")

(definterface restart-frame (frame-number)
  "Restart execution of the frame FRAME-NUMBER with the same arguments
as it was called originally.")

(definterface print-condition (condition stream)
  "Print a condition for display in SLDB."
  (princ condition stream))

(definterface condition-extras (condition)
  "Return a list of extra for the debugger.
The allowed elements are of the form:
  (:SHOW-FRAME-SOURCE frame-number)
  (:REFERENCES &rest refs)
"
  (declare (ignore condition))
  '())

(definterface gdb-initial-commands ()
  "List of gdb commands supposed to be executed first for the
   ATTACH-GDB restart."
  nil)

(definterface activate-stepping (frame-number)
  "Prepare the frame FRAME-NUMBER for stepping.")

(definterface sldb-break-on-return (frame-number)
  "Set a breakpoint in the frame FRAME-NUMBER.")

(definterface sldb-break-at-start (symbol)
  "Set a breakpoint on the beginning of the function for SYMBOL.")

(definterface sldb-stepper-condition-p (condition)
  "Return true if SLDB was invoked due to a single-stepping condition,
false otherwise. "
  (declare (ignore condition))
  nil)

(definterface sldb-step-into ()
  "Step into the current single-stepper form.")

(definterface sldb-step-next ()
  "Step to the next form in the current function.")

(definterface sldb-step-out ()
  "Stop single-stepping temporarily, but resume it once the current function
returns.")


;;;; Definition finding

(defstruct (location (:type list)
                      (:constructor make-location
                          (buffer position &optional hints)))
  (type :location)
  buffer position
  ;; Hints is a property list optionally containing:
  ;;   :snippet SOURCE-TEXT
  ;;     This is a snippet of the actual source text at the start of
  ;;     the definition, which could be used in a text search.
  hints)

(defmacro converting-errors-to-error-location (&body body)
  "Catches errors during BODY and converts them to an error location."
  (let ((gblock (gensym "CONVERTING-ERRORS+")))
    `(block ,gblock
       (handler-bind ((error
                       #'(lambda (e)
                            (if *debug-swank-backend*
                                nil     ;decline
                                (return-from ,gblock
                                  (make-error-location e))))))
         ,@body))))

(defun make-error-location (datum &rest args)
  (cond ((typep datum 'condition)
         `(:error ,(format nil "Error: ~A" datum)))
        ((symbolp datum)
         `(:error ,(format nil "Error: ~A"
                           (apply #'make-condition datum args))))
        (t
         (assert (stringp datum))
         `(:error ,(apply #'format nil datum args)))))

(definterface find-definitions (name)
   "Return a list ((DSPEC LOCATION) ...) for NAME's definitions.

NAME is a \"definition specifier\".

DSPEC is a \"definition specifier\" describing the
definition, e.g., FOO or (METHOD FOO (STRING NUMBER)) or
\(DEFVAR FOO).

LOCATION is the source location for the definition.")

(definterface find-source-location (object)
  "Returns the source location of OBJECT, or NIL.

That is the source location of the underlying datastructure of
OBJECT. E.g. on a STANDARD-OBJECT, the source location of the
respective DEFCLASS definition is returned, on a STRUCTURE-CLASS the
respective DEFSTRUCT definition, and so on."
  ;; This returns one source location and not a list of locations. It's
  ;; supposed to return the location of the DEFGENERIC definition on
  ;; #'SOME-GENERIC-FUNCTION.
  (declare (ignore object))
  (make-error-location "FIND-SOURCE-LOCATION is not yet implemented on ~
                        this implementation."))

(definterface buffer-first-change (filename)
  "Called for effect the first time FILENAME's buffer is modified.
CMUCL/SBCL use this to cache the unmodified file and use the
unmodified text to improve the precision of source locations."
  (declare (ignore filename))
  nil)



;;;; XREF

(definterface who-calls (function-name)
  "Return the call sites of FUNCTION-NAME (a symbol).
The results is a list ((DSPEC LOCATION) ...)."
  (declare (ignore function-name))
  :not-implemented)

(definterface calls-who (function-name)
  "Return the call sites of FUNCTION-NAME (a symbol).
The results is a list ((DSPEC LOCATION) ...)."
  (declare (ignore function-name))
  :not-implemented)

(definterface who-references (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is referenced.
See WHO-CALLS for a description of the return value."
  (declare (ignore variable-name))
  :not-implemented)

(definterface who-binds (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is bound.
See WHO-CALLS for a description of the return value."
  (declare (ignore variable-name))
  :not-implemented)

(definterface who-sets (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is set.
See WHO-CALLS for a description of the return value."
  (declare (ignore variable-name))
  :not-implemented)

(definterface who-macroexpands (macro-name)
  "Return the locations where MACRO-NAME (a symbol) is expanded.
See WHO-CALLS for a description of the return value."
  (declare (ignore macro-name))
  :not-implemented)

(definterface who-specializes (class-name)
  "Return the locations where CLASS-NAME (a symbol) is specialized.
See WHO-CALLS for a description of the return value."
  (declare (ignore class-name))
  :not-implemented)

;;; Simpler variants.

(definterface list-callers (function-name)
  "List the callers of FUNCTION-NAME.
This function is like WHO-CALLS except that it is expected to use
lower-level means. Whereas WHO-CALLS is usually implemented with
special compiler support, LIST-CALLERS is usually implemented by
groveling for constants in function objects throughout the heap.

The return value is as for WHO-CALLS.")

(definterface list-callees (function-name)
  "List the functions called by FUNCTION-NAME.
See LIST-CALLERS for a description of the return value.")


;;;; Profiling

;;; The following functions define a minimal profiling interface.

(definterface profile (fname)
  "Marks symbol FNAME for profiling.")

(definterface profiled-functions ()
  "Returns a list of profiled functions.")

(definterface unprofile (fname)
  "Marks symbol FNAME as not profiled.")

(definterface unprofile-all ()
  "Marks all currently profiled functions as not profiled."
  (dolist (f (profiled-functions))
    (unprofile f)))

(definterface profile-report ()
  "Prints profile report.")

(definterface profile-reset ()
  "Resets profile counters.")

(definterface profile-package (package callers-p methods)
  "Wrap profiling code around all functions in PACKAGE.  If a function
is already profiled, then unprofile and reprofile (useful to notice
function redefinition.)

If CALLERS-P is T names have counts of the most common calling
functions recorded.

When called with arguments :METHODS T, profile all methods of all
generic functions having names in the given package.  Generic functions
themselves, that is, their dispatch functions, are left alone.")


;;;; Trace

(definterface toggle-trace (spec)
  "Toggle tracing of the function(s) given with SPEC.
SPEC can be:
 (setf NAME)                            ; a setf function
 (:defmethod NAME QUALIFIER... (SPECIALIZER...)) ; a specific method
 (:defgeneric NAME)                     ; a generic function with all methods
 (:call CALLER CALLEE)                  ; trace calls from CALLER to CALLEE.
 (:labels TOPLEVEL LOCAL)
 (:flet TOPLEVEL LOCAL) ")


;;;; Inspector

(defgeneric emacs-inspect (object)
  (:documentation
   "Explain to Emacs how to inspect OBJECT.

Returns a list specifying how to render the object for inspection.

Every element of the list must be either a string, which will be
inserted into the buffer as is, or a list of the form:

 (:value object &optional format) - Render an inspectable
 object. If format is provided it must be a string and will be
 rendered in place of the value, otherwise use princ-to-string.

 (:newline) - Render a \\n

 (:action label lambda &key (refresh t)) - Render LABEL (a text
 string) which when clicked will call LAMBDA. If REFRESH is
 non-NIL the currently inspected object will be re-inspected
 after calling the lambda.
"))

(defmethod emacs-inspect ((object t))
  "Generic method for inspecting any kind of object.

Since we don't know how to deal with OBJECT we simply dump the
output of CL:DESCRIBE."
   `("Type: " (:value ,(type-of object)) (:newline)
     "Don't know how to inspect the object, dumping output of CL:DESCRIBE:"
     (:newline) (:newline)
     ,(with-output-to-string (desc) (describe object desc))))

(definterface eval-context (object)
  "Return a list of bindings corresponding to OBJECT's slots."
  (declare (ignore object))
  '())

;;; Utilities for inspector methods.
;;;

(defun label-value-line (label value &key (newline t))
  "Create a control list which prints \"LABEL: VALUE\" in the inspector.
If NEWLINE is non-NIL a `(:newline)' is added to the result."
  (list* (princ-to-string label) ": " `(:value ,value)
         (if newline '((:newline)) nil)))

(defmacro label-value-line* (&rest label-values)
  ` (append ,@(loop for (label value) in label-values
                    collect `(label-value-line ,label ,value))))

(definterface describe-primitive-type (object)
  "Return a string describing the primitive type of object."
  (declare (ignore object))
  "N/A")


;;;; Multithreading
;;;
;;; The default implementations are sufficient for non-multiprocessing
;;; implementations.

(definterface initialize-multiprocessing (continuation)
   "Initialize multiprocessing, if necessary and then invoke CONTINUATION.

Depending on the impleimentaion, this function may never return."
   (funcall continuation))

(definterface spawn (fn &key name)
  "Create a new thread to call FN.")

(definterface thread-id (thread)
  "Return an Emacs-parsable object to identify THREAD.

Ids should be comparable with equal, i.e.:
 (equal (thread-id <t1>) (thread-id <t2>)) <==> (eq <t1> <t2>)"
  thread)

(definterface find-thread (id)
  "Return the thread for ID.
ID should be an id previously obtained with THREAD-ID.
Can return nil if the thread no longer exists."
  (declare (ignore id))
  (current-thread))

(definterface thread-name (thread)
   "Return the name of THREAD.
Thread names are short strings meaningful to the user. They do not
have to be unique."
   (declare (ignore thread))
   "The One True Thread")

(definterface thread-status (thread)
   "Return a string describing THREAD's state."
   (declare (ignore thread))
   "")

(definterface thread-attributes (thread)
  "Return a plist of implementation-dependent attributes for THREAD"
  (declare (ignore thread))
  '())

(definterface current-thread ()
  "Return the currently executing thread."
  0)

(definterface all-threads ()
  "Return a fresh list of all threads."
  '())

(definterface thread-alive-p (thread)
  "Test if THREAD is termintated."
  (member thread (all-threads)))

(definterface interrupt-thread (thread fn)
  "Cause THREAD to execute FN.")

(definterface kill-thread (thread)
  "Terminate THREAD immediately.
Don't execute unwind-protected sections, don't raise conditions.
(Do not pass go, do not collect $200.)"
  (declare (ignore thread))
  nil)

(definterface send (thread object)
  "Send OBJECT to thread THREAD."
  (declare (ignore thread))
  object)

(definterface receive (&optional timeout)
  "Return the next message from current thread's mailbox."
  (receive-if (constantly t) timeout))

(definterface receive-if (predicate &optional timeout)
  "Return the first message satisfiying PREDICATE.")

(definterface wake-thread (thread)
  "Trigger a call to CHECK-SLIME-INTERRUPTS in THREAD without using
asynchronous interrupts."
  (declare (ignore thread))
  ;; Doesn't have to implement this if RECEIVE-IF periodically calls
  ;; CHECK-SLIME-INTERRUPTS, but that's energy inefficient
  nil)

(definterface register-thread (name thread)
  "Associate the thread THREAD with the symbol NAME.
The thread can then be retrieved with `find-registered'.
If THREAD is nil delete the association."
  (declare (ignore name thread))
  nil)

(definterface find-registered (name)
  "Find the thread that was registered for the symbol NAME.
Return nil if the no thread was registred or if the tread is dead."
  (declare (ignore name))
  nil)

(definterface set-default-initial-binding (var form)
  "Initialize special variable VAR by default with FORM.

Some implementations initialize certain variables in each newly
created thread.  This function sets the form which is used to produce
the initial value."
  (set var (eval form)))

;; List of delayed interrupts.
;; This should only have thread-local bindings, so no init form.
(defvar *pending-slime-interrupts*)

(defun check-slime-interrupts ()
  "Execute pending interrupts if any.
This should be called periodically in operations which
can take a long time to complete.
Return a boolean indicating whether any interrupts was processed."
  (when (and (boundp '*pending-slime-interrupts*)
             *pending-slime-interrupts*)
    (funcall (pop *pending-slime-interrupts*))
    t))

(defvar *interrupt-queued-handler* nil
  "Function to call on queued interrupts.
Interrupts get queued when an interrupt occurs while interrupt
handling is disabled.

Backends can use this function to abort slow operations.")

(definterface wait-for-input (streams &optional timeout)
  "Wait for input on a list of streams.  Return those that are ready.
STREAMS is a list of streams
TIMEOUT nil, t, or real number. If TIMEOUT is t, return those streams
which are ready (or have reached end-of-file) without waiting.
If TIMEOUT is a number and no streams is ready after TIMEOUT seconds,
return nil.

Return :interrupt if an interrupt occurs while waiting."
  (declare (ignore streams timeout))
  ;; Invoking the slime debugger will just endlessly loop.
  (call-with-debugger-hook
   nil
   (lambda ()
     (error "~s not implemented. Check if ~s = ~s is supported by the implementation."
            'wait-for-input 'swank:*communication-style* swank:*communication-style*))))


;;;;  Locks

;; Please use locks only in swank-gray.lisp.  Locks are too low-level
;; for our taste.

(definterface make-lock (&key name)
   "Make a lock for thread synchronization.
Only one thread may hold the lock (via CALL-WITH-LOCK-HELD) at a time
but that thread may hold it more than once."
   (declare (ignore name))
   :null-lock)

(definterface call-with-lock-held (lock function)
   "Call FUNCTION with LOCK held, queueing if necessary."
   (declare (ignore lock)
            (type function function))
   (funcall function))


;;;; Weak datastructures

(definterface make-weak-key-hash-table (&rest args)
  "Like MAKE-HASH-TABLE, but weak w.r.t. the keys."
  (apply #'make-hash-table args))

(definterface make-weak-value-hash-table (&rest args)
  "Like MAKE-HASH-TABLE, but weak w.r.t. the values."
  (apply #'make-hash-table args))

(definterface hash-table-weakness (hashtable)
  "Return nil or one of :key :value :key-or-value :key-and-value"
  (declare (ignore hashtable))
  nil)


;;;; Floating point

(definterface float-nan-p (float)
  "Return true if FLOAT is a NaN value (Not a Number)."
  ;; When the float type implements IEEE-754 floats, two NaN values
  ;; are never equal; when the implementation does not support NaN,
  ;; the predicate should return false. An implementation can
  ;; implement comparison with "unordered-signaling predicates", which
  ;; emit floating point exceptions.
  (handler-case (not (= float float))
    ;; Comparisons never signal an exception other than the invalid
    ;; operation exception (5.11 Details of comparison predicates).
    (floating-point-invalid-operation () t)))

(definterface float-infinity-p (float)
  "Return true if FLOAT is positive or negative infinity."
  (not (< most-negative-long-float
          float
          most-positive-long-float)))


;;;; Character names

(definterface character-completion-set (prefix matchp)
  "Return a list of names of characters that match PREFIX."
  ;; Handle the standard and semi-standard characters.
  (loop for name in '("Newline" "Space" "Tab" "Page" "Rubout"
                      "Linefeed" "Return" "Backspace")
     when (funcall matchp prefix name)
     collect name))


(defparameter *type-specifier-arglists*
  '((and                . (&rest type-specifiers))
    (array              . (&optional element-type dimension-spec))
    (base-string        . (&optional size))
    (bit-vector         . (&optional size))
    (complex            . (&optional type-specifier))
    (cons               . (&optional car-typespec cdr-typespec))
    (double-float       . (&optional lower-limit upper-limit))
    (eql                . (object))
    (float              . (&optional lower-limit upper-limit))
    (function           . (&optional arg-typespec value-typespec))
    (integer            . (&optional lower-limit upper-limit))
    (long-float         . (&optional lower-limit upper-limit))
    (member             . (&rest eql-objects))
    (mod                . (n))
    (not                . (type-specifier))
    (or                 . (&rest type-specifiers))
    (rational           . (&optional lower-limit upper-limit))
    (real               . (&optional lower-limit upper-limit))
    (satisfies          . (predicate-symbol))
    (short-float        . (&optional lower-limit upper-limit))
    (signed-byte        . (&optional size))
    (simple-array       . (&optional element-type dimension-spec))
    (simple-base-string . (&optional size))
    (simple-bit-vector  . (&optional size))
    (simple-string      . (&optional size))
    (single-float       . (&optional lower-limit upper-limit))
    (simple-vector      . (&optional size))
    (string             . (&optional size))
    (unsigned-byte      . (&optional size))
    (values             . (&rest typespecs))
    (vector             . (&optional element-type size))
    ))

;;; Heap dumps

(definterface save-image (filename &optional restart-function)
  "Save a heap image to the file FILENAME.
RESTART-FUNCTION, if non-nil, should be called when the image is loaded.")

(definterface background-save-image (filename &key restart-function
                                              completion-function)
  "Request saving a heap image to the file FILENAME.
RESTART-FUNCTION, if non-nil, should be called when the image is loaded.
COMPLETION-FUNCTION, if non-nil, should be called after saving the image.")

(defun deinit-log-output ()
  ;; Can't hang on to an fd-stream from a previous session.
  (setf *log-output* nil))


;;;; Wrapping

(definterface wrap (spec indicator &key before after replace)
  "Intercept future calls to SPEC and surround them in callbacks.

INDICATOR is a symbol identifying a particular wrapping, and is used
to differentiate between multiple wrappings.

Implementations intercept calls to SPEC and call, in this order:

* the BEFORE callback, if it's provided, with a single argument set to
  the list of arguments passed to the intercepted call;

* the original definition of SPEC recursively honouring any wrappings
  previously established under different values of INDICATOR. If the
  compatible function REPLACE is provided, call that instead.

* the AFTER callback, if it's provided, with a single set to the list
  of values returned by the previous call, or, if that call exited
  non-locally, a single descriptive symbol, like :EXITED-NON-LOCALLY."
  (declare (ignore indicator))
  (assert (symbolp spec) nil
          "The default implementation for WRAP allows only simple names")
  (assert (null (get spec 'slime-wrap)) nil
          "The default implementation for WRAP allows a single wrapping")
  (let* ((saved (symbol-function spec))
         (replacement (lambda (&rest args)
                        (let (retlist completed)
                          (unwind-protect
                               (progn
                                 (when before
                                   (funcall before args))
                                 (setq retlist (multiple-value-list
                                                (apply (or replace
                                                           saved) args)))
                                 (setq completed t)
                                 (values-list retlist))
                            (when after
                              (funcall after (if completed
                                                 retlist
                                                 :exited-non-locally))))))))
    (setf (get spec 'slime-wrap) (list saved replacement))
    (setf (symbol-function spec) replacement))
  spec)

(definterface unwrap (spec indicator)
  "Remove from SPEC any wrappings tagged with INDICATOR."
  (if (wrapped-p spec indicator)
      (setf (symbol-function spec) (first (get spec 'slime-wrap)))
      (cerror "All right, so I did"
              "Hmmm, ~a is not correctly wrapped, you probably redefined it"
              spec))
  (setf (get spec 'slime-wrap) nil)
  spec)

(definterface wrapped-p (spec indicator)
  "Returns true if SPEC is wrapped with INDICATOR."
  (declare (ignore indicator))
  (and (symbolp spec)
       (let ((prop-value (get spec 'slime-wrap)))
         (cond ((and prop-value
                     (not (eq (second prop-value)
                              (symbol-function spec))))
                (warn "~a appears to be incorrectly wrapped" spec)
                nil)
               (prop-value t)
               (t nil)))))
