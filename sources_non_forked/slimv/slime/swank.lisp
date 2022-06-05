;;;; swank.lisp --- Server for SLIME commands.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file defines the "Swank" TCP server for Emacs to talk to. The
;;; code in this file is purely portable Common Lisp. We do require a
;;; smattering of non-portable functions in order to write the server,
;;; so we have defined them in `swank/backend.lisp' and implemented
;;; them separately for each Lisp implementation. These extensions are
;;; available to us here via the `SWANK/BACKEND' package.

(in-package :swank)
;;;; Top-level variables, constants, macros

(defconstant cl-package (find-package :cl)
  "The COMMON-LISP package.")

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defconstant default-server-port 4005
  "The default TCP port for the server (when started manually).")

(defvar *swank-debug-p* t
  "When true, print extra debugging information.")

(defvar *backtrace-pprint-dispatch-table*
  (let ((table (copy-pprint-dispatch nil)))
    (flet ((print-string (stream string)
             (cond (*print-escape* 
                    (escape-string string stream
                                   :map '((#\" . "\\\"")
                                          (#\\ . "\\\\")
                                          (#\newline . "\\n")
                                          (#\return . "\\r"))))
                   (t (write-string string stream)))))
      (set-pprint-dispatch 'string  #'print-string 0 table)
      table)))

(defvar *backtrace-printer-bindings*
  `((*print-pretty*           . t)
    (*print-readably*         . nil)
    (*print-level*            . 4)
    (*print-length*           . 6)
    (*print-lines*            . 1)
    (*print-right-margin*     . 200)
    (*print-pprint-dispatch*  . ,*backtrace-pprint-dispatch-table*))
  "Pretter settings for printing backtraces.")

(defvar *default-worker-thread-bindings* '()
  "An alist to initialize dynamic variables in worker threads.  
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE.")

(defun call-with-bindings (alist fun)
  "Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."
  (if (null alist)
      (funcall fun)
      (let* ((rlist (reverse alist))
             (vars (mapcar #'car rlist))
             (vals (mapcar #'cdr rlist)))
        (progv vars vals
          (funcall fun)))))

(defmacro with-bindings (alist &body body)
  "See `call-with-bindings'."
  `(call-with-bindings ,alist (lambda () ,@body)))

;;; The `DEFSLIMEFUN' macro defines a function that Emacs can call via
;;; RPC.

(defmacro defslimefun (name arglist &body rest)
  "A DEFUN for functions that Emacs can call by RPC."
  `(progn
     (defun ,name ,arglist ,@rest)
     ;; see <http://www.franz.com/support/documentation/6.2/\
     ;; doc/pages/variables/compiler/\
     ;; s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name (symbol-package ',name)))))

(defun missing-arg ()
  "A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."
  (error "A required &KEY or &OPTIONAL argument was not supplied."))


;;;; Hooks
;;;
;;; We use Emacs-like `add-hook' and `run-hook' utilities to support
;;; simple indirection. The interface is more CLish than the Emacs
;;; Lisp one.

(defmacro add-hook (place function)
  "Add FUNCTION to the list of values on PLACE."
  `(pushnew ,function ,place))

(defun run-hook (functions &rest arguments)
  "Call each of FUNCTIONS with ARGUMENTS."
  (dolist (function functions)
    (apply function arguments)))

(defun run-hook-until-success (functions &rest arguments)
  "Call each of FUNCTIONS with ARGUMENTS, stop if any function returns
a truthy value"
  (loop for hook in functions
          thereis (apply hook arguments)))

(defvar *new-connection-hook* '()
  "This hook is run each time a connection is established.
The connection structure is given as the argument.
Backend code should treat the connection structure as opaque.")

(defvar *connection-closed-hook* '()
  "This hook is run when a connection is closed.
The connection as passed as an argument.
Backend code should treat the connection structure as opaque.")

(defvar *pre-reply-hook* '()
  "Hook run (without arguments) immediately before replying to an RPC.")

(defvar *after-init-hook* '()
  "Hook run after user init files are loaded.")


;;;; Connections
;;;
;;; Connection structures represent the network connections between
;;; Emacs and Lisp. Each has a socket stream, a set of user I/O
;;; streams that redirect to Emacs, and optionally a second socket
;;; used solely to pipe user-output to Emacs (an optimization).  This
;;; is also the place where we keep everything that needs to be
;;; freed/closed/killed when we disconnect.

(defstruct (connection
             (:constructor %make-connection)
             (:conc-name connection.)
             (:print-function print-connection))
  ;; The listening socket. (usually closed)
  (socket           (missing-arg) :type t :read-only t)
  ;; Character I/O stream of socket connection.  Read-only to avoid
  ;; race conditions during initialization.
  (socket-io        (missing-arg) :type stream :read-only t)
  ;; Optional dedicated output socket (backending `user-output' slot).
  ;; Has a slot so that it can be closed with the connection.
  (dedicated-output nil :type (or stream null))
  ;; Streams that can be used for user interaction, with requests
  ;; redirected to Emacs.
  (user-input       nil :type (or stream null))
  (user-output      nil :type (or stream null))
  (user-io          nil :type (or stream null))
  ;; Bindings used for this connection (usually streams)
  (env '() :type list)
  ;; A stream that we use for *trace-output*; if nil, we user user-output.
  (trace-output     nil :type (or stream null))
  ;; A stream where we send REPL results.
  (repl-results     nil :type (or stream null))
  ;; Cache of macro-indentation information that has been sent to Emacs.
  ;; This is used for preparing deltas to update Emacs's knowledge.
  ;; Maps: symbol -> indentation-specification
  (indentation-cache (make-hash-table :test 'eq) :type hash-table)
  ;; The list of packages represented in the cache:
  (indentation-cache-packages '())
  ;; The communication style used.
  (communication-style nil :type (member nil :spawn :sigio :fd-handler))
  )

(defun print-connection (conn stream depth)
  (declare (ignore depth))
  (print-unreadable-object (conn stream :type t :identity t)))

(defstruct (singlethreaded-connection (:include connection)
                                      (:conc-name sconn.))
  ;; The SIGINT handler we should restore when the connection is
  ;; closed.
  saved-sigint-handler
  ;; A queue of events.  Not all events can be processed in order and
  ;; we need a place to stored them.
  (event-queue '() :type list)
  ;; A counter that is incremented whenever an event is added to the
  ;; queue.  This is used to detected modifications to the event queue
  ;; by interrupts.  The counter wraps around.
  (events-enqueued 0 :type fixnum))

(defstruct (multithreaded-connection (:include connection)
                                     (:conc-name mconn.))
  ;; In multithreaded systems we delegate certain tasks to specific
  ;; threads. The `reader-thread' is responsible for reading network
  ;; requests from Emacs and sending them to the `control-thread'; the
  ;; `control-thread' is responsible for dispatching requests to the
  ;; threads that should handle them; the `repl-thread' is the one
  ;; that evaluates REPL expressions. The control thread dispatches
  ;; all REPL evaluations to the REPL thread and for other requests it
  ;; spawns new threads.
  reader-thread
  control-thread
  repl-thread
  auto-flush-thread
  indentation-cache-thread
  ;; List of threads that are currently processing requests.  We use
  ;; this to find the newest/current thread for an interrupt.  In the
  ;; future we may store here (thread . request-tag) pairs so that we
  ;; can interrupt specific requests.
  (active-threads '() :type list)
  )

(defvar *emacs-connection* nil
  "The connection to Emacs currently in use.")

(defun make-connection (socket stream style)
  (let ((conn (funcall (ecase style
                         (:spawn 
                          #'make-multithreaded-connection)
                         ((:sigio nil :fd-handler)
                          #'make-singlethreaded-connection))
                       :socket socket
                       :socket-io stream
                       :communication-style style)))
    (run-hook *new-connection-hook* conn)
    (send-to-sentinel `(:add-connection ,conn))
    conn))

(defslimefun ping (tag)
  tag)

(defun safe-backtrace ()
  (ignore-errors 
    (call-with-debugging-environment 
     (lambda () (backtrace 0 nil)))))

(define-condition swank-error (error) 
  ((backtrace :initarg :backtrace :reader swank-error.backtrace)
   (condition :initarg :condition :reader swank-error.condition))
  (:report (lambda (c s) (princ (swank-error.condition c) s)))
  (:documentation "Condition which carries a backtrace."))

(defun signal-swank-error (condition &optional (backtrace (safe-backtrace)))
  (error 'swank-error :condition condition :backtrace backtrace))

(defvar *debug-on-swank-protocol-error* nil
  "When non-nil invoke the system debugger on errors that were
signalled during decoding/encoding the wire protocol.  Do not set this
to T unless you want to debug swank internals.")

(defmacro with-swank-error-handler ((connection) &body body)
  "Close the connection on internal `swank-error's."
  (let ((conn (gensym)))
  `(let ((,conn ,connection))
     (handler-case 
         (handler-bind ((swank-error 
                         (lambda (condition)
                           (when *debug-on-swank-protocol-error*
                             (invoke-default-debugger condition)))))
           (progn . ,body))
       (swank-error (condition)
         (close-connection ,conn
                           (swank-error.condition condition)
                           (swank-error.backtrace condition)))))))

(defmacro with-panic-handler ((connection) &body body)
  "Close the connection on unhandled `serious-condition's."
  (let ((conn (gensym)))
    `(let ((,conn ,connection))
       (handler-bind ((serious-condition
                        (lambda (condition)
                          (close-connection ,conn condition (safe-backtrace))
                          (abort condition))))
         . ,body))))

(add-hook *new-connection-hook* 'notify-backend-of-connection)
(defun notify-backend-of-connection (connection)
  (declare (ignore connection))
  (emacs-connected))


;;;; Utilities


;;;;; Logging

(defvar *swank-io-package*
  (let ((package (make-package :swank-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defvar *log-events* nil)

(defun init-log-output ()
  (unless *log-output*
    (setq *log-output* (real-output-stream *error-output*))))

(add-hook *after-init-hook* 'init-log-output)

(defun real-input-stream (stream)
  (typecase stream
    (synonym-stream 
     (real-input-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-input-stream (two-way-stream-input-stream stream)))
    (t stream)))

(defun real-output-stream (stream)
  (typecase stream
    (synonym-stream 
     (real-output-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-output-stream (two-way-stream-output-stream stream)))
    (t stream)))

(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (when *enable-event-history*
        (setf (aref *event-history* *event-history-index*) 
              (format nil "~?" format-string args))
        (setf *event-history-index* 
              (mod (1+ *event-history-index*) (length *event-history*))))
      (when *log-events*
        (write-string (escape-non-ascii (format nil "~?" format-string args))
                      *log-output*)
        (force-output *log-output*)))))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun clear-event-history ()
  (fill *event-history* nil)
  (setq *event-history-index* 0))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t 
         (write-string
          (escape-non-ascii (format nil "Unexpected event: ~A~%" event))
          stream))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c) 
  (<= (char-code c) 127))


;;;;; Helper macros

(defmacro dcase (value &body patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect 
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands 
                               ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "dcase failed: ~S" ,tmp))))))))


;;;; Interrupt handling 

;; Usually we'd like to enter the debugger when an interrupt happens.
;; But for some operations, in particular send&receive, it's crucial
;; that those are not interrupted when the mailbox is in an
;; inconsistent/locked state. Obviously, if send&receive don't work we
;; can't communicate and the debugger will not work.  To solve that
;; problem, we try to handle interrupts only at certain safe-points.
;;
;; Whenever an interrupt happens we call the function
;; INVOKE-OR-QUEUE-INTERRUPT.  Usually this simply invokes the
;; debugger, but if interrupts are disabled the interrupt is put in a
;; queue for later processing.  At safe-points, we call
;; CHECK-SLIME-INTERRUPTS which looks at the queue and invokes the
;; debugger if needed.
;;
;; The queue for interrupts is stored in a thread local variable.
;; WITH-CONNECTION sets it up.  WITH-SLIME-INTERRUPTS allows
;; interrupts, i.e. the debugger is entered immediately.  When we call
;; "user code" or non-problematic code we allow interrupts.  When
;; inside WITHOUT-SLIME-INTERRUPTS, interrupts are queued.  When we
;; switch from "user code" to more delicate operations we need to
;; disable interrupts.  In particular, interrupts should be disabled
;; for SEND and RECEIVE-IF.

;; If true execute interrupts, otherwise queue them.
;; Note: `with-connection' binds *pending-slime-interrupts*.
(defvar *slime-interrupts-enabled*)

(defmacro with-interrupts-enabled% (flag body)
  `(progn
     ,@(if flag '((check-slime-interrupts)))
     (multiple-value-prog1
         (let ((*slime-interrupts-enabled* ,flag))
           ,@body)
       ,@(if flag '((check-slime-interrupts))))))

(defmacro with-slime-interrupts (&body body)
  `(with-interrupts-enabled% t ,body))

(defmacro without-slime-interrupts (&body body)
  `(with-interrupts-enabled% nil ,body))

(defun queue-thread-interrupt (thread function)
  (interrupt-thread thread
                    (lambda ()
                      ;; safely interrupt THREAD
                      (when (invoke-or-queue-interrupt function)
                        (wake-thread thread)))))

(defun invoke-or-queue-interrupt (function)
  (log-event "invoke-or-queue-interrupt: ~a~%" function)
  (cond ((not (boundp '*slime-interrupts-enabled*))
         (without-slime-interrupts
           (funcall function)))
        (*slime-interrupts-enabled*
         (log-event "interrupts-enabled~%")
         (funcall function))
        (t
         (setq *pending-slime-interrupts*
               (nconc *pending-slime-interrupts*
                      (list function)))
         (cond ((cdr *pending-slime-interrupts*)
                (log-event "too many queued interrupts~%")
                (with-simple-restart (continue "Continue from interrupt")
                  (handler-bind ((serious-condition #'invoke-slime-debugger))
                    (check-slime-interrupts))))
               (t
                (log-event "queue-interrupt: ~a~%" function)
                (when *interrupt-queued-handler*
                  (funcall *interrupt-queued-handler*))
                t)))))


;;; FIXME: poor name?
(defmacro with-io-redirection ((connection) &body body)
  "Execute BODY I/O redirection to CONNECTION. "
  `(with-bindings (connection.env ,connection)
     . ,body))

;; Thread local variable used for flow-control.
;; It's bound by `with-connection'.
(defvar *send-counter*)

(defmacro with-connection ((connection) &body body)
  "Execute BODY in the context of CONNECTION."
  `(let ((connection ,connection)
         (function (lambda () . ,body)))
     (if (eq *emacs-connection* connection)
         (funcall function)
         (let ((*emacs-connection* connection)
               (*pending-slime-interrupts* '())
               (*send-counter* 0))
           (without-slime-interrupts
             (with-swank-error-handler (connection)
               (with-io-redirection (connection)
                 (call-with-debugger-hook #'swank-debugger-hook 
                                          function))))))))

(defun call-with-retry-restart (msg thunk)
  (loop (with-simple-restart (retry "~a" msg)
          (return (funcall thunk)))))

(defmacro with-retry-restart ((&key (msg "Retry.")) &body body)
  (check-type msg string)
  `(call-with-retry-restart ,msg (lambda () ,@body)))

(defmacro with-struct* ((conc-name get obj) &body body)
  (let ((var (gensym)))
    `(let ((,var ,obj))
       (macrolet ((,get (slot)
                    (let ((getter (intern (concatenate 'string
                                                       ',(string conc-name)
                                                       (string slot))
                                          (symbol-package ',conc-name))))
                      `(,getter ,',var))))
         ,@body))))

(defmacro define-special (name doc)
  "Define a special variable NAME with doc string DOC.
This is like defvar, but NAME will not be initialized."
  `(progn
    (defvar ,name)
    (setf (documentation ',name 'variable) ,doc)))


;;;;; Sentinel
;;;
;;; The sentinel thread manages some global lists.
;;; FIXME: Overdesigned?

(defvar *connections* '()
  "List of all active connections, with the most recent at the front.")

(defvar *servers* '()
  "A list ((server-socket port thread) ...) describing the listening sockets.
Used to close sockets on server shutdown or restart.")

;; FIXME: we simply access the global variable here.  We could ask the
;; sentinel thread instead but then we still have the problem that the
;; connection could be closed before we use it.  
(defun default-connection ()
  "Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."
  (car *connections*))

(defun start-sentinel () 
  (unless (find-registered 'sentinel)
    (let ((thread (spawn #'sentinel :name "Swank Sentinel")))
      (register-thread 'sentinel thread))))

(defun sentinel ()
  (catch 'exit-sentinel
    (loop (sentinel-serve (receive)))))

(defun send-to-sentinel (msg)
  (let ((sentinel (find-registered 'sentinel)))
    (cond (sentinel (send sentinel msg))
          (t (sentinel-serve msg)))))

(defun sentinel-serve (msg)
  (dcase msg
    ((:add-connection conn)
     (push conn *connections*))
    ((:close-connection connection condition backtrace)
     (close-connection% connection condition backtrace)
     (sentinel-maybe-exit))
    ((:add-server socket port thread)
     (push (list socket port thread) *servers*))
    ((:stop-server key port)
     (sentinel-stop-server key port)
     (sentinel-maybe-exit))))

(defun sentinel-stop-server (key value)
  (let ((probe (find value *servers* :key (ecase key 
                                            (:socket #'car)
                                            (:port #'cadr)))))
    (cond (probe 
           (setq *servers* (delete probe *servers*))
           (destructuring-bind (socket _port thread) probe
             (declare (ignore _port))
             (ignore-errors (close-socket socket))
             (when (and thread 
                        (thread-alive-p thread)
                        (not (eq thread (current-thread))))
               (ignore-errors (kill-thread thread)))))
          (t
           (warn "No server for ~s: ~s" key value)))))

(defun sentinel-maybe-exit ()
  (when (and (null *connections*)
             (null *servers*)
             (and (current-thread)
                  (eq (find-registered 'sentinel)
                      (current-thread))))
    (register-thread 'sentinel nil)
    (throw 'exit-sentinel nil)))


;;;;; Misc

(defun use-threads-p ()
  (eq (connection.communication-style *emacs-connection*) :spawn))

(defun current-thread-id ()
  (thread-id (current-thread)))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))


;;;;; Symbols

;; FIXME: this docstring is more confusing than helpful.
(defun symbol-status (symbol &optional (package (symbol-package symbol)))
  "Returns one of 

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is
  
     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)
  
        You can check that with: (AND (SYMBOL-STATUS S P) T)
  
  
     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.
  
                   Or more simply, if S is not _inherited_.
  
        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :INHERITED))))
  
  
     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.
  
                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.
  
        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)
  
  
     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :EXTERNAL))))
  

        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does. 

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited 
        from other packages, but only about the symbols directly specific
        to the package in question.
"
  (when package     ; may be NIL when symbol is completely uninterned.
    (check-type symbol symbol) (check-type package package)
    (multiple-value-bind (present-symbol status)
        (find-symbol (symbol-name symbol) package)
      (and (eq symbol present-symbol) status))))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (eq (symbol-status symbol package) :external))


;;;; TCP Server

(defvar *communication-style* (preferred-communication-style))

(defvar *dont-close* nil
  "Default value of :dont-close argument to start-server and
  create-server.")

(defparameter *loopback-interface* "localhost")

(defun start-server (port-file &key (style *communication-style*)
                                    (dont-close *dont-close*))
  "Start the server and write the listen port number to PORT-FILE.
This is the entry point for Emacs."
  (setup-server 0
                (lambda (port) (announce-server-port port-file port))
                style dont-close nil))

(defun create-server (&key (port default-server-port)
                        (style *communication-style*)
                        (dont-close *dont-close*)
                        interface
                        backlog)
  "Start a SWANK server on PORT running in STYLE.
If DONT-CLOSE is true then the listen socket will accept multiple
connections, otherwise it will be closed after the first.

Optionally, an INTERFACE could be specified and swank will bind
the PORT on this interface. By default, interface is \"localhost\"."
  (let ((*loopback-interface* (or interface
                                  *loopback-interface*)))
    (setup-server port #'simple-announce-function
                  style dont-close backlog)))

(defun find-external-format-or-lose (coding-system)
  (or (find-external-format coding-system)
      (error "Unsupported coding system: ~s" coding-system)))

(defmacro restart-loop (form &body clauses)
  "Executes FORM, with restart-case CLAUSES which have a chance to modify FORM's
environment before trying again (by returning normally) or giving up (through an
explicit transfer of control), all within an implicit block named nil.
e.g.: (restart-loop (http-request url) (use-value (new) (setq url new)))"
  `(loop (restart-case (return ,form) ,@clauses)))

(defun socket-quest (port backlog)
  (restart-loop (create-socket *loopback-interface* port :backlog backlog)
    (use-value (&optional (new-port (1+ port)))
      :report (lambda (stream) (format stream "Try a port other than ~D" port))
      :interactive
      (lambda ()
        (format *query-io* "Enter port (defaults to ~D): " (1+ port))
        (finish-output *query-io*)      ; necessary for tunnels
        (ignore-errors (list (parse-integer (read-line *query-io*)))))
      (setq port new-port))))

(defun setup-server (port announce-fn style dont-close backlog)
  (init-log-output)
  (let* ((socket (socket-quest port backlog))
         (port (local-port socket)))
    (funcall announce-fn port)
    (labels ((serve () (accept-connections socket style dont-close))
             (note () (send-to-sentinel `(:add-server ,socket ,port
                                                      ,(current-thread))))
             (serve-loop () (note) (loop do (serve) while dont-close)))
      (ecase style
        (:spawn (initialize-multiprocessing
                 (lambda ()
                   (start-sentinel)
                   (spawn #'serve-loop :name (format nil "Swank ~s" port)))))
        ((:fd-handler :sigio)
         (note)
         (add-fd-handler socket #'serve))
        ((nil) (serve-loop))))
    port))

(defun stop-server (port)
  "Stop server running on PORT."
  (send-to-sentinel `(:stop-server :port ,port)))

(defun restart-server (&key (port default-server-port)
                       (style *communication-style*)
                       (dont-close *dont-close*))
  "Stop the server listening on PORT, then start a new SWANK server 
on PORT running in STYLE. If DONT-CLOSE is true then the listen socket 
will accept multiple connections, otherwise it will be closed after the 
first."
  (stop-server port)
  (sleep 5)
  (create-server :port port :style style :dont-close dont-close))

(defun accept-connections (socket style dont-close)
  (unwind-protect
       (let ((client (accept-connection socket :external-format nil
                                               :buffering t)))
         (authenticate-client client)
         (serve-requests (make-connection socket client style)))
    (unless dont-close
      (send-to-sentinel `(:stop-server :socket ,socket)))))

(defun authenticate-client (stream)
  (let ((secret (slime-secret)))
    (when secret
      (set-stream-timeout stream 20)
      (let ((first-val (read-packet stream)))
        (unless (and (stringp first-val) (string= first-val secret))
          (error "Incoming connection doesn't know the password.")))
      (set-stream-timeout stream nil))))

(defun slime-secret ()
  "Finds the magic secret from the user's home directory.  Returns nil
if the file doesn't exist; otherwise the first line of the file."
  (with-open-file (in
                   (merge-pathnames (user-homedir-pathname) #p".slime-secret")
                   :if-does-not-exist nil)
    (and in (read-line in nil ""))))

(defun serve-requests (connection)
  "Read and process all requests on connections."
  (etypecase connection
    (multithreaded-connection
     (spawn-threads-for-connection connection))
    (singlethreaded-connection
     (ecase (connection.communication-style connection)
       ((nil) (simple-serve-requests connection))
       (:sigio (install-sigio-handler connection))
       (:fd-handler (install-fd-handler connection))))))

(defun stop-serving-requests (connection)
  (etypecase connection
    (multithreaded-connection
     (cleanup-connection-threads connection))
    (singlethreaded-connection
     (ecase (connection.communication-style connection)
       ((nil))
       (:sigio (deinstall-sigio-handler connection))
       (:fd-handler (deinstall-fd-handler connection))))))

(defun announce-server-port (file port)
  (with-open-file (s file
                     :direction :output
                     :if-exists :error
                     :if-does-not-exist :create)
    (format s "~S~%" port))
  (simple-announce-function port))

(defun simple-announce-function (port)
  (when *swank-debug-p*
    (format *log-output* "~&;; Swank started at port: ~D.~%" port)
    (force-output *log-output*)))


;;;;; Event Decoding/Encoding

(defun decode-message (stream)
  "Read an S-expression from STREAM using the SLIME protocol."
  (log-event "decode-message~%")
  (without-slime-interrupts
    (handler-bind ((error #'signal-swank-error))
      (handler-case (read-message stream *swank-io-package*)
        (swank-reader-error (c) 
          `(:reader-error ,(swank-reader-error.packet c)
                          ,(swank-reader-error.cause c)))))))

(defun encode-message (message stream)
  "Write an S-expression to STREAM using the SLIME protocol."
  (log-event "encode-message~%")
  (without-slime-interrupts
    (handler-bind ((error #'signal-swank-error))
      (write-message message *swank-io-package* stream))))


;;;;; Event Processing

(defvar *sldb-quit-restart* nil
  "The restart that will be invoked when the user calls sldb-quit.")

;; Establish a top-level restart and execute BODY.
;; Execute K if the restart is invoked.
(defmacro with-top-level-restart ((connection k) &body body)
  `(with-connection (,connection)
     (restart-case
         (let ((*sldb-quit-restart* (find-restart 'abort)))
           ,@body)
       (abort (&optional v)
         :report "Return to SLIME's top level."
         (declare (ignore v))
         (force-user-output)
         ,k))))

(defun handle-requests (connection &optional timeout)
  "Read and process :emacs-rex requests.
The processing is done in the extent of the toplevel restart."
  (with-connection (connection)
    (cond (*sldb-quit-restart*
           (process-requests timeout))
          (t
           (tagbody
            start
              (with-top-level-restart (connection (go start))
                (process-requests timeout)))))))

(defun process-requests (timeout)
  "Read and process requests from Emacs."
  (loop
   (multiple-value-bind (event timeout?)
       (wait-for-event `(or (:emacs-rex . _)
                            (:emacs-channel-send . _))
                       timeout)
    (when timeout? (return))
    (dcase event
      ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
      ((:emacs-channel-send channel (selector &rest args))
       (channel-send channel selector args))))))

(defun current-socket-io ()
  (connection.socket-io *emacs-connection*))

(defun close-connection (connection condition backtrace)
  (send-to-sentinel `(:close-connection ,connection ,condition ,backtrace)))

(defun close-connection% (c condition backtrace)
  (let ((*debugger-hook* nil))
    (log-event "close-connection: ~a ...~%" condition)
    (format *log-output* "~&;; swank:close-connection: ~A~%"
            (escape-non-ascii (safe-condition-message condition)))
    (stop-serving-requests c)
    (close (connection.socket-io c))
    (when (connection.dedicated-output c)
      (ignore-errors (close (connection.dedicated-output c))))
    (setf *connections* (remove c *connections*))
    (run-hook *connection-closed-hook* c)
    (when (and condition (not (typep condition 'end-of-file)))
      (finish-output *log-output*)
      (format *log-output* "~&;; Event history start:~%")
      (dump-event-history *log-output*)
      (format *log-output* "~
;; Event history end.~%~
;; Backtrace:~%~{~A~%~}~
;; Connection to Emacs lost. [~%~
;;  condition: ~A~%~
;;  type: ~S~%~
;;  style: ~S]~%"
              (loop for (i f) in backtrace collect 
                    (ignore-errors 
                      (format nil "~d: ~a" i (escape-non-ascii f))))
              (escape-non-ascii (safe-condition-message condition) )
              (type-of condition)
              (connection.communication-style c)))
    (finish-output *log-output*)
    (log-event "close-connection ~a ... done.~%" condition)))

;;;;;; Thread based communication

(defun read-loop (connection)
  (let ((input-stream (connection.socket-io connection))
        (control-thread (mconn.control-thread connection)))
    (with-swank-error-handler (connection)
      (loop (send control-thread (decode-message input-stream))))))

(defun dispatch-loop (connection)
  (let ((*emacs-connection* connection))
    (with-panic-handler (connection)
      (loop (dispatch-event connection (receive))))))

(defgeneric thread-for-evaluation (connection id)
  (:documentation "Find or create a thread to evaluate the next request.")
  (:method ((connection multithreaded-connection) (id (eql t)))
    (spawn-worker-thread connection))
  (:method ((connection multithreaded-connection) (id (eql :find-existing)))
    (car (mconn.active-threads connection)))
  (:method (connection (id integer))
    (declare (ignorable connection))
    (find-thread id))
  (:method ((connection singlethreaded-connection) id)
    (declare (ignorable connection connection id))
    (current-thread)))

(defun interrupt-worker-thread (connection id)
  (let ((thread (thread-for-evaluation connection
                                       (cond ((eq id t) :find-existing)
                                             (t id)))))
    (log-event "interrupt-worker-thread: ~a ~a~%" id thread)
    (if thread
        (etypecase connection
          (multithreaded-connection
           (queue-thread-interrupt thread #'simple-break))
          (singlethreaded-connection
           (simple-break)))
        (encode-message (list :debug-condition (current-thread-id)
                              (format nil "Thread with id ~a not found" 
                                      id))
                        (current-socket-io)))))

(defun spawn-worker-thread (connection)
  (spawn (lambda () 
           (with-bindings *default-worker-thread-bindings*
             (with-top-level-restart (connection nil)
               (apply #'eval-for-emacs 
                      (cdr (wait-for-event `(:emacs-rex . _)))))))
         :name "worker"))

(defun add-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection 
     (push thread (mconn.active-threads connection)))
    (singlethreaded-connection)))

(defun remove-active-thread (connection thread)
  (etypecase connection
    (multithreaded-connection
     (setf (mconn.active-threads connection)
           (delete thread (mconn.active-threads connection) :count 1)))
    (singlethreaded-connection)))

(defparameter *event-hook* nil)

(defun dispatch-event (connection event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "dispatch-event: ~s~%" event)
  (or (run-hook-until-success *event-hook* connection event)
      (dcase event
        ((:emacs-rex form package thread-id id)
         (let ((thread (thread-for-evaluation connection thread-id)))
           (cond (thread
                  (add-active-thread connection thread)
                  (send-event thread `(:emacs-rex ,form ,package ,id)))
                 (t
                  (encode-message
                   (list :invalid-rpc id
                         (format nil "Thread not found: ~s" thread-id))
                   (current-socket-io))))))
        ((:return thread &rest args)
         (remove-active-thread connection thread)
         (encode-message `(:return ,@args) (current-socket-io)))
        ((:emacs-interrupt thread-id)
         (interrupt-worker-thread connection thread-id))
        (((:write-string
           :debug :debug-condition :debug-activate :debug-return :channel-send
           :presentation-start :presentation-end
           :new-package :new-features :ed :indentation-update
           :eval :eval-no-wait :background-message :inspect :ping
           :y-or-n-p :read-from-minibuffer :read-string :read-aborted :test-delay
           :write-image :ed-rpc :ed-rpc-no-wait)
          &rest _)
         (declare (ignore _))
         (encode-message event (current-socket-io)))
        (((:emacs-pong :emacs-return :emacs-return-string :ed-rpc-forbidden)
          thread-id &rest args)
         (send-event (find-thread thread-id) (cons (car event) args)))
        ((:emacs-channel-send channel-id msg)
         (let ((ch (find-channel channel-id)))
           (send-event (channel-thread ch) `(:emacs-channel-send ,ch ,msg))))
        ((:reader-error packet condition)
         (encode-message `(:reader-error ,packet
                                         ,(safe-condition-message condition))
                         (current-socket-io))))))


(defun send-event (thread event)
  (log-event "send-event: ~s ~s~%" thread event)
  (let ((c *emacs-connection*))
    (etypecase c
      (multithreaded-connection 
       (send thread event))
      (singlethreaded-connection 
       (setf (sconn.event-queue c) (nconc (sconn.event-queue c) (list event)))
       (setf (sconn.events-enqueued c) (mod (1+ (sconn.events-enqueued c))
                                            most-positive-fixnum))))))

(defun send-to-emacs (event)
  "Send EVENT to Emacs."
  ;;(log-event "send-to-emacs: ~a" event)
  (without-slime-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (send (mconn.control-thread c) event))
        (singlethreaded-connection
         (dispatch-event c event)))
      (maybe-slow-down))))
  

;;;;;; Flow control

;; After sending N (usually 100) messages we slow down and ping Emacs
;; to make sure that everything we have sent so far was received.

(defconstant send-counter-limit 100)

(defun maybe-slow-down ()
  (let ((counter (incf *send-counter*)))
    (when (< send-counter-limit counter)
      (setf *send-counter* 0)
      (ping-pong))))

(defun ping-pong ()
  (let* ((tag (make-tag))
         (pattern `(:emacs-pong ,tag)))
    (send-to-emacs `(:ping ,(current-thread-id) ,tag))
    (wait-for-event pattern)))


(defun wait-for-event (pattern &optional timeout)
  "Scan the event queue for PATTERN and return the event.
If TIMEOUT is 'nil wait until a matching event is enqued.
If TIMEOUT is 't only scan the queue without waiting.
The second return value is t if the timeout expired before a matching
event was found."
  (log-event "wait-for-event: ~s ~s~%" pattern timeout)
  (without-slime-interrupts
    (let ((c *emacs-connection*))
      (etypecase c
        (multithreaded-connection
         (receive-if (lambda (e) (event-match-p e pattern)) timeout))
        (singlethreaded-connection
         (wait-for-event/event-loop c pattern timeout))))))

(defun wait-for-event/event-loop (connection pattern timeout)
  (assert (or (not timeout) (eq timeout t)))
  (loop 
   (check-slime-interrupts)
   (let ((event (poll-for-event connection pattern)))
     (when event (return (car event))))
   (let ((events-enqueued (sconn.events-enqueued connection))
         (ready (wait-for-input (list (current-socket-io)) timeout)))
     (cond ((and timeout (not ready))
            (return (values nil t)))
           ((or (/= events-enqueued (sconn.events-enqueued connection))
                (eq ready :interrupt))
            ;; rescan event queue, interrupts may enqueue new events 
            )
           (t
            (assert (equal ready (list (current-socket-io))))
            (dispatch-event connection
                            (decode-message (current-socket-io))))))))

(defun poll-for-event (connection pattern)
  (let* ((c connection)
         (tail (member-if (lambda (e) (event-match-p e pattern))
                          (sconn.event-queue c))))
    (when tail 
      (setf (sconn.event-queue c) 
            (nconc (ldiff (sconn.event-queue c) tail) (cdr tail)))
      tail)))

;;; FIXME: Make this use SWANK-MATCH.
(defun event-match-p (event pattern)
  (cond ((or (keywordp pattern) (numberp pattern) (stringp pattern)
	     (member pattern '(nil t)))
	 (equal event pattern))
	((symbolp pattern) t)
	((consp pattern)
         (case (car pattern)
           ((or) (some (lambda (p) (event-match-p event p)) (cdr pattern)))
           (t (and (consp event)
                   (and (event-match-p (car event) (car pattern))
                        (event-match-p (cdr event) (cdr pattern)))))))
        (t (error "Invalid pattern: ~S" pattern))))



(defun spawn-threads-for-connection (connection)
  (setf (mconn.control-thread connection)
        (spawn (lambda () (control-thread connection))
               :name "control-thread"))
  connection)

(defun control-thread (connection)
  (with-struct* (mconn. @ connection)
    (setf (@ control-thread) (current-thread))
    (setf (@ reader-thread) (spawn (lambda () (read-loop connection)) 
                                   :name "reader-thread"))
    (setf (@ indentation-cache-thread)
          (spawn (lambda () (indentation-cache-loop connection))
                 :name "swank-indentation-cache-thread"))
    (dispatch-loop connection)))

(defun cleanup-connection-threads (connection)
  (let* ((c connection)
         (threads (list (mconn.repl-thread c)
                        (mconn.reader-thread c)
                        (mconn.control-thread c)
                        (mconn.auto-flush-thread c)
                        (mconn.indentation-cache-thread c))))
    (dolist (thread threads)
      (when (and thread
                 (thread-alive-p thread)
                 (not (equal (current-thread) thread)))
        (ignore-errors (kill-thread thread))))))

;;;;;; Signal driven IO

(defun install-sigio-handler (connection)
  (add-sigio-handler (connection.socket-io connection) 
                     (lambda () (process-io-interrupt connection)))
  (handle-requests connection t))

(defvar *io-interupt-level* 0)

(defun process-io-interrupt (connection)
  (log-event "process-io-interrupt ~d ...~%" *io-interupt-level*)
  (let ((*io-interupt-level* (1+ *io-interupt-level*)))
    (invoke-or-queue-interrupt
     (lambda () (handle-requests connection t))))
  (log-event "process-io-interrupt ~d ... done ~%" *io-interupt-level*))

(defun deinstall-sigio-handler (connection)
  (log-event "deinstall-sigio-handler...~%")
  (remove-sigio-handlers (connection.socket-io connection))
  (log-event "deinstall-sigio-handler...done~%"))

;;;;;; SERVE-EVENT based IO

(defun install-fd-handler (connection)
  (add-fd-handler (connection.socket-io connection)
                  (lambda () (handle-requests connection t)))
  (setf (sconn.saved-sigint-handler connection)
        (install-sigint-handler 
         (lambda () 
           (invoke-or-queue-interrupt
            (lambda () (dispatch-interrupt-event connection))))))
  (handle-requests connection t))

(defun dispatch-interrupt-event (connection)
  (with-connection (connection)
    (dispatch-event connection `(:emacs-interrupt ,(current-thread-id)))))

(defun deinstall-fd-handler (connection)
  (log-event "deinstall-fd-handler~%")
  (remove-fd-handlers (connection.socket-io connection))
  (install-sigint-handler (sconn.saved-sigint-handler connection)))

;;;;;; Simple sequential IO

(defun simple-serve-requests (connection)
  (unwind-protect
       (with-connection (connection)
         (call-with-user-break-handler
          (lambda ()
            (invoke-or-queue-interrupt
             (lambda () (dispatch-interrupt-event connection))))
          (lambda ()
            (with-simple-restart (close-connection "Close SLIME connection.")
              (let* ((stdin (real-input-stream *standard-input*))
                     (*standard-input* (make-repl-input-stream connection 
                                                               stdin)))
                (tagbody toplevel
                   (with-top-level-restart (connection (go toplevel))
                     (simple-repl))))))))
    (close-connection connection nil (safe-backtrace))))

;; this is signalled when our custom stream thinks the end-of-file is reached.
;; (not when the end-of-file on the socket is reached)
(define-condition end-of-repl-input (end-of-file) ())

(defun simple-repl ()
  (loop
    (format t "~a> " (package-string-for-prompt *package*))
    (force-output)
    (let ((form (handler-case (read)
                  (end-of-repl-input () (return)))))
      (let ((- form)
            (values (multiple-value-list (eval form))))
        (setq *** **  ** *  * (car values)
              /// //  // /  / values
              +++ ++  ++ +  + form)
        (cond ((null values) (format t "; No values~&"))
              (t (mapc (lambda (v) (format t "~s~&" v)) values)))))))

(defun make-repl-input-stream (connection stdin)
  (make-input-stream
   (lambda () (repl-input-stream-read connection stdin))))

(defun repl-input-stream-read (connection stdin)
  (loop
   (let* ((socket (connection.socket-io connection))
          (inputs (list socket stdin))
          (ready (wait-for-input inputs)))
     (cond ((eq ready :interrupt)
            (check-slime-interrupts))
           ((member socket ready)
            ;; A Slime request from Emacs is pending; make sure to
            ;; redirect IO to the REPL buffer.
            (with-simple-restart (process-input "Continue reading input.")
              (let ((*sldb-quit-restart* (find-restart 'process-input)))
                (with-io-redirection (connection)
                  (handle-requests connection t)))))
           ((member stdin ready)
            ;; User typed something into the  *inferior-lisp* buffer,
            ;; so do not redirect.
            (return (read-non-blocking stdin)))
           (t (assert (null ready)))))))

(defun read-non-blocking (stream)
  (with-output-to-string (str)
    (handler-case 
        (loop (let ((c (read-char-no-hang stream)))
                (unless c (return))
                (write-char c str)))
      (end-of-file () (error 'end-of-repl-input :stream stream)))))


;;; Channels

;; FIXME: should be per connection not global.
(defvar *channels* '())
(defvar *channel-counter* 0)

(defclass channel ()
  ((id :reader channel-id)
   (thread :initarg :thread :initform (current-thread) :reader channel-thread)
   (name :initarg :name :initform nil)))

(defmethod initialize-instance :after ((ch channel) &key)
  (with-slots (id) ch
    (setf id (incf *channel-counter*))
    (push (cons id ch) *channels*)))

(defmethod print-object ((c channel) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (id name) c
      (format stream "~d ~a" id name))))

(defun find-channel (id)
  (cdr (assoc id *channels*)))

(defgeneric channel-send (channel selector args))

(defmacro define-channel-method (selector (channel &rest args) &body body)
  `(defmethod channel-send (,channel (selector (eql ',selector)) args)
     (destructuring-bind ,args args
       . ,body)))

(defun send-to-remote-channel (channel-id msg)
  (send-to-emacs `(:channel-send ,channel-id ,msg)))



(defvar *slime-features* nil
  "The feature list that has been sent to Emacs.")

(defun send-oob-to-emacs (object)
  (send-to-emacs object))

;; FIXME: belongs to swank-repl.lisp
(defun force-user-output ()
  (force-output (connection.user-io *emacs-connection*)))

(add-hook *pre-reply-hook* 'force-user-output)

;; FIXME: belongs to swank-repl.lisp
(defun clear-user-input  ()
  (clear-input (connection.user-input *emacs-connection*)))

;; FIXME: not thread save.
(defvar *tag-counter* 0)

(defun make-tag () 
  (setq *tag-counter* (mod (1+ *tag-counter*) (expt 2 22))))

(defun y-or-n-p-in-emacs (format-string &rest arguments)
  "Like y-or-n-p, but ask in the Emacs minibuffer."
  (let ((tag (make-tag))
        (question (apply #'format nil format-string arguments)))
    (force-output)
    (send-to-emacs `(:y-or-n-p ,(current-thread-id) ,tag ,question))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defun read-from-minibuffer-in-emacs (prompt &optional initial-value)
  "Ask user a question in Emacs' minibuffer. Returns \"\" when user
entered nothing, returns NIL when user pressed C-g."
  (check-type prompt string) (check-type initial-value (or null string))
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:read-from-minibuffer ,(current-thread-id) ,tag
                                           ,prompt ,initial-value))
    (third (wait-for-event `(:emacs-return ,tag result)))))

(defstruct (unreadable-result
            (:constructor make-unreadable-result (string))
            (:copier nil)
            (:print-object
             (lambda (object stream)
               (print-unreadable-object (object stream :type t)
                 (princ (unreadable-result-string object) stream)))))
  string)

(defun symbol-name-for-emacs (symbol)
  (check-type symbol symbol)
  (let ((name (string-downcase (symbol-name symbol))))
    (if (keywordp symbol)
        (concatenate 'string ":" name)
        name)))

(defun process-form-for-emacs (form)
  "Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."
  (etypecase form
    (string (format nil "~S" form))
    (cons (format nil "(~A . ~A)"
                  (process-form-for-emacs (car form))
                  (process-form-for-emacs (cdr form))))
    (character (format nil "?~C" form))
    (symbol (symbol-name-for-emacs form))
    (number (let ((*print-base* 10))
              (princ-to-string form)))))

(defun wait-for-emacs-return (tag)
  (let ((event (caddr (wait-for-event `(:emacs-return ,tag result)))))
    (dcase event
      ((:unreadable value) (make-unreadable-result value))
      ((:ok value) value)
      ((:error kind . data) (error "~a: ~{~a~}" kind data))
      ((:abort) (abort))
      ;; only in reply to :ed-rpc{-no-wait} events.
      ((:ed-rpc-forbidden fn) (error "ED-RPC forbidden for ~a" fn)))))

(defun eval-in-emacs (form &optional nowait)
  "Eval FORM in Emacs.
`slime-enable-evaluate-in-emacs' should be set to T on the Emacs side."
  (cond (nowait
         (send-to-emacs `(:eval-no-wait ,(process-form-for-emacs form))))
        (t
         (force-output)
         (let ((tag (make-tag)))
           (send-to-emacs `(:eval ,(current-thread-id) ,tag
                                  ,(process-form-for-emacs form)))
           (wait-for-emacs-return tag)))))

(defun ed-rpc-no-wait (fn &rest args)
  "Invoke FN in Emacs (or some lesser editor) and don't wait for the result."
  (send-to-emacs `(:ed-rpc-no-wait ,(symbol-name-for-emacs fn) ,@args))
  (values))

(defun ed-rpc (fn &rest args)
  "Invoke FN in Emacs (or some lesser editor). FN should be defined in
Emacs Lisp via `defslimefun' or otherwise marked as RPCallable."
  (let ((tag (make-tag)))
    (send-to-emacs `(:ed-rpc ,(current-thread-id) ,tag
                             ,(symbol-name-for-emacs fn)
                             ,@args))
    (wait-for-emacs-return tag)))

(defvar *swank-wire-protocol-version* nil
  "The version of the swank/slime communication protocol.")

(defslimefun connection-info ()
  "Return a key-value list of the form: 
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"
  (let ((c *emacs-connection*))
    (setq *slime-features* *features*)
    `(:pid ,(getpid) :style ,(connection.communication-style c)
      :encoding (:coding-systems
                 ,(loop for cs in '("utf-8-unix" "iso-latin-1-unix")
                        when (find-external-format cs) collect cs))
      :lisp-implementation (:type ,(lisp-implementation-type)
                            :name ,(lisp-implementation-type-name)
                            :version ,(lisp-implementation-version)
                            :program ,(lisp-implementation-program))
      :machine (:instance ,(machine-instance)
               :type ,(machine-type)
               :version ,(machine-version))
      :features ,(features-for-emacs)
      :modules ,*modules*
      :package (:name ,(package-name *package*)
               :prompt ,(package-string-for-prompt *package*))
      :version ,*swank-wire-protocol-version*)))

(defun debug-on-swank-error ()
  (assert (eq *debug-on-swank-protocol-error* *debug-swank-backend*))
  *debug-on-swank-protocol-error*)

(defun (setf debug-on-swank-error) (new-value)
  (setf *debug-on-swank-protocol-error* new-value)
  (setf *debug-swank-backend* new-value))

(defslimefun toggle-debug-on-swank-error ()
  (setf (debug-on-swank-error) (not (debug-on-swank-error))))


;;;; Reading and printing

(define-special *buffer-package*     
    "Package corresponding to slime-buffer-package.  

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(define-special *buffer-readtable*
    "Readtable associated with the current buffer")

(defmacro with-buffer-syntax ((&optional package) &body body)
  "Execute BODY with appropriate *package* and *readtable* bindings.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  `(call-with-buffer-syntax ,package (lambda () ,@body)))

(defun call-with-buffer-syntax (package fun)
  (let ((*package* (if package
                       (guess-buffer-package package)
                       *buffer-package*)))
    ;; Don't shadow *readtable* unnecessarily because that prevents
    ;; the user from assigning to it.
    (if (eq *readtable* *buffer-readtable*)
        (call-with-syntax-hooks fun)
        (let ((*readtable* *buffer-readtable*))
          (call-with-syntax-hooks fun)))))

(defmacro without-printing-errors ((&key object stream
                                        (msg "<<error printing object>>"))
                                  &body body)
  "Catches errors during evaluation of BODY and prints MSG instead."
  `(handler-case (progn ,@body) 
     (serious-condition ()
       ,(cond ((and stream object)
               (let ((gstream (gensym "STREAM+")))
                 `(let ((,gstream ,stream))
                    (print-unreadable-object (,object ,gstream :type t 
                                                      :identity t)
                      (write-string ,msg ,gstream)))))
              (stream
               `(write-string ,msg ,stream))
              (object
               `(with-output-to-string (s)
                  (print-unreadable-object (,object s :type t :identity t)
                    (write-string ,msg  s))))
              (t msg)))))

(defun to-string (object)
  "Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (without-printing-errors (:object object :stream nil)
        (prin1-to-string object)))))

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (with-buffer-syntax ()
    (let ((*read-suppress* nil))
      (values (read-from-string string)))))

(defun parse-string (string package)
  "Read STRING in PACKAGE."
  (with-buffer-syntax (package)
    (let ((*read-suppress* nil))
      (read-from-string string))))

;; FIXME: deal with #\| etc.  hard to do portably.
(defun tokenize-symbol (string)
  "STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"
  (let ((package (let ((pos (position #\: string)))
                   (if pos (subseq string 0 pos) nil)))
        (symbol (let ((pos (position #\: string :from-end t)))
                  (if pos (subseq string (1+ pos)) string)))
        (internp (not (= (count #\: string) 1))))
    (values symbol package internp)))

(defun tokenize-symbol-thoroughly (string)
  "This version of TOKENIZE-SYMBOL handles escape characters."
  (let ((package nil)
        (token (make-array (length string) :element-type 'character
                           :fill-pointer 0))
        (backslash nil)
        (vertical nil)
        (internp nil))
    (loop for char across string do
          (cond
            (backslash
             (vector-push-extend char token)
             (setq backslash nil))
            ((char= char #\\) ; Quotes next character, even within |...|
             (setq backslash t))
            ((char= char #\|)
             (setq vertical (not vertical)))
            (vertical
             (vector-push-extend char token))
            ((char= char #\:)
             (cond ((and package internp)
                    (return-from tokenize-symbol-thoroughly))
                   (package
                    (setq internp t))
                   (t
                    (setq package token
                          token (make-array (length string)
                                            :element-type 'character
                                            :fill-pointer 0)))))
            (t
             (vector-push-extend (casify-char char) token))))
    (unless vertical
          (values token package (or (not package) internp)))))

(defun untokenize-symbol (package-name internal-p symbol-name)
  "The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"
  (cond ((not package-name) 	symbol-name)
        (internal-p 		(cat package-name "::" symbol-name))
        (t 			(cat package-name ":" symbol-name))))

(defun casify-char (char)
  "Convert CHAR accoring to readtable-case."
  (ecase (readtable-case *readtable*)
    (:preserve char)
    (:upcase   (char-upcase char))
    (:downcase (char-downcase char))
    (:invert (if (upper-case-p char)
                 (char-downcase char)
                 (char-upcase char)))))


(defun find-symbol-with-status (symbol-name status 
                                &optional (package *package*))
  (multiple-value-bind (symbol flag) (find-symbol symbol-name package)
    (if (and flag (eq flag status))
        (values symbol flag)
        (values nil nil))))

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."
  (multiple-value-bind (sname pname internalp)
      (tokenize-symbol-thoroughly string)
    (when sname
     (let ((package (cond ((string= pname "") keyword-package)
                          (pname              (find-package pname))
                          (t                  package))))
       (if package
           (multiple-value-bind (symbol flag)
               (if internalp
                   (find-symbol sname package)
                   (find-symbol-with-status sname ':external package))
             (values symbol flag sname package))
           (values nil nil nil nil))))))

(defun parse-symbol-or-lose (string &optional (package *package*))
  (multiple-value-bind (symbol status) (parse-symbol string package)
    (if status
        (values symbol status)
        (error "Unknown symbol: ~A [in ~A]" string package))))

(defun parse-package (string)
  "Find the package named STRING.
Return the package or nil."
  ;; STRING comes usually from a (in-package STRING) form.
  (ignore-errors
    (find-package (let ((*package* *swank-io-package*))
                    (read-from-string string)))))

(defun unparse-name (string)
  "Print the name STRING according to the current printer settings."
  ;; this is intended for package or symbol names
  (subseq (prin1-to-string (make-symbol string)) 2))

(defun guess-package (string)
  "Guess which package corresponds to STRING.
Return nil if no package matches."
  (when string
    (or (find-package string)
        (parse-package string)
        (if (find #\! string)           ; for SBCL
            (guess-package (substitute #\- #\! string))))))

(defvar *readtable-alist* (default-readtable-alist)
  "An alist mapping package names to readtables.")

(defun guess-buffer-readtable (package-name)
  (let ((package (guess-package package-name)))
    (or (and package 
             (cdr (assoc (package-name package) *readtable-alist* 
                         :test #'string=)))
        *readtable*)))


;;;; Evaluation

(defvar *pending-continuations* '()
  "List of continuations for Emacs. (thread local)")

(defun guess-buffer-package (string)
  "Return a package for STRING. 
Fall back to the current if no such package exists."
  (or (and string (guess-package string))
      *package*))

(defun eval-for-emacs (form buffer-package id)
  "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.
Errors are trapped and invoke our debugger."
  (let (ok result condition)
    (unwind-protect
         (let ((*buffer-package* (guess-buffer-package buffer-package))
               (*buffer-readtable* (guess-buffer-readtable buffer-package))
               (*pending-continuations* (cons id *pending-continuations*)))
           (check-type *buffer-package* package)
           (check-type *buffer-readtable* readtable)
           ;; APPLY would be cleaner than EVAL. 
           ;; (setq result (apply (car form) (cdr form)))
           (handler-bind ((t (lambda (c) (setf condition c))))
             (setq result (with-slime-interrupts (eval form))))
           (run-hook *pre-reply-hook*)
           (setq ok t))
      (send-to-emacs `(:return ,(current-thread)
                               ,(if ok
                                    `(:ok ,result)
                                    `(:abort ,(prin1-to-string condition)))
                               ,id)))))

(defvar *echo-area-prefix* "=> "
  "A prefix that `format-values-for-echo-area' should use.")

(defun format-values-for-echo-area (values)
  (with-buffer-syntax ()
    (let ((*print-readably* nil))
      (cond ((null values) "; No value")
            ((and (integerp (car values)) (null (cdr values)))
             (let ((i (car values)))
               (format nil "~A~D (~a bit~:p, #x~X, #o~O, #b~B)" 
                       *echo-area-prefix*
                       i (integer-length i) i i i)))
            ((and (typep (car values) 'ratio)
                  (null (cdr values))
                  (ignore-errors
                   ;; The ratio may be to large to be represented as a single float
                   (format nil "~A~D (~:*~f)" 
                           *echo-area-prefix*
                           (car values)))))
            (t (format nil "~a~{~S~^, ~}" *echo-area-prefix* values))))))

(defmacro values-to-string (values)
  `(format-values-for-echo-area (multiple-value-list ,values)))

(defslimefun interactive-eval (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
      (let ((values (multiple-value-list (eval (from-string string)))))
        (finish-output)
        (format-values-for-echo-area values)))))

(defslimefun eval-and-grab-output (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (let* ((s (make-string-output-stream))
             (*standard-output* s)
             (values (multiple-value-list (eval (from-string string)))))
        (list (get-output-stream-string s) 
              (format nil "~{~S~^~%~}" values))))))

(defun eval-region (string)
  "Evaluate STRING.
Return the results of the last form as a list and as secondary value the 
last form."
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
         (when (eq form stream)
           (finish-output)
           (return (values values -)))
         (setq - form)
         (setq values (multiple-value-list (eval form)))
         (finish-output))))))

(defslimefun interactive-eval-region (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME interactive evaluation request.")
      (format-values-for-echo-area (eval-region string)))))

(defslimefun re-evaluate-defvar (form)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME evaluation request.")
      (let ((form (read-from-string form)))
        (destructuring-bind (dv name &optional value doc) form
          (declare (ignore value doc))
          (assert (eq dv 'defvar))
          (makunbound name)
          (prin1-to-string (eval form)))))))

(defvar *swank-pprint-bindings*
  `((*print-pretty*   . t) 
    (*print-level*    . nil)
    (*print-length*   . nil)
    (*print-circle*   . t)
    (*print-gensym*   . t)
    (*print-readably* . nil))
  "A list of variables bindings during pretty printing.
Used by pprint-eval.")

(defun swank-pprint (values)
  "Bind some printer variables and pretty print each object in VALUES."
  (with-buffer-syntax ()
    (with-bindings *swank-pprint-bindings*
      (cond ((null values) "; No value")
            (t (with-output-to-string (*standard-output*)
                 (dolist (o values)
                   (pprint o)
                   (terpri))))))))
  
(defslimefun pprint-eval (string)
  (with-buffer-syntax ()
    (let* ((s (make-string-output-stream))
           (values 
            (let ((*standard-output* s)
                  (*trace-output* s))
              (multiple-value-list (eval (read-from-string string))))))
      (cat (get-output-stream-string s)
           (swank-pprint values)))))

(defslimefun set-package (name)
  "Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."
  (let ((p (guess-package name)))
    (assert (packagep p) nil "Package ~a doesn't exist." name)
    (setq *package* p)
    (list (package-name p) (package-string-for-prompt p))))

(defun cat (&rest strings)
  "Concatenate all arguments and make the result a string."
  (with-output-to-string (out)
    (dolist (s strings)
      (etypecase s
        (string (write-string s out))
        (character (write-char s out))))))

(defun truncate-string (string width &optional ellipsis)
  (let ((len (length string)))
    (cond ((< len width) string)
          (ellipsis (cat (subseq string 0 width) ellipsis))
          (t (subseq string 0 width)))))

(defun call/truncated-output-to-string (length function 
                                        &optional (ellipsis ".."))
  "Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."
  (let ((buffer (make-string (+ length (length ellipsis))))
        (fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
               (let* ((free (- length fill-pointer))
                      (count (min free (length string))))
                 (replace buffer string :start1 fill-pointer :end2 count)
                 (incf fill-pointer count)
                 (when (> (length string) free)
                   (replace buffer ellipsis :start1 fill-pointer)
                   (return-from buffer-full buffer)))))
        (let ((stream (make-output-stream #'write-output)))
          (funcall function stream)
          (finish-output stream)
          (subseq buffer 0 fill-pointer))))))

(defmacro with-string-stream ((var &key length bindings)
                              &body body)
  (cond ((and (not bindings) (not length))
         `(with-output-to-string (,var) . ,body))
        ((not bindings)
         `(call/truncated-output-to-string 
           ,length (lambda (,var) . ,body)))
        (t
         `(with-bindings ,bindings 
            (with-string-stream (,var :length ,length)
              . ,body)))))

(defun to-line (object &optional width)
  "Print OBJECT to a single line. Return the string."
  (let ((width (or width 512)))
    (without-printing-errors (:object object :stream nil)
      (with-string-stream (stream :length width)
        (write object :stream stream :right-margin width :lines 1)))))

(defun escape-string (string stream &key length (map '((#\" . "\\\"")
                                                       (#\\ . "\\\\"))))
  "Write STRING to STREAM surronded by double-quotes.
LENGTH -- if non-nil truncate output after LENGTH chars.
MAP -- rewrite the chars in STRING according to this alist."
  (let ((limit (or length array-dimension-limit)))
    (write-char #\" stream)
    (loop for c across string 
          for i from 0 do
          (when (= i limit)
            (write-string "..." stream)
            (return))
          (let ((probe (assoc c map)))
            (cond (probe (write-string (cdr probe) stream))
                  (t (write-char c stream)))))
    (write-char #\" stream)))


;;;; Prompt 

;; FIXME: do we really need 45 lines of code just to figure out the
;; prompt?

(defvar *canonical-package-nicknames*
  `((:common-lisp-user . :cl-user))
  "Canonical package names to use instead of shortest name/nickname.")

(defvar *auto-abbreviate-dotted-packages* t
  "Abbreviate dotted package names to their last component if T.")

(defun package-string-for-prompt (package)
  "Return the shortest nickname (or canonical name) of PACKAGE."
  (unparse-name
   (or (canonical-package-nickname package)
       (auto-abbreviated-package-name package)
       (shortest-package-nickname package))))

(defun canonical-package-nickname (package)
  "Return the canonical package nickname, if any, of PACKAGE."
  (let ((name (cdr (assoc (package-name package) *canonical-package-nicknames* 
                          :test #'string=))))
    (and name (string name))))

(defun auto-abbreviated-package-name (package)
  "Return an abbreviated 'name' for PACKAGE. 

N.B. this is not an actual package name or nickname."
  (when *auto-abbreviate-dotted-packages*
    (loop with package-name = (package-name package)
          with offset = nil
          do (let ((last-dot-pos (position #\. package-name :end offset 
                                           :from-end t)))
               (unless last-dot-pos
                 (return nil))
               ;; If a dot chunk contains only numbers, that chunk most
               ;; likely represents a version number; so we collect the
               ;; next chunks, too, until we find one with meat.
               (let ((name (subseq package-name (1+ last-dot-pos) offset)))
                 (if (notevery #'digit-char-p name)
                     (return (subseq package-name (1+ last-dot-pos)))
                     (setq offset last-dot-pos)))))))

(defun shortest-package-nickname (package)
  "Return the shortest nickname of PACKAGE."
  (loop for name in (cons (package-name package) (package-nicknames package))
        for shortest = name then (if (< (length name) (length shortest))
                                   name
                                   shortest)
              finally (return shortest)))



(defslimefun ed-in-emacs (&optional what)
  "Edit WHAT in Emacs.

WHAT can be:
  A pathname or a string,
  A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
  A function name (symbol or cons),
  NIL. "
  (flet ((canonicalize-filename (filename)
           (pathname-to-filename (or (probe-file filename) filename))))
    (let ((target 
           (etypecase what
             (null nil)
             ((or string pathname) 
              `(:filename ,(canonicalize-filename what)))
             ((cons (or string pathname) *)
              `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
             ((or symbol cons)
              `(:function-name ,(prin1-to-string what))))))
      (cond (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
            ((default-connection)
             (with-connection ((default-connection))
               (send-oob-to-emacs `(:ed ,target))))
            (t (error "No connection"))))))

(defslimefun inspect-in-emacs (what &key wait)
  "Inspect WHAT in Emacs. If WAIT is true (default NIL) blocks until the
inspector has been closed in Emacs."
  (flet ((send-it ()
           (let ((tag (when wait (make-tag)))
                 (thread (when wait (current-thread-id))))
             (with-buffer-syntax ()
               (reset-inspector)
               (send-oob-to-emacs `(:inspect ,(inspect-object what)
                                             ,thread
                                             ,tag)))
             (when wait
               (wait-for-event `(:emacs-return ,tag result))))))
    (cond
      (*emacs-connection*
       (send-it))
      ((default-connection)
       (with-connection ((default-connection))
         (send-it))))
    what))

(defslimefun value-for-editing (form)
  "Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."
  ;; FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
  (with-buffer-syntax ()
    (let* ((value (eval (read-from-string form)))
           (*print-length* nil))
      (prin1-to-string value))))

(defslimefun commit-edited-value (form value)
  "Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."
  (with-buffer-syntax ()
    (eval `(setf ,(read-from-string form) 
            ,(read-from-string (concatenate 'string "`" value))))
    t))

(defun background-message  (format-string &rest args)
  "Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped if we are too busy with other things."
  (when *emacs-connection*
    (send-to-emacs `(:background-message
                     ,(apply #'format nil format-string args)))))

;; This is only used by the test suite.
(defun sleep-for (seconds)
  "Sleep for at least SECONDS seconds.
This is just like cl:sleep but guarantees to sleep
at least SECONDS."
  (let* ((start (get-internal-real-time))
         (end (+ start
                 (* seconds internal-time-units-per-second))))
    (loop
     (let ((now (get-internal-real-time)))
       (cond ((< end now) (return))
             (t (sleep (/ (- end now)
                          internal-time-units-per-second))))))))


;;;; Debugger

(defun invoke-slime-debugger (condition)
  "Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (without-slime-interrupts
    (cond (*emacs-connection*
           (debug-in-emacs condition))
          ((default-connection)
           (with-connection ((default-connection))
             (debug-in-emacs condition))))))

(define-condition invoke-default-debugger () ())

(defun swank-debugger-hook (condition hook)
  "Debugger function for binding *DEBUGGER-HOOK*."
  (declare (ignore hook))
  (handler-case
      (call-with-debugger-hook #'swank-debugger-hook
                               (lambda () (invoke-slime-debugger condition)))
    (invoke-default-debugger ()
      (invoke-default-debugger condition))))

(defun invoke-default-debugger (condition)
  (call-with-debugger-hook nil (lambda () (invoke-debugger condition))))
  
(defvar *global-debugger* t
  "Non-nil means the Swank debugger hook will be installed globally.")

(add-hook *new-connection-hook* 'install-debugger)
(defun install-debugger (connection)
  (declare (ignore connection))
  (when *global-debugger*
    (install-debugger-globally #'swank-debugger-hook)))

;;;;; Debugger loop
;;;
;;; These variables are dynamically bound during debugging.
;;;
(defvar *swank-debugger-condition* nil
  "The condition being debugged.")

(defvar *sldb-level* 0
  "The current level of recursive debugging.")

(defvar *sldb-initial-frames* 20
  "The initial number of backtrace frames to send to Emacs.")

(defvar *sldb-restarts* nil
  "The list of currenlty active restarts.")

(defvar *sldb-stepping-p* nil
  "True during execution of a step command.")

(defun debug-in-emacs (condition)
  (let ((*swank-debugger-condition* condition)
        (*sldb-restarts* (compute-restarts condition))
        (*sldb-quit-restart* (and *sldb-quit-restart*
                                  (find-restart *sldb-quit-restart*)))
        (*package* (or (and (boundp '*buffer-package*)
                            (symbol-value '*buffer-package*))
                       *package*))
        (*sldb-level* (1+ *sldb-level*))
        (*sldb-stepping-p* nil))
    (force-user-output)
    (call-with-debugging-environment
     (lambda ()
       (sldb-loop *sldb-level*)))))

(defun sldb-loop (level)
  (unwind-protect
       (loop
        (with-simple-restart (abort "Return to sldb level ~D." level)
          (send-to-emacs
           (list* :debug (current-thread-id) level
                  (debugger-info-for-emacs 0 *sldb-initial-frames*)))
          (send-to-emacs 
           (list :debug-activate (current-thread-id) level nil))
          (loop 
           (handler-case 
               (dcase (wait-for-event 
                                  `(or (:emacs-rex . _)
                                       (:sldb-return ,(1+ level))))
                 ((:emacs-rex &rest args) (apply #'eval-for-emacs args))
                 ((:sldb-return _) (declare (ignore _)) (return nil)))
             (sldb-condition (c) 
               (handle-sldb-condition c))))))
    (send-to-emacs `(:debug-return
                     ,(current-thread-id) ,level ,*sldb-stepping-p*))
    (wait-for-event `(:sldb-return ,(1+ level)) t) ; clean event-queue
    (when (> level 1)
      (send-event (current-thread) `(:sldb-return ,level)))))

(defun handle-sldb-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(current-thread-id)
                                      ,(princ-to-string real-condition)))))

(defun %%condition-message (condition)
  (let ((limit (ash 1 16)))
    (with-string-stream (stream :length limit)
      (handler-case
          (let ((*print-readably* nil)
                (*print-pretty* t)
                (*print-right-margin* 65)
                (*print-circle* t)
                (*print-length* (or *print-length* limit))
                (*print-level* (or *print-level* limit))
                (*print-lines* (or *print-lines* limit)))
            (print-condition condition stream))
        (serious-condition (c)
          (ignore-errors
            (with-standard-io-syntax
              (let ((*print-readably* nil))
                (format stream "~&Error (~a) during printing: " (type-of c))
                (print-unreadable-object (condition stream :type t
                                                    :identity t))))))))))

(defun %condition-message (condition)
  (string-trim #(#\newline #\space #\tab)
               (%%condition-message condition)))

(defvar *sldb-condition-printer* #'%condition-message
  "Function called to print a condition to an SLDB buffer.")

(defun safe-condition-message (condition)
  "Print condition to a string, handling any errors during printing."
  (funcall *sldb-condition-printer* condition))

(defun debugger-condition-for-emacs ()
  (list (safe-condition-message *swank-debugger-condition*)
        (format nil "   [Condition of type ~S]"
                (type-of *swank-debugger-condition*))
        (condition-extras *swank-debugger-condition*)))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."
  (let ((*print-right-margin* most-positive-fixnum))
    (loop for restart in *sldb-restarts* collect 
          (list (format nil "~:[~;*~]~a" 
                        (eq restart *sldb-quit-restart*)
                        (restart-name restart))
                (with-output-to-string (stream)
                  (without-printing-errors (:object restart
                                            :stream stream
                                            :msg "<<error printing restart>>")
                    (princ restart stream)))))))

;;;;; SLDB entry points

(defslimefun sldb-break-with-default-debugger (dont-unwind)
  "Invoke the default debugger."
  (cond (dont-unwind 
         (invoke-default-debugger *swank-debugger-condition*))
        (t
         (signal 'invoke-default-debugger))))

(defslimefun backtrace (start end)
  "Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."
  (loop for frame in (compute-backtrace start end)
        for i from start collect 
        (list* i (frame-to-string frame)
               (ecase (frame-restartable-p frame)
                 ((nil) nil)
                 ((t) `((:restartable t)))))))

(defun frame-to-string (frame)
  (with-string-stream (stream :length (* (or *print-lines* 1) 
                                         (or *print-right-margin* 100))
                              :bindings *backtrace-printer-bindings*)
    (handler-case (print-frame frame stream)
      (serious-condition ()
        (format stream "[error printing frame]")))))

(defslimefun debugger-info-for-emacs (start end)
  "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description [plist])
  extra       ::= (:references and other random things)
  cont        ::= continutation
  plist       ::= (:restartable {nil | t | :unknown})

condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continutation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Slime toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
  (4))"
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)
        *pending-continuations*))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (when restart
      (invoke-restart-interactively restart))))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defslimefun sldb-continue ()
  (continue))

(defun coerce-to-condition (datum args)
  (etypecase datum
    (string (make-condition 'simple-error :format-control datum 
                            :format-arguments args))
    (symbol (apply #'make-condition datum args))))

(defslimefun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (invoke-slime-debugger (coerce-to-condition datum args))))

;; FIXME: (last (compute-restarts)) looks dubious.
(defslimefun throw-to-toplevel ()
  "Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."
  (let ((restart (or (and *sldb-quit-restart* 
                          (find-restart *sldb-quit-restart*))
                     (car (last (compute-restarts))))))
    (cond (restart (invoke-restart restart))
          (t (format nil "Restart not active [~s]" *sldb-quit-restart*)))))

(defslimefun invoke-nth-restart-for-emacs (sldb-level n)
  "Invoke the Nth available restart.
SLDB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."
  (when (= sldb-level *sldb-level*)
    (invoke-nth-restart n)))

(defun wrap-sldb-vars (form)
  `(let ((*sldb-level* ,*sldb-level*))
     ,form))

(defun eval-in-frame-aux (frame string package print)
  (let* ((form (wrap-sldb-vars (parse-string string package)))
         (values (multiple-value-list (eval-in-frame form frame))))
    (with-buffer-syntax (package)
      (funcall print values))))

(defslimefun eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'format-values-for-echo-area))

(defslimefun pprint-eval-string-in-frame (string frame package)
  (eval-in-frame-aux frame string package #'swank-pprint))

(defslimefun frame-package-name (frame)
  (let ((pkg (frame-package frame)))
    (cond (pkg (package-name pkg))
          (t (with-buffer-syntax () (package-name *package*))))))

(defslimefun frame-locals-and-catch-tags (index)
  "Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
LOCALS is a list of the form ((&key NAME ID VALUE) ...).
TAGS has is a list of strings."
  (list (frame-locals-for-emacs index)
        (mapcar #'to-string (frame-catch-tags index))))

(defun frame-locals-for-emacs (index)
  (with-bindings *backtrace-printer-bindings*
    (loop for var in (frame-locals index) collect
          (destructuring-bind (&key name id value) var
            (list :name (let ((*package* (or (frame-package index) *package*)))
                          (prin1-to-string name))
                  :id id
                  :value (to-line value *print-right-margin*))))))

(defslimefun sldb-disassemble (index)
  (with-output-to-string (*standard-output*)
    (disassemble-frame index)))

(defslimefun sldb-return-from-frame (index string)
  (let ((form (from-string string)))
    (to-string (multiple-value-list (return-from-frame index form)))))

(defslimefun sldb-break (name)
  (with-buffer-syntax ()
    (sldb-break-at-start (read-from-string name))))

(defmacro define-stepper-function (name backend-function-name)
  `(defslimefun ,name (frame)
     (cond ((sldb-stepper-condition-p *swank-debugger-condition*)
            (setq *sldb-stepping-p* t)
            (,backend-function-name))
           ((find-restart 'continue)
            (activate-stepping frame)
            (setq *sldb-stepping-p* t)
            (continue))
           (t
            (error "Not currently single-stepping, ~
and no continue restart available.")))))

(define-stepper-function sldb-step sldb-step-into)
(define-stepper-function sldb-next sldb-step-next)
(define-stepper-function sldb-out  sldb-step-out)

(defslimefun toggle-break-on-signals ()
  (setq *break-on-signals* (not *break-on-signals*))
  (format nil "*break-on-signals* = ~a" *break-on-signals*))

(defslimefun sdlb-print-condition ()
  (princ-to-string *swank-debugger-condition*))


;;;; Compilation Commands.

(defstruct (compilation-result (:type list))
  (type :compilation-result)
  notes
  (successp nil :type boolean)
  (duration 0.0 :type float)
  (loadp nil :type boolean)
  (faslfile nil :type (or null string)))

(defun measure-time-interval (fun)
  "Call FUN and return the first return value and the elapsed time.
The time is measured in seconds."
  (declare (type function fun))
  (let ((before (get-internal-real-time)))
    (values
     (funcall fun)
     (/ (- (get-internal-real-time) before)
        (coerce internal-time-units-per-second 'float)))))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list* :message (message condition)
         :severity (severity condition)
         :location (location condition)
         :references (references condition)
         (let ((s (source-context condition)))
           (if s (list :source-context s)))))

(defun collect-notes (function)
  (let ((notes '()))
    (multiple-value-bind (result seconds)
        (handler-bind ((compiler-condition
                        (lambda (c) (push (make-compiler-note c) notes))))
          (measure-time-interval
           (lambda ()
             ;; To report location of error-signaling toplevel forms
             ;; for errors in EVAL-WHEN or during macroexpansion.
             (restart-case (multiple-value-list (funcall function))
               (abort () :report "Abort compilation." (list nil))))))
      (destructuring-bind (successp &optional loadp faslfile) result
        (let ((faslfile (etypecase faslfile
                          (null nil)
                          (pathname (pathname-to-filename faslfile)))))
          (make-compilation-result :notes (reverse notes) 
                                   :duration seconds
                                   :successp (if successp t)
                                   :loadp (if loadp t)
                                   :faslfile faslfile))))))

(defun swank-compile-file* (pathname load-p &rest options &key policy
                                                      &allow-other-keys)
  (multiple-value-bind (output-pathname warnings? failure?)
      (swank-compile-file pathname
                          (fasl-pathname pathname options)
                          nil
                          (or (guess-external-format pathname)
                              :default)
                          :policy policy)
    (declare (ignore warnings?))
    (values t (not failure?) load-p output-pathname)))

(defvar *compile-file-for-emacs-hook* '(swank-compile-file*))

(defslimefun compile-file-for-emacs (filename load-p &rest options)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (with-buffer-syntax ()
    (collect-notes
     (lambda ()
       (let ((pathname (filename-to-pathname filename))
             (*compile-print* nil)
             (*compile-verbose* t))
         (loop for hook in *compile-file-for-emacs-hook*
               do
               (multiple-value-bind (tried success load? output-pathname)
                   (apply hook pathname load-p options)
                 (when tried
                   (return (values success load? output-pathname))))))))))

;; FIXME: now that *compile-file-for-emacs-hook* is there this is
;; redundant and confusing.
(defvar *fasl-pathname-function* nil
  "In non-nil, use this function to compute the name for fasl-files.")

(defun pathname-as-directory (pathname)
  (append (pathname-directory pathname)
          (when (pathname-name pathname)
            (list (file-namestring pathname)))))

(defun compile-file-output (file directory)
  (make-pathname :directory (pathname-as-directory directory)
                 :defaults (compile-file-pathname file)))

(defun fasl-pathname (input-file options)
  (cond (*fasl-pathname-function*
         (funcall *fasl-pathname-function* input-file options))
        ((getf options :fasl-directory)
         (let ((dir (getf options :fasl-directory)))
           (assert (char= (aref dir (1- (length dir))) #\/))
           (compile-file-output input-file dir)))
        (t
         (compile-file-pathname input-file))))

(defslimefun compile-string-for-emacs (string buffer position filename policy)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (let* ((offset (cadr (assoc :position position)))
         (line-column (cdr (assoc :line position)))
         (line (first line-column))
         (column (second line-column)))
    (with-buffer-syntax ()
      (collect-notes
       (lambda () 
         (let ((*compile-print* t) (*compile-verbose* nil))
           (swank-compile-string string
                                 :buffer buffer
                                 :position offset 
                                 :filename filename
                                 :line line
                                 :column column
                                 :policy policy)))))))

(defslimefun compile-multiple-strings-for-emacs (strings policy)
  "Compile STRINGS (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (loop for (string buffer package position filename) in strings collect
        (collect-notes
         (lambda ()
           (with-buffer-syntax (package)
             (let ((*compile-print* t) (*compile-verbose* nil))
               (swank-compile-string string
                                     :buffer buffer
                                     :position position 
                                     :filename filename
                                     :policy policy)))))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun requires-compile-p (source-file)
  (let ((fasl-file (probe-file (compile-file-pathname source-file))))
    (or (not fasl-file)
        (file-newer-p source-file fasl-file))))

(defslimefun compile-file-if-needed (filename loadp)
  (let ((pathname (filename-to-pathname filename)))
    (cond ((requires-compile-p pathname)
           (compile-file-for-emacs pathname loadp))
          (t
           (collect-notes
            (lambda ()
              (or (not loadp)
                  (load (compile-file-pathname pathname)))))))))


;;;; Loading

(defslimefun load-file (filename)
  (to-string (load (filename-to-pathname filename))))


;;;;; swank-require

(defslimefun swank-require (modules &optional filename)
  "Load the module MODULE."
  (dolist (module (ensure-list modules))
    (unless (member (string module) *modules* :test #'string=)
      (require module (if filename
                          (filename-to-pathname filename)
                          (module-filename module)))
      (assert (member (string module) *modules* :test #'string=)
              () "Required module ~s was not provided" module)))
  *modules*)

(defvar *find-module* 'find-module
  "Pluggable function to locate modules.
The function receives a module name as argument and should return
the filename of the module (or nil if the file doesn't exist).")

(defun module-filename (module)
  "Return the filename for the module MODULE."
  (or (funcall *find-module* module)
      (error "Can't locate module: ~s" module)))

;;;;;; Simple *find-module* function.

(defun merged-directory (dirname defaults)
  (pathname-directory
   (merge-pathnames
    (make-pathname :directory `(:relative ,dirname) :defaults defaults)
    defaults)))

(defvar *load-path* '()
  "A list of directories to search for modules.")

(defun module-candidates (name dir)
  (list (compile-file-pathname (make-pathname :name name :defaults dir))
        (make-pathname :name name :type "lisp" :defaults dir)))

(defun find-module (module)
  (let ((name (string-downcase module)))
    (some (lambda (dir) (some #'probe-file (module-candidates name dir)))
          *load-path*)))


;;;; Macroexpansion

(defvar *macroexpand-printer-bindings*
  '((*print-circle* . nil)
    (*print-pretty* . t)
    (*print-escape* . t)
    (*print-lines* . nil)
    (*print-level* . nil)
    (*print-length* . nil)))

(defun apply-macro-expander (expander string)
  (with-buffer-syntax ()
    (with-bindings *macroexpand-printer-bindings*
      (prin1-to-string (funcall expander (from-string string))))))

(defslimefun swank-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslimefun swank-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslimefun swank-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))

(defslimefun swank-compiler-macroexpand-1 (string)
  (apply-macro-expander #'compiler-macroexpand-1 string))

(defslimefun swank-compiler-macroexpand (string)
  (apply-macro-expander #'compiler-macroexpand string))

(defslimefun swank-expand-1 (string)
  (apply-macro-expander #'expand-1 string))

(defslimefun swank-expand (string)
  (apply-macro-expander #'expand string))

(defun expand-1 (form)
  (multiple-value-bind (expansion expanded?) (macroexpand-1 form)
    (if expanded?
        (values expansion t)
        (compiler-macroexpand-1 form))))

(defun expand (form)
  (expand-repeatedly #'expand-1 form))

(defun expand-repeatedly (expander form)
  (loop
    (multiple-value-bind (expansion expanded?) (funcall expander form)
      (unless expanded? (return expansion))
      (setq form expansion))))

(defslimefun swank-format-string-expand (string)
  (apply-macro-expander #'format-string-expand string))

(defslimefun disassemble-form (form)
  (with-buffer-syntax ()
    (with-output-to-string (*standard-output*)
      (let ((*print-readably* nil))
        (disassemble (eval (read-from-string form)))))))


;;;; Simple completion

(defslimefun simple-completions (prefix package)
  "Return a list of completions for the string PREFIX."
  (let ((strings (all-completions prefix package)))
    (list strings (longest-common-prefix strings))))

(defun all-completions (prefix package)
  (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
    (let* ((extern (and pname (not intern)))
	   (pkg (cond ((equal pname "") keyword-package)
                      ((not pname) (guess-buffer-package package))
                      (t (guess-package pname))))
	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
	   (syms (and pkg (matching-symbols pkg extern test)))
           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (format-completion-set strings intern pname))))

(defun matching-symbols (package external test)
  (let ((test (if external 
		  (lambda (s)
		    (and (symbol-external-p s package) 
			 (funcall test s)))
		  test))
	(result '()))
    (do-symbols (s package)
      (when (funcall test s) 
	(push s result)))
    (remove-duplicates result)))

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*) 
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix))
                 :test #'char-equal)))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string) (untokenize-symbol package-name internal-p string))
          (sort strings #'string<)))


;;;; Simple arglist display

(defslimefun operator-arglist (name package)
  (ignore-errors
    (let ((args (arglist (parse-symbol name (guess-buffer-package package)))))
      (cond ((eq args :not-available) nil)
	    (t (princ-to-string (cons name args)))))))


;;;; Documentation

(defslimefun apropos-list-for-emacs  (name &optional external-only 
                                           case-sensitive package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (parse-package package)
                         (error "No such package: ~S" package)))))
    ;; The MAPCAN will filter all uninteresting symbols, i.e. those
    ;; who cannot be meaningfully described.
    (mapcan (listify #'briefly-describe-symbol-for-emacs)
            (sort (remove-duplicates
                   (apropos-symbols name external-only case-sensitive package))
                  #'present-symbol-before-p))))

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string)
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (let ((desc (map-if #'stringp #'first-line 
                        (describe-symbol-for-emacs symbol))))
      (if desc 
          (list* :designator (to-string symbol) desc)))))

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun present-symbol-before-p (x y)
  "Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (declare (type symbol x y))
  (flet ((accessible (s)
           ;; Test breaks on NIL for package that does not inherit it
           (eq (find-symbol (symbol-name s) *buffer-package*) s)))
    (let ((ax (accessible x)) (ay (accessible y)))
      (cond ((and ax ay) (string< (symbol-name x) (symbol-name y)))
            (ax t)
            (ay nil)
            (t (let ((px (symbol-package x)) (py (symbol-package y)))
                 (if (eq px py)
                     (string< (symbol-name x) (symbol-name y))
                     (string< (package-name px) (package-name py)))))))))

(defun make-apropos-matcher (pattern case-sensitive)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (symbol)
      (search pattern (string symbol) :test chr=))))

(defun apropos-symbols (string external-only case-sensitive package)
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher  (make-apropos-matcher string case-sensitive))
        (result))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (cond ((not morep) (return))
                    ((and (if external-only (symbol-external-p symbol) t)
                          (funcall matcher symbol))
                     (push symbol result))))))
    result))

(defun call-with-describe-settings (fn)
  (let ((*print-readably* nil))
    (funcall fn)))

(defmacro with-describe-settings ((&rest _) &body body)
  (declare (ignore _))
  `(call-with-describe-settings (lambda () ,@body)))
    
(defun describe-to-string (object)
  (with-describe-settings ()
    (with-output-to-string (*standard-output*)
      (describe object))))

(defslimefun describe-symbol (symbol-name)
  (with-buffer-syntax ()
    (describe-to-string (parse-symbol-or-lose symbol-name))))

(defslimefun describe-function (name)
  (with-buffer-syntax ()
    (let ((symbol (parse-symbol-or-lose name)))
      (describe-to-string (or (macro-function symbol)
                              (symbol-function symbol))))))

(defslimefun describe-definition-for-emacs (name kind)
  (with-buffer-syntax ()
    (with-describe-settings ()
      (with-output-to-string (*standard-output*)
        (describe-definition (parse-symbol-or-lose name) kind)))))

(defslimefun documentation-symbol (symbol-name)
  (with-buffer-syntax ()
    (multiple-value-bind (sym foundp) (parse-symbol symbol-name)
      (if foundp
          (let ((vdoc (documentation sym 'variable))
                (fdoc (documentation sym 'function)))
            (with-output-to-string (string)
              (format string "Documentation for the symbol ~a:~2%" sym)
              (unless (or vdoc fdoc)
                (format string "Not documented." ))
              (when vdoc
                (format string "Variable:~% ~a~2%" vdoc))
              (when fdoc
                (format string "Function:~% Arglist: ~a~2% ~a"
                        (arglist sym)
                        fdoc))))
          (format nil "No such symbol, ~a." symbol-name)))))


;;;; Package Commands

(defslimefun list-all-package-names (&optional nicknames)
  "Return a list of all package names.
Include the nicknames if NICKNAMES is true."
  (mapcar #'unparse-name
          (if nicknames
              (mapcan #'package-names (list-all-packages))
              (mapcar #'package-name  (list-all-packages)))))


;;;; Tracing

;; Use eval for the sake of portability... 
(defun tracedp (fspec)
  (member fspec (eval '(trace))))

(defvar *after-toggle-trace-hook* nil
  "Hook called whenever a SPEC is traced or untraced.

If non-nil, called with two arguments SPEC and TRACED-P." )
(defslimefun swank-toggle-trace (spec-string)
  (let* ((spec (from-string spec-string))
         (retval (cond ((consp spec) ; handle complicated cases in the backend
                        (toggle-trace spec))
                       ((tracedp spec)
                        (eval `(untrace ,spec))
                        (format nil "~S is now untraced." spec))
                       (t
                        (eval `(trace ,spec))
                        (format nil "~S is now traced." spec))))
         (traced-p (let* ((tosearch "is now traced.")
                          (start (- (length retval)
                                    (length tosearch)))
                          (end (+ start (length tosearch))))
                     (search tosearch (subseq retval start end))))
         (hook-msg (when *after-toggle-trace-hook*
                     (funcall *after-toggle-trace-hook*
                              spec
                              traced-p))))
    (if hook-msg
        (format nil "~a~%(also ~a)" retval hook-msg)
        retval)))

(defslimefun untrace-all ()
  (untrace))


;;;; Undefing

(defslimefun undefine-function (fname-string)
  (let ((fname (from-string fname-string)))
    (format nil "~S" (fmakunbound fname))))

(defslimefun unintern-symbol (name package)
  (let ((pkg (guess-package package)))
    (cond ((not pkg) (format nil "No such package: ~s" package))
          (t
           (multiple-value-bind (sym found) (parse-symbol name pkg)
             (case found
               ((nil) (format nil "~s not in package ~s" name package))
               (t
                (unintern sym pkg)
                (format nil "Uninterned symbol: ~s" sym))))))))

(defslimefun swank-delete-package (package-name)
  (let ((pkg (or (guess-package package-name)
                 (error "No such package: ~s" package-name))))
    (delete-package pkg)
    nil))


;;;; Profiling

(defun profiledp (fspec)
  (member fspec (profiled-functions)))

(defslimefun toggle-profile-fdefinition (fname-string)
  (let ((fname (from-string fname-string)))
    (cond ((profiledp fname)
	   (unprofile fname)
	   (format nil "~S is now unprofiled." fname))
	  (t
           (profile fname)
	   (format nil "~S is now profiled." fname)))))

(defslimefun profile-by-substring (substring package)
  (let ((count 0))
    (flet ((maybe-profile (symbol)
             (when (and (fboundp symbol)
                        (not (profiledp symbol))
                        (search substring (symbol-name symbol) :test #'equalp))
               (handler-case (progn
                               (profile symbol)
                               (incf count))
                 (error (condition)
                   (warn "~a" condition))))))
      (if package
          (do-symbols (symbol (parse-package package))
            (maybe-profile symbol))
          (do-all-symbols (symbol)
            (maybe-profile symbol))))
    (format nil "~a function~:p ~:*~[are~;is~:;are~] now profiled" count)))

(defslimefun swank-profile-package (package-name callersp methodsp)
  (let ((pkg (or (guess-package package-name)
                 (error "Not a valid package name: ~s" package-name))))
    (check-type callersp boolean)
    (check-type methodsp boolean)
    (profile-package pkg callersp methodsp)))


;;;; Source Locations

(defslimefun find-definition-for-thing (thing)
  (find-source-location thing))

(defslimefun find-source-location-for-emacs (spec)
  (find-source-location (value-spec-ref spec)))

(defun value-spec-ref (spec)
  (dcase spec
    ((:string string package)
     (with-buffer-syntax (package)
       (eval (read-from-string string))))
    ((:inspector part) 
     (inspector-nth-part part))
    ((:sldb frame var)
     (frame-var-value frame var))))

(defvar *find-definitions-right-trim* ",:.>")
(defvar *find-definitions-left-trim* "#:<")

(defun find-definitions-find-symbol-or-package (name)
  (flet ((do-find (name)
           (multiple-value-bind (symbol found name)
               (with-buffer-syntax ()
                 (parse-symbol name))
             (cond (found
                    (return-from find-definitions-find-symbol-or-package
                      (values symbol found)))
                   ;; Packages are not named by symbols, so
                   ;; not-interned symbols can refer to packages
                   ((find-package name)
                    (return-from find-definitions-find-symbol-or-package
                      (values (make-symbol name) t)))))))
    (do-find name)
    (do-find (string-right-trim *find-definitions-right-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim* name))
    (do-find (string-left-trim *find-definitions-left-trim*
                               (string-right-trim
                                *find-definitions-right-trim* name)))
    ;; Not exactly robust
    (when (and (eql (search "(setf " name :test #'char-equal) 0)
               (char= (char name (1- (length name))) #\)))
      (multiple-value-bind (symbol found)
          (with-buffer-syntax ()
            (parse-symbol (subseq name (length "(setf ")
                                  (1- (length name)))))
        (when found
          (values `(setf ,symbol) t))))))

(defslimefun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (multiple-value-bind (symbol found)
      (find-definitions-find-symbol-or-package name)
    (when found
      (mapcar #'xref>elisp (find-definitions symbol)))))

;;; Generic function so contribs can extend it.
(defgeneric xref-doit (type thing)
  (:method (type thing)
    (declare (ignore type thing))
    :not-implemented))

(macrolet ((define-xref-action (xref-type handler)
             `(defmethod xref-doit ((type (eql ,xref-type)) thing)
                (declare (ignorable type))
                (funcall ,handler thing))))
  (define-xref-action :calls        #'who-calls)
  (define-xref-action :calls-who    #'calls-who)
  (define-xref-action :references   #'who-references)
  (define-xref-action :binds        #'who-binds)
  (define-xref-action :sets         #'who-sets)
  (define-xref-action :macroexpands #'who-macroexpands)
  (define-xref-action :specializes  #'who-specializes)
  (define-xref-action :callers      #'list-callers)
  (define-xref-action :callees      #'list-callees))

(defslimefun xref (type name)
  (multiple-value-bind (sexp error) (ignore-errors (from-string name))
    (unless error
      (let ((xrefs  (xref-doit type sexp)))
        (if (eq xrefs :not-implemented)
            :not-implemented
            (mapcar #'xref>elisp xrefs))))))

(defslimefun xrefs (types name)
  (loop for type in types
        for xrefs = (xref type name)
        when (and (not (eq :not-implemented xrefs))
                  (not (null xrefs)))
          collect (cons type xrefs)))

(defun xref>elisp (xref)
  (destructuring-bind (name loc) xref
    (list (to-string name) loc)))


;;;;; Lazy lists

(defstruct (lcons (:constructor %lcons (car %cdr))
                  (:predicate lcons?))
  car
  (%cdr nil :type (or null lcons function))
  (forced? nil))

(defmacro lcons (car cdr)
  `(%lcons ,car (lambda () ,cdr)))

(defmacro lcons* (car cdr &rest more)
  (cond ((null more) `(lcons ,car ,cdr))
        (t `(lcons ,car (lcons* ,cdr ,@more)))))

(defun lcons-cdr (lcons)
  (with-struct* (lcons- @ lcons)
    (cond ((@ forced?)
           (@ %cdr))
          (t
           (let ((value (funcall (@ %cdr))))
             (setf (@ forced?) t
                   (@ %cdr) value))))))

(defun llist-range (llist start end)
  (llist-take (llist-skip llist start) (- end start)))

(defun llist-skip (lcons index)
  (do ((i 0 (1+ i))
       (l lcons (lcons-cdr l)))
      ((or (= i index) (null l))
       l)))

(defun llist-take (lcons count)
  (let ((result '()))
    (do ((i 0 (1+ i))
         (l lcons (lcons-cdr l)))
        ((or (= i count)
             (null l)))
      (push (lcons-car l) result))
    (nreverse result)))

(defun iline (label value)
  `(:line ,label ,value))


;;;; Inspecting

(defvar *inspector-verbose* nil)

(defvar *inspector-printer-bindings*
  '((*print-lines*        . 1) 
    (*print-right-margin* . 75)
    (*print-pretty*       . t)
    (*print-readably*     . nil)))

(defvar *inspector-verbose-printer-bindings*
  '((*print-escape* . t)
    (*print-circle* . t)
    (*print-array*  . nil)))

(defstruct inspector-state)
(defstruct (istate (:conc-name istate.) (:include inspector-state))
  object
  (verbose *inspector-verbose*)
  (parts (make-array 10 :adjustable t :fill-pointer 0))
  (actions (make-array 10 :adjustable t :fill-pointer 0))
  metadata-plist
  content
  next previous)

(defvar *istate* nil)
(defvar *inspector-history*)

(defun reset-inspector ()
  (setq *istate* nil
        *inspector-history* (make-array 10 :adjustable t :fill-pointer 0)))
  
(defslimefun init-inspector (string)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME inspection request.")
      (reset-inspector)
      (inspect-object (eval (read-from-string string))))))

(defun ensure-istate-metadata (o indicator default)
  (with-struct (istate. object metadata-plist) *istate*
    (assert (eq object o))
    (let ((data (getf metadata-plist indicator default)))
      (setf (getf metadata-plist indicator) data)
      data)))

(defun inspect-object (o)
  (let* ((prev *istate*)
         (istate (make-istate :object o :previous prev
                              :verbose (cond (prev (istate.verbose prev))
                                             (t *inspector-verbose*)))))
    (setq *istate* istate)
    (setf (istate.content istate) (emacs-inspect/istate istate))
    (unless (find o *inspector-history*)
      (vector-push-extend o *inspector-history*))
    (let ((previous (istate.previous istate)))
      (if previous (setf (istate.next previous) istate)))
    (istate>elisp istate)))

(defun emacs-inspect/istate (istate)
  (with-bindings (if (istate.verbose istate)
                     *inspector-verbose-printer-bindings*
                     *inspector-printer-bindings*)
    (emacs-inspect (istate.object istate))))

(defun istate>elisp (istate)
  (list :title (prepare-title istate)
        :id (assign-index (istate.object istate) (istate.parts istate))
        :content (prepare-range istate 0 500)))

(defun prepare-title (istate)
  (if (istate.verbose istate)
      (with-bindings *inspector-verbose-printer-bindings*
        (to-string (istate.object istate)))
      (with-string-stream (stream :length 200
                                  :bindings *inspector-printer-bindings*)
        (print-unreadable-object
            ((istate.object istate) stream :type t :identity t)))))

(defun prepare-range (istate start end)
  (let* ((range (content-range (istate.content istate) start end))
         (ps (loop for part in range append (prepare-part part istate))))
    (list ps 
          (if (< (length ps) (- end start))
              (+ start (length ps))
              (+ end 1000))
          start end)))

(defun prepare-part (part istate)
  (let ((newline '#.(string #\newline)))
    (etypecase part
      (string (list part))
      (cons (dcase part
              ((:newline) (list newline))
              ((:value obj &optional str) 
               (list (value-part obj str (istate.parts istate))))
              ((:label &rest strs)
               (list (list :label (apply #'cat (mapcar #'string strs)))))
              ((:action label lambda &key (refreshp t)) 
               (list (action-part label lambda refreshp
                                  (istate.actions istate))))
              ((:line label value)
               (list (princ-to-string label) ": "
                     (value-part value nil (istate.parts istate))
                     newline)))))))

(defun value-part (object string parts)
  (list :value 
        (or string (print-part-to-string object))
        (assign-index object parts)))

(defun action-part (label lambda refreshp actions)
  (list :action label (assign-index (list lambda refreshp) actions)))

(defun assign-index (object vector)
  (let ((index (fill-pointer vector)))
    (vector-push-extend object vector)
    index))

(defun print-part-to-string (value)
  (let* ((*print-readably* nil)
         (string (to-line value))
         (pos (position value *inspector-history*)))
    (if pos
        (format nil "@~D=~A" pos string)
        string)))

(defun content-range (list start end)
  (typecase list
    (list (let ((len (length list)))
            (subseq list start (min len end))))
    (lcons (llist-range list start end))))

(defslimefun inspector-nth-part (index)
  "Return the current inspector's INDEXth part.
The second value indicates if that part exists at all."
  (let* ((parts (istate.parts *istate*))
         (foundp (< index (length parts))))
    (values (and foundp (aref parts index))
            foundp)))

(defslimefun inspect-nth-part (index)
  (with-buffer-syntax ()
    (inspect-object (inspector-nth-part index))))

(defslimefun inspector-range (from to)
  (prepare-range *istate* from to))

(defslimefun inspector-call-nth-action (index &rest args)
  (destructuring-bind (fun refreshp) (aref (istate.actions *istate*) index)
    (apply fun args)
    (if refreshp
        (inspector-reinspect)
        ;; tell emacs that we don't want to refresh the inspector buffer
        nil)))

(defslimefun inspector-pop ()
  "Inspect the previous object.
Return nil if there's no previous object."
  (with-buffer-syntax ()
    (cond ((istate.previous *istate*)
           (setq *istate* (istate.previous *istate*))
           (istate>elisp *istate*))
          (t nil))))

(defslimefun inspector-next ()
  "Inspect the next element in the history of inspected objects.."
  (with-buffer-syntax ()
    (cond ((istate.next *istate*)
           (setq *istate* (istate.next *istate*))
           (istate>elisp *istate*))
          (t nil))))

(defslimefun inspector-reinspect ()
  (let ((istate *istate*))
    (setf (istate.content istate) (emacs-inspect/istate istate))
    (istate>elisp istate)))

(defslimefun inspector-toggle-verbose ()
  "Toggle verbosity of inspected object."
  (setf (istate.verbose *istate*) (not (istate.verbose *istate*)))
  (istate>elisp *istate*))

(defslimefun inspector-eval (string)
  (let* ((obj (istate.object *istate*))
         (context (eval-context obj))
         (form (with-buffer-syntax ((cdr (assoc '*package* context)))
                 (read-from-string string)))
         (ignorable (remove-if #'boundp (mapcar #'car context))))
    (to-string (eval `(let ((* ',obj) (- ',form)
                            . ,(loop for (var . val) in context 
                                     unless (constantp var) collect 
                                     `(,var ',val)))
                        (declare (ignorable . ,ignorable))
                        ,form)))))

(defslimefun inspector-history ()
  (with-output-to-string (out)
    (let ((newest (loop for s = *istate* then next
                        for next = (istate.next s)
                        if (not next) return s)))
      (format out "--- next/prev chain ---")
      (loop for s = newest then (istate.previous s) while s do
            (let ((val (istate.object s)))
              (format out "~%~:[  ~; *~]@~d " 
                      (eq s *istate*)
                      (position val *inspector-history*))
              (print-unreadable-object (val out :type t :identity t)))))
    (format out "~%~%--- all visited objects ---")
    (loop for val across *inspector-history* for i from 0 do
          (format out "~%~2,' d " i)
          (print-unreadable-object (val out :type t :identity t)))))

(defslimefun quit-inspector ()
  (reset-inspector)
  nil)

(defslimefun describe-inspectee ()
  "Describe the currently inspected object."
  (with-buffer-syntax ()
    (describe-to-string (istate.object *istate*))))

(defslimefun pprint-inspector-part (index)
  "Pretty-print the currently inspected object."
  (with-buffer-syntax ()
    (swank-pprint (list (inspector-nth-part index)))))

(defslimefun inspect-in-frame (string index)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME inspection request.")
      (reset-inspector)
      (inspect-object (eval-in-frame (from-string string) index)))))

(defslimefun inspect-current-condition ()
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object *swank-debugger-condition*)))

(defslimefun inspect-frame-var (frame var)
  (with-buffer-syntax ()
    (reset-inspector)
    (inspect-object (frame-var-value frame var))))

;;;;; Lists

(defmethod emacs-inspect ((o cons))
  (if (listp (cdr o))
      (inspect-list o)
      (inspect-cons o)))

(defun inspect-cons (cons)
  (label-value-line* 
   ('car (car cons))
   ('cdr (cdr cons))))

(defun inspect-list (list)
  (multiple-value-bind (length tail) (safe-length list)
    (flet ((frob (title list)
             (list* title '(:newline) (inspect-list-aux list))))
      (cond ((not length)
             (frob "A circular list:"
                   (cons (car list)
                         (ldiff (cdr list) list))))
            ((not tail)
             (frob "A proper list:" list))
            (t
             (frob "An improper list:" list))))))

(defun inspect-list-aux (list)
  (loop for i from 0  for rest on list  while (consp rest)  append 
        (if (listp (cdr rest))
            (label-value-line i (car rest))
            (label-value-line* (i (car rest)) (:tail (cdr rest))))))

(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Return NIL if LIST is circular."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return nil))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;;;;; Hashtables

(defun hash-table-to-alist (ht)
  (let ((result '()))
    (maphash (lambda (key value) 
               (setq result (acons key value result)))
             ht)
    result))

(defmethod emacs-inspect ((ht hash-table))
  (append
   (label-value-line*
    ("Count" (hash-table-count ht))
    ("Size" (hash-table-size ht))
    ("Test" (hash-table-test ht))
    ("Rehash size" (hash-table-rehash-size ht))
    ("Rehash threshold" (hash-table-rehash-threshold ht)))
   (let ((weakness (hash-table-weakness ht)))
     (when weakness
       (label-value-line "Weakness:" weakness)))
   (unless (zerop (hash-table-count ht))
     `((:action "[clear hashtable]" 
                ,(lambda () (clrhash ht))) (:newline)
       "Contents: " (:newline)))
   (let ((content (hash-table-to-alist ht)))
     (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
            (setf content (sort content 'string< :key #'first)))
           ((every (lambda (x) (typep (first x) 'real)) content)
            (setf content (sort content '< :key #'first))))
     (loop for (key . value) in content appending
           `((:value ,key) " = " (:value ,value)
             " " (:action "[remove entry]"
                          ,(let ((key key))
                                (lambda () (remhash key ht))))
             (:newline))))))

;;;;; Arrays

(defmethod emacs-inspect ((array array))
  (lcons*
   (iline "Dimensions" (array-dimensions array))
   (iline "Element type" (array-element-type array))
   (iline "Total size" (array-total-size array))
   (iline "Adjustable" (adjustable-array-p array))
   (iline "Fill pointer" (if (array-has-fill-pointer-p array)
                             (fill-pointer array)))
   "Contents:" '(:newline)
   (labels ((k (i max)
              (cond ((= i max) '())
                    (t (lcons (iline i (row-major-aref array i))
                              (k (1+ i) max))))))
     (k 0 (array-total-size array)))))

;;;;; Chars

(defmethod emacs-inspect ((char character))
  (append 
   (label-value-line*
    ("Char code" (char-code char))
    ("Lower cased" (char-downcase char))
    ("Upper cased" (char-upcase char)))
   (if (get-macro-character char)
       `("In the current readtable (" 
         (:value ,*readtable*) ") it is a macro character: "
         (:value ,(get-macro-character char))))))

;;;; Thread listing

(defvar *thread-list* ()
  "List of threads displayed in Emacs.  We don't care a about
synchronization issues (yet).  There can only be one thread listing at
a time.")

(defslimefun list-threads ()
  "Return a list (LABELS (ID NAME STATUS ATTRS ...) ...).
LABELS is a list of attribute names and the remaining lists are the
corresponding attribute values per thread.  
Example: 
  ((:id :name :status :priority)
   (6 \"swank-indentation-cache-thread\" \"Semaphore timed wait\" 0)
   (5 \"reader-thread\" \"Active\" 0)
   (4 \"control-thread\" \"Semaphore timed wait\" 0)
   (2 \"Swank Sentinel\" \"Semaphore timed wait\" 0)
   (1 \"listener\" \"Active\" 0)
   (0 \"Initial\" \"Sleep\" 0))"
  (setq *thread-list* (all-threads))
  (when (and *emacs-connection*
             (use-threads-p)
             (equalp (thread-name (current-thread)) "worker"))
    (setf *thread-list* (delete (current-thread) *thread-list*)))
  (let* ((plist (thread-attributes (car *thread-list*)))
         (labels (loop for (key) on plist by #'cddr 
                       collect key)))
    `((:id :name :status ,@labels)
      ,@(loop for thread in *thread-list*
              for name = (thread-name thread)
              for attributes = (thread-attributes thread)
              collect (list* (thread-id thread)
                             (string name)
                             (thread-status thread)
                             (loop for label in labels
                                   collect (getf attributes label)))))))

(defslimefun quit-thread-browser ()
  (setq *thread-list* nil))

(defun nth-thread (index)
  (nth index *thread-list*))

(defslimefun debug-nth-thread (index)
  (let ((connection *emacs-connection*))
    (queue-thread-interrupt
     (nth-thread index)
     (lambda ()
       (with-connection (connection)
         (simple-break))))))

(defslimefun kill-nth-thread (index)
  (kill-thread (nth-thread index)))

(defslimefun start-swank-server-in-thread (index port-file-name)
  "Interrupt the INDEXth thread and make it start a swank server.
The server port is written to PORT-FILE-NAME."
  (interrupt-thread (nth-thread index)
                    (lambda ()
                      (start-server port-file-name :style nil))))

;;;; Class browser

(defun mop-helper (class-name fn)
  (let ((class (find-class class-name nil)))
    (if class
        (mapcar (lambda (x) (to-string (class-name x)))
                (funcall fn class)))))

(defslimefun mop (type symbol-name)
  "Return info about classes using mop.

    When type is:
     :subclasses - return the list of subclasses of class.
     :superclasses - return the list of superclasses of class."
  (let ((symbol (parse-symbol symbol-name *buffer-package*)))
    (ecase type
      (:subclasses
       (mop-helper symbol #'swank-mop:class-direct-subclasses))
      (:superclasses 
       (mop-helper symbol #'swank-mop:class-direct-superclasses)))))


;;;; Automatically synchronized state
;;;
;;; Here we add hooks to push updates of relevant information to
;;; Emacs.

;;;;; *FEATURES*

(defun sync-features-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  ;; FIXME: *slime-features* should be connection-local
  (unless (eq *slime-features* *features*)
    (setq *slime-features* *features*)
    (send-to-emacs (list :new-features (features-for-emacs)))))

(defun features-for-emacs ()
  "Return `*slime-features*' in a format suitable to send it to Emacs."
  *slime-features*)

(add-hook *pre-reply-hook* 'sync-features-to-emacs)


;;;;; Indentation of macros
;;;
;;; This code decides how macros should be indented (based on their
;;; arglists) and tells Emacs. A per-connection cache is used to avoid
;;; sending redundant information to Emacs -- we just say what's
;;; changed since last time.
;;;
;;; The strategy is to scan all symbols, pick out the macros, and look
;;; for &body-arguments.

(defvar *configure-emacs-indentation* t
  "When true, automatically send indentation information to Emacs
after each command.")

(defslimefun update-indentation-information ()
  (send-to-indentation-cache `(:update-indentation-information))
  nil)

;; This function is for *PRE-REPLY-HOOK*.
(defun sync-indentation-to-emacs ()
  "Send any indentation updates to Emacs via CONNECTION."
  (when *configure-emacs-indentation*
    (send-to-indentation-cache `(:sync-indentation ,*buffer-package*))))

;; Send REQUEST to the cache.  If we are single threaded perform the
;; request right away, otherwise delegate the request to the
;; indentation-cache-thread.
(defun send-to-indentation-cache (request)
  (let ((c *emacs-connection*))
    (etypecase c
      (singlethreaded-connection 
       (handle-indentation-cache-request c request))
      (multithreaded-connection
       (without-slime-interrupts
         (send (mconn.indentation-cache-thread c) request))))))

(defun indentation-cache-loop (connection)
  (with-connection (connection)
    (loop
      (restart-case
          (handle-indentation-cache-request connection (receive))
        (abort ()
          :report "Return to the indentation cache request handling loop.")))))

(defun handle-indentation-cache-request (connection request)
  (dcase request
    ((:sync-indentation package)
     (let ((fullp (need-full-indentation-update-p connection)))
       (perform-indentation-update connection fullp package)))
    ((:update-indentation-information)
     (perform-indentation-update connection t nil))))

(defun need-full-indentation-update-p (connection)
  "Return true if the whole indentation cache should be updated.
This is a heuristic to avoid scanning all symbols all the time:
instead, we only do a full scan if the set of packages has changed."
  (set-difference (list-all-packages)
                  (connection.indentation-cache-packages connection)))

(defun perform-indentation-update (connection force package)
  "Update the indentation cache in CONNECTION and update Emacs.
If FORCE is true then start again without considering the old cache."
  (let ((cache (connection.indentation-cache connection)))
    (when force (clrhash cache))
    (let ((delta (update-indentation/delta-for-emacs cache force package)))
      (setf (connection.indentation-cache-packages connection)
            (list-all-packages))
      (unless (null delta)
        (setf (connection.indentation-cache connection) cache)
        (send-to-emacs (list :indentation-update delta))))))

(defun update-indentation/delta-for-emacs (cache force package)
  "Update the cache and return the changes in a (SYMBOL INDENT PACKAGES) list.
If FORCE is true then check all symbols, otherwise only check symbols
belonging to PACKAGE."
  (let ((alist '()))
    (flet ((consider (symbol)
             (let ((indent (symbol-indentation symbol)))
               (when indent
                 (unless (equal (gethash symbol cache) indent)
                   (setf (gethash symbol cache) indent)
                   (let ((pkgs (mapcar #'package-name 
                                       (symbol-packages symbol)))
                         (name (string-downcase symbol)))
                     (push (list name indent pkgs) alist)))))))
      (cond (force
             (do-all-symbols (symbol)
               (consider symbol)))
            ((package-name package) ; don't try to iterate over a
                                    ; deleted package.
             (do-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (consider symbol)))))
      alist)))

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))

(defun symbol-packages (symbol)
  "Return the  packages where SYMBOL can be found."
  (let ((string (string symbol)))
    (loop for p in (list-all-packages)
          when (eq symbol (find-symbol string p))
          collect p)))

(defun cl-symbol-p (symbol)
  "Is SYMBOL a symbol in the COMMON-LISP package?"
  (eq (symbol-package symbol) cl-package))

(defun known-to-emacs-p (symbol)
  "Return true if Emacs has special rules for indenting SYMBOL."
  (cl-symbol-p symbol))

(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL.
The form is to be used as the `common-lisp-indent-function' property
in Emacs."
  (if (and (macro-function symbol)
           (not (known-to-emacs-p symbol)))
      (let ((arglist (arglist symbol)))
        (etypecase arglist
          ((member :not-available)
           nil)
          (list
           (macro-indentation arglist))))
      nil))

(defun macro-indentation (arglist)
  (if (well-formed-list-p arglist)
      (position '&body (remove '&optional (clean-arglist arglist)))
      nil))

(defun clean-arglist (arglist)
  "Remove &whole, &enviroment, and &aux elements from ARGLIST."
  (cond ((null arglist) '())
        ((member (car arglist) '(&whole &environment))
         (clean-arglist (cddr arglist)))
        ((eq (car arglist) '&aux)
         '())
        (t (cons (car arglist) (clean-arglist (cdr arglist))))))

(defun well-formed-list-p (list)
  "Is LIST a proper list terminated by NIL?"
  (typecase list
    (null t)
    (cons (well-formed-list-p (cdr list)))
    (t    nil)))

(defun print-indentation-lossage (&optional (stream *standard-output*))
  "Return the list of symbols whose indentation styles collide incompatibly.
Collisions are caused because package information is ignored."
  (let ((table (make-hash-table :test 'equal)))
    (flet ((name (s) (string-downcase (symbol-name s))))
      (do-all-symbols (s)
        (setf (gethash (name s) table)
              (cons s (symbol-indentation s))))
      (let ((collisions '()))
        (do-all-symbols (s)
          (let* ((entry (gethash (name s) table))
                 (owner (car entry))
                 (indent (cdr entry)))
            (unless (or (eq s owner)
                        (equal (symbol-indentation s) indent)
                        (and (not (fboundp s))
                             (null (macro-function s))))
              (pushnew owner collisions)
              (pushnew s collisions))))
        (if (null collisions)
            (format stream "~&No worries!~%")
            (format stream "~&Symbols with collisions:~%~{  ~S~%~}"
                    collisions))))))

;;; FIXME: it's too slow on CLASP right now, remove once it's fast enough.
#-clasp
(add-hook *pre-reply-hook* 'sync-indentation-to-emacs)

(defun make-output-function-for-target (connection target)
  "Create a function to send user output to a specific TARGET in Emacs."
  (lambda (string)
    (swank::with-connection (connection)
      (with-simple-restart
          (abort "Abort sending output to Emacs.")
        (swank::send-to-emacs `(:write-string ,string ,target))))))

(defun make-output-stream-for-target (connection target)
  "Create a stream that sends output to a specific TARGET in Emacs."
  (make-output-stream (make-output-function-for-target connection target)))


;;;; Testing 

(defslimefun io-speed-test (&optional (n 1000) (m 1))
  (let* ((s *standard-output*)
         (*trace-output* (make-broadcast-stream s *log-output*)))
    (time (progn
            (dotimes (i n)
              (format s "~D abcdefghijklm~%" i)
              (when (zerop (mod n m))
                (finish-output s)))
            (finish-output s)
            (when *emacs-connection*
              (eval-in-emacs '(message "done.")))))
    (terpri *trace-output*)
    (finish-output *trace-output*)
    nil))

(defslimefun flow-control-test (n delay)
  (let ((stream (make-output-stream 
                 (let ((conn *emacs-connection*))
                   (lambda (string)
                     (declare (ignore string))
                     (with-connection (conn)
                       (send-to-emacs `(:test-delay ,delay))))))))
    (dotimes (i n)
      (print i stream)
      (force-output stream)
      (background-message "flow-control-test: ~d" i))))


(defun before-init (version load-path)
  (pushnew :swank *features*)
  (setq *swank-wire-protocol-version* version)
  (setq *load-path* load-path))

(defun init ()
  (run-hook *after-init-hook*))

;; Local Variables:
;; coding: latin-1-unix
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;;*"
;; End:

;;; swank.lisp ends here
