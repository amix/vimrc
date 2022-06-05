(defpackage swank/backend
  (:use cl)
  (:nicknames swank-backend)
  (:export *debug-swank-backend*
           *log-output*
           sldb-condition
           compiler-condition
           original-condition
           message
           source-context
           condition
           severity
           with-compilation-hooks
           make-location
           location
           location-p
           location-buffer
           location-position
           location-hints
           position-p
           position-pos
           print-output-to-string
           quit-lisp
           references
           unbound-slot-filler
           declaration-arglist
           type-specifier-arglist
           with-struct
           when-let
           defimplementation
           converting-errors-to-error-location
           make-error-location
           deinit-log-output
           ;; interrupt macro for the backend
           *pending-slime-interrupts*
           check-slime-interrupts
           *interrupt-queued-handler*
           ;; inspector related symbols
           emacs-inspect
           label-value-line
           label-value-line*
           boolean-to-feature-expression
           with-symbol
           choose-symbol
           ;; package helper for backend
           import-to-swank-mop
           import-swank-mop-symbols
           ;;
           default-directory
           set-default-directory
           frame-source-location
           restart-frame
           gdb-initial-commands
           sldb-break-on-return
           buffer-first-change

           profiled-functions
           unprofile-all
           profile-report
           profile-reset
           profile-package

           with-collected-macro-forms
           auto-flush-loop
           *auto-flush-interval*))

(defpackage swank/rpc
  (:use :cl)
  (:export
   read-message
   read-packet
   swank-reader-error
   swank-reader-error.packet
   swank-reader-error.cause
   write-message))

(defpackage swank/match
  (:use cl)
  (:export match))

;; FIXME: rename to sawnk/mop
(defpackage swank-mop
  (:use)
  (:export
   ;; classes
   standard-generic-function
   standard-slot-definition
   standard-method
   standard-class
   eql-specializer
   eql-specializer-object
   ;; standard-class readers
   class-default-initargs
   class-direct-default-initargs
   class-direct-slots
   class-direct-subclasses
   class-direct-superclasses
   class-finalized-p
   class-name
   class-precedence-list
   class-prototype
   class-slots
   specializer-direct-methods
   ;; generic function readers
   generic-function-argument-precedence-order
   generic-function-declarations
   generic-function-lambda-list
   generic-function-methods
   generic-function-method-class
   generic-function-method-combination
   generic-function-name
   ;; method readers
   method-generic-function
   method-function
   method-lambda-list
   method-specializers
   method-qualifiers
   ;; slot readers
   slot-definition-allocation
   slot-definition-documentation
   slot-definition-initargs
   slot-definition-initform
   slot-definition-initfunction
   slot-definition-name
   slot-definition-type
   slot-definition-readers
   slot-definition-writers
   slot-boundp-using-class
   slot-value-using-class
   slot-makunbound-using-class
   ;; generic function protocol
   compute-applicable-methods-using-classes
   finalize-inheritance))

(defpackage swank
  (:use cl swank/backend swank/match swank/rpc)
  (:export #:startup-multiprocessing
           #:start-server
           #:create-server
           #:stop-server
           #:restart-server
           #:ed-in-emacs
           #:inspect-in-emacs
           #:print-indentation-lossage
           #:invoke-slime-debugger
           #:swank-debugger-hook
           #:emacs-inspect
           ;;#:inspect-slot-for-emacs
           ;; These are user-configurable variables:
           #:*communication-style*
           #:*dont-close*
           #:*fasl-pathname-function*
           #:*log-events*
           #:*use-dedicated-output-stream*
           #:*dedicated-output-stream-port*
           #:*configure-emacs-indentation*
           #:*readtable-alist*
           #:*globally-redirect-io*
           #:*global-debugger*
           #:*sldb-quit-restart*
           #:*backtrace-printer-bindings*
           #:*default-worker-thread-bindings*
           #:*macroexpand-printer-bindings*
           #:*swank-pprint-bindings*
           #:*record-repl-results*
           #:*inspector-verbose*
           ;; This is SETFable.
           #:debug-on-swank-error
           ;; These are re-exported directly from the backend:
           #:buffer-first-change
           #:frame-source-location
           #:gdb-initial-commands
           #:restart-frame
           #:sldb-step
           #:sldb-break
           #:sldb-break-on-return
           #:profiled-functions
           #:profile-report
           #:profile-reset
           #:unprofile-all
           #:profile-package
           #:default-directory
           #:set-default-directory
           #:quit-lisp
           #:eval-for-emacs
           #:eval-in-emacs
           #:ed-rpc
           #:ed-rpc-no-wait
           #:y-or-n-p-in-emacs
           #:*find-definitions-right-trim*
           #:*find-definitions-left-trim*
           #:*after-toggle-trace-hook*
           #:unreadable-result
           #:unreadable-result-p
           #:unreadable-result-string
           #:parse-string
           #:from-string
           #:to-string
           #:*swank-debugger-condition*
           #:run-hook-with-args-until-success
           #:make-output-function-for-target
           #:make-output-stream-for-target))
