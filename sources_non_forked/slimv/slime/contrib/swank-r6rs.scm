;; swank-r6rs.sls --- Shareable code between swank-ikarus and swank-larceny
;;
;; Licence: public domain
;; Author: Helmut Eller
;;
;; This is a Swank server barely capable enough to process simple eval
;; requests from Emacs before dying.  No fancy features like
;; backtraces, module redefintion, M-. etc. are implemented.  Don't
;; even think about pc-to-source mapping.
;;
;; Despite standard modules, this file uses (swank os) and (swank sys)
;; which define implementation dependend functionality.  There are
;; multiple modules in this files, which is probably not standardized.
;;

;; Naive FORMAT implementation which supports: ~a ~s ~d ~x ~c
(library (swank format)
    (export format printf fprintf)
    (import (rnrs))

 (define (format f . args)
   (call-with-string-output-port
    (lambda (port) (apply fprintf port f args))))

 (define (printf f . args)
   (let ((port (current-output-port)))
     (apply fprintf port f args)
     (flush-output-port port)))

 (define (fprintf port f . args)
   (let ((len (string-length f)))
     (let loop ((i 0) (args args))
       (cond ((= i len) (assert (null? args)))
	     ((and (char=? (string-ref f i) #\~)
		   (< (+ i 1) len))
	      (dispatch-format (string-ref f (+ i 1)) port (car args))
	      (loop (+ i 2) (cdr args)))
	     (else
	      (put-char port (string-ref f i))
	      (loop (+ i 1) args))))))
 
 (define (dispatch-format char port arg)
   (let ((probe (assoc char format-dispatch-table)))
     (cond (probe ((cdr probe) arg port))
	   (else (error "invalid format char: " char)))))

 (define format-dispatch-table 
   `((#\a . ,display)
     (#\s . ,write)
     (#\d . ,(lambda (arg port) (put-string port (number->string arg 10))))
     (#\x . ,(lambda (arg port) (put-string port (number->string arg 16))))
     (#\c . ,(lambda (arg port) (put-char port arg))))))


;; CL-style restarts to let us continue after errors.
(library (swank restarts)
    (export with-simple-restart compute-restarts invoke-restart restart-name
	    write-restart-report)
    (import (rnrs))

 (define *restarts* '())

 (define-record-type restart
   (fields name reporter continuation))
 
 (define (with-simple-restart name reporter thunk)
   (call/cc 
    (lambda (k)
      (let ((old-restarts *restarts*)
	    (restart (make-restart name (coerce-to-reporter reporter) k)))
	(dynamic-wind
	    (lambda () (set! *restarts* (cons restart old-restarts)))
	    thunk
	    (lambda () (set! *restarts* old-restarts)))))))

 (define (compute-restarts) *restarts*)

 (define (invoke-restart restart . args)
   (apply (restart-continuation restart) args))

 (define (write-restart-report restart port)
   ((restart-reporter restart) port))

 (define (coerce-to-reporter obj)
   (cond ((string? obj) (lambda (port) (put-string port obj)))
	 (#t (assert (procedure? obj)) obj)))

 )

;; This module encodes & decodes messages from the wire and queues them.
(library (swank event-queue)
    (export make-event-queue wait-for-event enqueue-event 
	    read-event write-event)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (swank format))

 (define-record-type event-queue
   (fields (mutable q) wait-fun)
   (protocol (lambda (init)
	       (lambda (wait-fun)
		 (init '() wait-fun)))))

 (define (wait-for-event q pattern)
   (or (poll q pattern)
       (begin
	 ((event-queue-wait-fun q) q)
	 (wait-for-event q pattern))))
 
 (define (poll q pattern)
   (let loop ((lag #f)
	      (l (event-queue-q q)))
     (cond ((null? l) #f)
	   ((event-match? (car l) pattern)
	    (cond (lag 
		   (set-cdr! lag (cdr l))
		   (car l))
		  (else
		   (event-queue-q-set! q (cdr l))
		   (car l))))
	   (else (loop l (cdr l))))))

 (define (event-match? event pattern)
   (cond ((or (number? pattern)
	      (member pattern '(t nil)))
	  (equal? event pattern))
	 ((symbol? pattern) #t)
	 ((pair? pattern)
	  (case (car pattern)
	    ((quote) (equal? event (cadr pattern)))
	    ((or) (exists (lambda (p) (event-match? event p)) (cdr pattern)))
	    (else (and (pair? event)
		       (event-match? (car event) (car pattern))
		       (event-match? (cdr event) (cdr pattern))))))
	 (else (error "Invalid pattern: " pattern))))
 
 (define (enqueue-event q event)
   (event-queue-q-set! q
		       (append (event-queue-q q) 
			       (list event))))

 (define (write-event event port)
   (let ((payload (call-with-string-output-port
		   (lambda (port) (write event port)))))
     (write-length (string-length payload) port)
     (put-string port payload)
     (flush-output-port port)))

 (define (write-length len port)
   (do ((i 24 (- i 4)))
       ((= i 0))
     (put-string port
		 (number->string (bitwise-bit-field len (- i 4) i)
				 16))))

 (define (read-event port)
   (let* ((header (string-append (get-string-n port 2) 
				 (get-string-n port 2)
				 (get-string-n port 2)))
	  (_ (printf "header: ~s\n" header))
	  (len (string->number header 16))
	  (_ (printf "len: ~s\n" len))
	  (payload (get-string-n port len)))
     (printf "payload: ~s\n" payload)
     (read (open-string-input-port payload))))

 )

;; Entry points for SLIME commands.
(library (swank rpc)
    (export connection-info interactive-eval
	    ;;compile-string-for-emacs 
	    throw-to-toplevel sldb-abort
	    operator-arglist buffer-first-change
	    create-repl listener-eval)
    (import (rnrs)
	    (rnrs eval)
	    (only (rnrs r5rs) scheme-report-environment)
	    (swank os)
	    (swank format)
	    (swank restarts)
	    (swank sys)
	    )
 
 (define (connection-info . _)
   `(,@'()
     :pid ,(getpid) 
     :package (:name ">" :prompt ">")
     :lisp-implementation (,@'() 
			   :name ,(implementation-name)
			   :type "R6RS-Scheme")))

 (define (interactive-eval string)
   (call-with-values 
       (lambda ()
	 (eval-in-interaction-environment (read-from-string string)))
     (case-lambda
      (() "; no value")
      ((value) (format "~s" value))
      (values (format "values: ~s" values)))))
 
 (define (throw-to-toplevel) (invoke-restart-by-name-or-nil 'toplevel))

 (define (sldb-abort) (invoke-restart-by-name-or-nil 'abort))
 
 (define (invoke-restart-by-name-or-nil name)
   (let ((r (find (lambda (r) (eq? (restart-name r) name))
		  (compute-restarts))))
     (if r (invoke-restart r) 'nil)))

 (define (create-repl target)
   (list "" ""))

 (define (listener-eval string)
   (call-with-values (lambda () (eval-region string))
     (lambda values `(:values ,@(map (lambda (v) (format "~s" v)) values)))))

 (define (eval-region string)
   (let ((sexp (read-from-string string)))
     (if (eof-object? exp)
	 (values)
	 (eval-in-interaction-environment sexp))))

 (define (read-from-string string)
   (call-with-port (open-string-input-port string) read))

 (define (operator-arglist . _) 'nil)
 (define (buffer-first-change . _) 'nil)

 )

;; The server proper.  Does the TCP stuff and exception handling.
(library (swank)
    (export start-server)
    (import (rnrs) 
	    (rnrs eval)
	    (swank os)
	    (swank format)
	    (swank event-queue)
	    (swank restarts))

 (define-record-type connection
   (fields in-port out-port event-queue))

 (define (start-server port)
   (accept-connections (or port 4005) #f))

 (define (start-server/port-file port-file)
   (accept-connections #f port-file))

 (define (accept-connections port port-file)
   (let ((sock (make-server-socket port)))
     (printf "Listening on port: ~s\n" (local-port sock))
     (when port-file 
       (write-port-file (local-port sock) port-file))
     (let-values (((in out) (accept sock (latin-1-codec))))
       (dynamic-wind 
	   (lambda () #f)
	   (lambda () 
	     (close-socket sock)
	     (serve in out))
	   (lambda () 
	     (close-port in)
	     (close-port out))))))

 (define (write-port-file port port-file)
   (call-with-output-file 
       (lambda (file) 
	 (write port file))))

 (define (serve in out) 
   (let ((err (current-error-port))
	 (q (make-event-queue 
	     (lambda (q)
	       (let ((e (read-event in)))
		 (printf "read: ~s\n" e)
		 (enqueue-event q e))))))
     (dispatch-loop (make-connection in out q))))

 (define-record-type sldb-state
   (fields level condition continuation next))

 (define (dispatch-loop conn)
   (let ((event (wait-for-event (connection-event-queue conn) 'x)))
     (case (car event)
       ((:emacs-rex) 
	(with-simple-restart 
	 'toplevel "Return to SLIME's toplevel"
	 (lambda ()
	   (apply emacs-rex conn #f (cdr event)))))
       (else (error "Unhandled event: ~s" event))))
   (dispatch-loop conn))

 (define (recover thunk on-error-thunk)
   (let ((ok #f))
     (dynamic-wind 
	 (lambda () #f) 
	 (lambda () 
	   (call-with-values thunk 
	     (lambda vals 
	       (set! ok #t) 
	       (apply values vals))))
	 (lambda ()
	   (unless ok
	     (on-error-thunk))))))

 ;; Couldn't resist to exploit the prefix feature.
 (define rpc-entries (environment '(prefix (swank rpc) swank:)))
 
 (define (emacs-rex conn sldb-state form package thread tag)
   (let ((out (connection-out-port conn)))
     (recover
      (lambda ()
	(with-exception-handler
	 (lambda (condition) 
	   (call/cc 
	    (lambda (k)
	      (sldb-exception-handler conn condition k sldb-state))))
	 (lambda ()
	   (let ((value (apply (eval (car form) rpc-entries) (cdr form))))
	     (write-event `(:return (:ok ,value) ,tag) out)))))
      (lambda ()
	(write-event `(:return (:abort) ,tag) out)))))

 (define (sldb-exception-handler connection condition k sldb-state)
   (when (serious-condition? condition)
     (let ((level (if sldb-state (+ (sldb-state-level sldb-state) 1) 1))
	   (out (connection-out-port connection)))
       (write-event `(:debug 0 ,level ,@(debugger-info condition connection))
		    out)
       (dynamic-wind
	   (lambda () #f)
	   (lambda ()
	     (sldb-loop connection 
			(make-sldb-state level condition k sldb-state)))
	   (lambda () (write-event `(:debug-return 0 ,level nil) out))))))

 (define (sldb-loop connection state)
   (apply emacs-rex connection state
	  (cdr (wait-for-event (connection-event-queue connection) 
			       '(':emacs-rex . _))))
   (sldb-loop connection state))

 (define (debugger-info condition connection)
   (list `(,(call-with-string-output-port 
	     (lambda (port) (print-condition condition port)))
	   ,(format " [type ~s]" (if (record? condition)
				     (record-type-name (record-rtd condition))
				     ))
	   ())
	 (map (lambda (r) 
		(list (format "~a" (restart-name r))
		      (call-with-string-output-port
		       (lambda (port)
			 (write-restart-report r port)))))
	      (compute-restarts))
	 '()
	 '()))

 (define (print-condition obj port)
   (cond ((condition? obj)
	  (let ((list (simple-conditions obj)))
	    (case (length list)
	      ((0)
	       (display "Compuond condition with zero components" port))
	      ((1)
	       (assert (eq? obj (car list)))
	       (print-simple-condition (car list) port))
	      (else
	       (display "Compound condition:\n" port)
	       (for-each (lambda (c)
			   (display "  " port)
			   (print-simple-condition c port)
			   (newline port))
			 list)))))
	 (#t
	  (fprintf port "Non-condition object: ~s" obj))))

 (define (print-simple-condition condition port)
   (fprintf port "~a" (record-type-name (record-rtd condition)))
   (case (count-record-fields condition)
     ((0) #f)
     ((1) 
      (fprintf port ": ")
      (do-record-fields condition (lambda (name value) (write value port))))
     (else
      (fprintf port ":")
      (do-record-fields condition (lambda (name value) 
				    (fprintf port "\n~a: ~s" name value))))))

 ;; Call FUN with RECORD's rtd and parent rtds.
 (define (do-record-rtds record fun)
   (do ((rtd (record-rtd record) (record-type-parent rtd)))
       ((not rtd))
     (fun rtd)))

 ;; Call FUN with RECORD's field names and values.
 (define (do-record-fields record fun)
   (do-record-rtds 
    record
    (lambda (rtd)
      (let* ((names (record-type-field-names rtd))
	     (len (vector-length names)))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (fun (vector-ref names i) ((record-accessor rtd i) record)))))))

 ;; Return the number of fields in RECORD
 (define (count-record-fields record)
   (let ((i 0))
     (do-record-rtds 
      record (lambda (rtd) 
	       (set! i (+ i (vector-length (record-type-field-names rtd))))))
     i))

 )
