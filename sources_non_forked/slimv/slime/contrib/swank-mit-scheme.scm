;;; swank-mit-scheme.scm --- SLIME server for MIT Scheme
;;
;; Copyright (C) 2008  Helmut Eller
;;
;; This file is licensed under the terms of the GNU General Public
;; License as distributed with Emacs (press C-h C-c for details).

;;;; Installation:
#|

1. You need MIT Scheme 9.2

2. The Emacs side needs some fiddling.  I have the following in
   my .emacs:

(setq slime-lisp-implementations
      '((mit-scheme ("mit-scheme") :init mit-scheme-init)))

(defun mit-scheme-init (file encoding)
  (format "%S\n\n"
	  `(begin
	    (load-option 'format)
	    (load-option 'sos)
	    (eval 
	     '(create-package-from-description
	       (make-package-description '(swank) (list (list))
					 (vector) (vector) (vector) false))
	     (->environment '(package)))
	    (load ,(expand-file-name 
		    ".../contrib/swank-mit-scheme.scm" ; <-- insert your path
		    slime-path)
		  (->environment '(swank)))
	    (eval '(start-swank ,file) (->environment '(swank))))))

(defun mit-scheme ()
  (interactive)
  (slime 'mit-scheme))

(defun find-mit-scheme-package ()
  (save-excursion
    (let ((case-fold-search t))
      (and (re-search-backward "^[;]+ package: \\((.+)\\).*$" nil t)
	   (match-string-no-properties 1)))))

(setq slime-find-buffer-package-function 'find-mit-scheme-package)
(add-hook 'scheme-mode-hook (lambda () (slime-mode 1)))

   The `mit-scheme-init' function first loads the SOS and FORMAT
   libraries, then creates a package "(swank)", and loads this file
   into that package.  Finally it starts the server.  

   `find-mit-scheme-package' tries to figure out which package the
   buffer belongs to, assuming that ";;; package: (FOO)" appears
   somewhere in the file.  Luckily, this assumption is true for many of
   MIT Scheme's own files.  Alternatively, you could add Emacs style
   -*- slime-buffer-package: "(FOO)" -*- file variables.

4. Start everything with `M-x mit-scheme'.

|#

;;; package: (swank)

;; Modified for Slimv:
;; - load options
;; - remove extension in compile-file-for-emacs
(load-option 'format)
(load-option 'sos)

(if (< (car (get-subsystem-version "Release"))
       '9)
    (error "This file requires MIT Scheme Release 9"))

(define (swank port)
  (accept-connections (or port 4005) #f))

;; ### hardcoded port number for now.  netcat-openbsd doesn't print
;; the listener port anymore.
(define (start-swank port-file)
  (accept-connections 4055 port-file) 
  )

;;;; Networking

(define (accept-connections port port-file)
  (let ((sock (open-tcp-server-socket port (host-address-loopback))))
    (format #t "Listening on port: ~s~%" port)
    (if port-file (write-port-file port port-file))
    (dynamic-wind 
	(lambda () #f)
	(lambda () (serve (tcp-server-connection-accept sock #t #f)))
	(lambda () (close-tcp-server-socket sock)))))

(define (write-port-file portnumber filename)
  (call-with-output-file filename (lambda (p) (write portnumber p))))

(define *top-level-restart* #f)
(define (serve socket)
  (with-simple-restart 
      'disconnect "Close connection."
      (lambda ()
	(with-keyboard-interrupt-handler 
	    (lambda () (main-loop socket))))))

(define (disconnect) 
  (format #t "Disconnecting ...~%")
  (invoke-restart (find-restart 'disconnect)))

(define (main-loop socket)
  (do () (#f)
    (with-simple-restart 
	'abort "Return to SLIME top-level."
	(lambda () 
	  (fluid-let ((*top-level-restart* (find-restart 'abort)))
	    (dispatch (read-packet socket) socket 0))))))

(define (with-keyboard-interrupt-handler fun)
  (define (set-^G-handler exp)
    (eval `(vector-set! keyboard-interrupt-vector (char->integer #\G) ,exp)
	  (->environment '(runtime interrupt-handler))))
  (dynamic-wind
      (lambda () #f)
      (lambda ()
	(set-^G-handler
	 `(lambda (char) (with-simple-restart
			  'continue "Continue from interrupt."
			  (lambda () (error "Keyboard Interrupt.")))))
	(fun))
      (lambda ()
	(set-^G-handler '^G-interrupt-handler))))


;;;; Reading/Writing of SLIME packets

(define (read-packet in)
  "Read an S-expression from STREAM using the SLIME protocol."
  (let* ((len (read-length in))
	 (buffer (make-string len)))
    (fill-buffer! in buffer)
    (read-from-string buffer)))

(define (write-packet message out)
  (let* ((string (write-to-string message)))
    (log-event "WRITE: [~a]~s~%" (string-length string) string)
    (write-length (string-length string) out)
    (write-string string out)
    (flush-output out)))

(define (fill-buffer! in buffer)
  (read-string! buffer in))

(define (read-length in)
  (if (eof-object? (peek-char in)) (disconnect))
  (do ((len 6 (1- len))
       (sum 0 (+ (* sum 16) (char->hex-digit (read-char in)))))
      ((zero? len) sum)))

(define (ldb size position integer)
  "LoaD a Byte of SIZE bits at bit position POSITION from INTEGER."
  (fix:and (fix:lsh integer (- position))
	   (1- (fix:lsh 1 size))))

(define (write-length len out)
  (do ((pos 20 (- pos 4)))
      ((< pos 0))
    (write-hex-digit (ldb 4 pos len) out)))

(define (write-hex-digit n out)
  (write-char (hex-digit->char n) out))

(define (hex-digit->char n)
  (digit->char n 16))

(define (char->hex-digit c)
  (char->digit c 16))


;;;; Event dispatching

(define (dispatch request socket level)
  (log-event "READ: ~s~%" request)
  (case (car request)
    ((:emacs-rex) (apply emacs-rex socket level (cdr request)))))

(define (swank-package)
  (if (name->package '(swank))
      '(swank)
      '(user)))

(define *buffer-package* #f)
(define (find-buffer-package name)
  (if (elisp-false? name)
      #f
      (let ((v (ignore-errors 
		(lambda () (name->package (read-from-string name))))))
	(and (package? v) v))))

(define swank-env (->environment (swank-package)))
(define (user-env buffer-package)
  (cond ((string? buffer-package)
	 (let ((p (find-buffer-package buffer-package)))
	   (if (not p) (error "Invalid package name: " buffer-package))
	   (package/environment p)))
	(else (nearest-repl/environment))))

;; quote keywords
(define (hack-quotes list)
  (map (lambda (x)
	 (cond ((symbol? x) `(quote ,x))
	       (#t x)))
       list))

(define (emacs-rex socket level sexp package thread id)
  (let ((ok? #f) (result #f) (condition #f))
    (dynamic-wind
	(lambda () #f)
	(lambda ()
	  (bind-condition-handler 
	   (list condition-type:serious-condition)
	   (lambda (c) (set! condition c) (invoke-sldb socket (1+ level) c))
	   (lambda ()
	     (fluid-let ((*buffer-package* package))
	       (set! result 
		     (eval (cons* (car sexp) socket (hack-quotes (cdr sexp)))
			   swank-env))
	       (set! ok? #t)))))
	(lambda ()
	  (write-packet `(:return 
			  ,(if ok? `(:ok ,result)
			       `(:abort 
				 ,(if condition 
				      (format #f "~a"
					      (condition/type condition))
				      "<unknown reason>")))
			  ,id)
			 socket)))))

(define (swank:connection-info _)
  (let ((p (environment->package (user-env #f))))
    `(:pid ,(unix/current-pid)
      :package (:name ,(write-to-string (package/name p))
		      :prompt ,(write-to-string (package/name p)))
      :lisp-implementation 
      (:type "MIT Scheme" :version ,(get-subsystem-version-string "release"))
      :encoding (:coding-systems ("iso-8859-1"))
      )))

(define (swank:quit-lisp _)
  (%exit))


;;;; Evaluation

(define (swank-repl:listener-eval socket string)
  ;;(call-with-values (lambda () (eval-region string socket))
  ;;  (lambda values `(:values . ,(map write-to-string values))))
  `(:values ,(write-to-string (eval-region string socket))))

(define (eval-region string socket)
  (let ((sexp (read-from-string string)))
    (if (eof-object? exp)
	(values)
	(with-output-to-repl socket
	  (lambda () (eval sexp (user-env *buffer-package*)))))))

(define (with-output-to-repl socket fun)
  (let ((p (make-port repl-port-type socket)))
    (dynamic-wind
	(lambda () #f)
	(lambda () (with-output-to-port p fun))
	(lambda () (flush-output p)))))

(define (swank:interactive-eval socket string)
  ;;(call-with-values (lambda () (eval-region string)) format-for-echo-area)
  (format-values (eval-region string socket))
  )

(define (format-values . values)
  (if (null? values) 
      "; No value"
      (with-string-output-port
	  (lambda (out)
	    (write-string "=> " out)
	    (do ((vs values (cdr vs))) ((null? vs))
	      (write (car vs) out)
	      (if (not (null? (cdr vs)))
		  (write-string ", " out)))))))

(define (swank:pprint-eval _ string)
  (pprint-to-string (eval (read-from-string string) 
			  (user-env *buffer-package*))))

(define (swank:interactive-eval-region socket string)
  (format-values (eval-region string socket)))

(define (swank:set-package _ package)
  (set-repl/environment! (nearest-repl) 
			 (->environment (read-from-string package)))
  (let* ((p (environment->package (user-env #f)))
	 (n (write-to-string (package/name p))))
    (list n n)))

 
(define (repl-write-substring port string start end)
  (cond ((< start end)
	 (write-packet `(:write-string ,(substring string start end))
		       (port/state port))))
  (- end start))

(define (repl-write-char port char)
  (write-packet `(:write-string ,(string char))
                (port/state port)))

(define repl-port-type
  (make-port-type `((write-substring ,repl-write-substring)
		    (write-char ,repl-write-char)) #f))

(define (swank-repl:create-repl socket . _)
  (let* ((env (user-env #f))
	 (name (format #f "~a" (package/name (environment->package env)))))
    (list name name)))


;;;; Compilation

(define (swank:compile-string-for-emacs _ string . x)
  (apply 
   (lambda (errors seconds)
     `(:compilation-result ,errors t ,seconds nil nil))
   (call-compiler
    (lambda ()
      (let* ((sexps (snarf-string string))
	     (env (user-env *buffer-package*))
	     (scode (syntax `(begin ,@sexps) env))
	     (compiled-expression (compile-scode scode #t)))
	(scode-eval compiled-expression env))))))

(define (snarf-string string)
  (with-input-from-string string
    (lambda () 
      (let loop ()
	(let ((e (read)))
	  (if (eof-object? e) '() (cons e (loop))))))))

(define (call-compiler fun)
  (let ((time #f))
    (with-timings fun
      (lambda (run-time gc-time real-time)
	(set! time real-time)))
    (list 'nil (internal-time/ticks->seconds time))))

(define (swank:compiler-notes-for-emacs _) nil)

(define (swank:compile-file-for-emacs socket file load?)
  (apply
   (lambda (errors seconds)
     (list ':compilation-result errors 't seconds load? 
	   (->namestring (pathname-name file))))
   (call-compiler
    (lambda () (with-output-to-repl socket (lambda () (compile-file file)))))))

(define (swank:load-file socket file)
  (with-output-to-repl socket
    (lambda () 
      (pprint-to-string 
       (load file (user-env *buffer-package*))))))

(define (swank:disassemble-form _ string)
  (let ((sexp (let ((sexp (read-from-string string)))
		(cond ((and (pair? sexp) (eq? (car sexp) 'quote))
		       (cadr sexp))
		      (#t sexp)))))
    (with-output-to-string
      (lambda () 
	(compiler:disassemble
	 (eval sexp (user-env *buffer-package*)))))))

(define (swank:disassemble-symbol _ string)
  (with-output-to-string
      (lambda () 
	(compiler:disassemble
	 (eval (read-from-string string) 
	       (user-env *buffer-package*))))))


;;;; Macroexpansion

(define (swank:swank-macroexpand-all _ string) 
  (with-output-to-string
      (lambda ()
	(pp (syntax (read-from-string string)
		    (user-env *buffer-package*))))))
(define swank:swank-macroexpand-1 swank:swank-macroexpand-all)
(define swank:swank-macroexpand swank:swank-macroexpand-all)


;;; Arglist

(define (swank:operator-arglist socket name pack)
  (let ((v (ignore-errors
	    (lambda ()
              (string-trim-right
               (with-output-to-string
                 (lambda ()
                   (carefully-pa
                    (eval (read-from-string name) (user-env pack))))))))))
    (if (condition? v) 'nil v)))

(define (carefully-pa o)
  (cond ((arity-dispatched-procedure? o) 
	 ;; MIT Scheme crashes for (pa /)
	 (display "arity-dispatched-procedure"))
	((procedure? o) (pa o))
	(else (error "Not a procedure"))))


;;; Some unimplemented stuff.
(define (swank:buffer-first-change . _) nil)
(define (swank:filename-to-modulename . _) nil)
(define (swank:swank-require . _) nil)

;; M-. is beyond my capabilities.
(define (swank:find-definitions-for-emacs . _) nil)


;;; Debugger

(define-structure (sldb-state (conc-name sldb-state.)) condition restarts)

(define *sldb-state* #f)
(define (invoke-sldb socket level condition)
  (fluid-let ((*sldb-state* (make-sldb-state condition (bound-restarts))))
    (dynamic-wind 
	(lambda () #f)
	(lambda ()
	  (write-packet `(:debug 0 ,level ,@(sldb-info *sldb-state* 0 20))
			socket)
	  (sldb-loop level socket))
	(lambda ()
	  (write-packet `(:debug-return 0 ,level nil) socket)))))

(define (sldb-loop level socket)
  (write-packet `(:debug-activate 0 ,level) socket)
  (with-simple-restart 
      'abort (format #f "Return to SLDB level ~a." level)
      (lambda () (dispatch (read-packet socket) socket level)))
  (sldb-loop level socket))

(define (sldb-info state start end)
  (let ((c (sldb-state.condition state))
	(rs (sldb-state.restarts state)))
    (list (list (condition/report-string c)
		(format #f "  [~a]" (%condition-type/name (condition/type c)))
		nil)
	  (sldb-restarts rs)
	  (sldb-backtrace c start end)
	  ;;'((0 "dummy frame"))
	  '())))

(define %condition-type/name
  (eval '%condition-type/name (->environment '(runtime error-handler))))

(define (sldb-restarts restarts)
  (map (lambda (r) 
	 (list (symbol->string (restart/name r))
	       (with-string-output-port 
		(lambda (p) (write-restart-report r p)))))
       restarts))

(define (swank:throw-to-toplevel . _)
  (invoke-restart *top-level-restart*))

(define (swank:sldb-abort . _)
  (abort (sldb-state.restarts *sldb-state*)))

(define (swank:sldb-continue . _)
  (continue (sldb-state.restarts *sldb-state*)))

(define (swank:invoke-nth-restart-for-emacs _ _sldb-level n)
  (invoke-restart (list-ref (sldb-state.restarts *sldb-state*) n)))

(define (swank:debugger-info-for-emacs _ from to)
  (sldb-info *sldb-state* from to))

(define (swank:backtrace _ from to)
  (sldb-backtrace (sldb-state.condition *sldb-state*) from to))

(define (sldb-backtrace condition from to)
  (sldb-backtrace-aux (condition/continuation condition) from to))

(define (sldb-backtrace-aux k from to)
  (let ((l (map frame>string (substream (continuation>frames k) from to))))
    (let loop ((i from) (l l))
      (if (null? l)
	  '()
	  (cons (list i (car l)) (loop (1+ i) (cdr l)))))))

;; Stack parser fails for this:
;; (map (lambda (x) x) "/tmp/x.x")

(define (continuation>frames k)
  (let loop ((frame (continuation->stack-frame k)))
    (cond ((not frame)	(stream))
	  (else
	   (let ((next (ignore-errors
			(lambda () (stack-frame/next-subproblem frame)))))
	     (cons-stream frame
			  (if (condition? next) 
			      (stream next) 
			      (loop next))))))))

(define (frame>string frame)
  (if (condition? frame)
      (format #f "Bogus frame: ~a ~a" frame
	      (condition/report-string frame))
      (with-string-output-port (lambda (p) (print-frame frame p)))))

(define (print-frame frame port)
  (define (invalid-subexpression? subexpression)
    (or (debugging-info/undefined-expression? subexpression)
	(debugging-info/unknown-expression? subexpression)))
  (define (invalid-expression? expression)
    (or (debugging-info/undefined-expression? expression)
	(debugging-info/compiled-code? expression)))
  (with-values (lambda () (stack-frame/debugging-info frame))
    (lambda (expression environment subexpression)
      (cond ((debugging-info/compiled-code? expression)
	     (write-string ";unknown compiled code" port))
	    ((not (debugging-info/undefined-expression? expression))
	     (fluid-let ((*unparse-primitives-by-name?* #t))
	       (write
		(unsyntax (if (invalid-subexpression? subexpression)
			      expression
			      subexpression))
		port)))
	    ((debugging-info/noise? expression)
	     (write-string ";" port)
	     (write-string ((debugging-info/noise expression) #f)
			   port))
	    (else
	     (write-string ";undefined expression" port))))))

(define (substream s from to)
  (let loop ((i 0) (l '()) (s s))
    (cond ((or (= i to) (stream-null? s)) (reverse l))
	  ((< i from) (loop (1+ i) l (stream-cdr s)))
	  (else (loop (1+ i) (cons (stream-car s) l) (stream-cdr s))))))

(define (swank:frame-locals-and-catch-tags _ frame)
  (list (map frame-var>elisp (frame-vars (sldb-get-frame frame)))
	'()))
  
(define (frame-vars frame)
  (with-values (lambda () (stack-frame/debugging-info frame))
    (lambda (expression environment subexpression)
      (cond ((environment? environment)
	     (environment>frame-vars environment))
	    (else '())))))

(define (environment>frame-vars environment)
  (let loop ((e environment))
    (cond ((environment->package e) '())
	  (else (append (environment-bindings e)
			(if (environment-has-parent? e)
			    (loop (environment-parent e))
			    '()))))))

(define (frame-var>elisp b)
  (list ':name (write-to-string (car b))
	':value (cond ((null? (cdr b)) "{unavailable}")
		      (else (>line (cadr b))))
	':id 0))

(define (sldb-get-frame index)
  (stream-ref (continuation>frames 
	       (condition/continuation 
		(sldb-state.condition *sldb-state*)))
	      index))

(define (frame-var-value frame var)
  (let ((binding (list-ref (frame-vars frame) var)))
    (cond ((cdr binding) (cadr binding))
	  (else unspecific))))

(define (swank:inspect-frame-var _ frame var)
  (reset-inspector)
  (inspect-object (frame-var-value (sldb-get-frame frame) var)))


;;;; Completion

(define (swank:simple-completions _ string package)
  (let ((strings (all-completions string (user-env package) string-prefix?)))
    (list (sort strings string<?)
	  (longest-common-prefix strings))))

(define (all-completions pattern env match?)
  (let ((ss (map %symbol->string (environment-names env))))
    (keep-matching-items ss (lambda (s) (match? pattern s)))))

;; symbol->string is too slow
(define %symbol->string symbol-name)

(define (environment-names env)
  (append (environment-bound-names env)
	  (if (environment-has-parent? env)
	      (environment-names (environment-parent env))
	      '())))

(define (longest-common-prefix strings)
  (define (common-prefix s1 s2)
    (substring s1 0 (string-match-forward s1 s2)))
  (reduce common-prefix "" strings))


;;;; Apropos

(define (swank:apropos-list-for-emacs _ name #!optional 
				      external-only case-sensitive package)
  (let* ((pkg (and (string? package)
		   (find-package (read-from-string package))))
	 (parent (and (not (default-object? external-only))
		      (elisp-false? external-only)))
	 (ss  (append-map (lambda (p)
			    (map (lambda (s) (cons p s))
				 (apropos-list name p (and pkg parent))))
			  (if pkg (list pkg) (all-packages))))
	 (ss (sublist ss 0 (min (length ss) 200))))
    (map (lambda (e)
	   (let ((p (car e)) (s (cdr e)))
	     (list ':designator (format #f "~a ~a" s (package/name p))
		   ':variable (>line
			       (ignore-errors
				(lambda () (package-lookup p s)))))))
	 ss)))

(define (swank:list-all-package-names . _)
  (map (lambda (p) (write-to-string (package/name p)))
       (all-packages)))

(define (all-packages)
  (define (package-and-children package)
    (append (list package)
	    (append-map package-and-children (package/children package))))
  (package-and-children system-global-package))


;;;; Inspector

(define-structure (inspector-state (conc-name istate.))
  object parts next previous content)

(define istate #f)

(define (reset-inspector)
  (set! istate #f))

(define (swank:init-inspector _ string)
  (reset-inspector)
  (inspect-object (eval (read-from-string string) 
			(user-env *buffer-package*))))

(define (inspect-object o)
  (let ((previous istate)
	(content (inspect o))
	(parts (make-eqv-hash-table)))
    (set! istate (make-inspector-state o parts #f previous content))
    (if previous (set-istate.next! previous istate))
    (istate>elisp istate)))

(define (istate>elisp istate)
  (list ':title (>line (istate.object istate))
	':id (assign-index (istate.object istate) (istate.parts istate))
	':content (prepare-range (istate.parts istate)
				 (istate.content istate)
				 0 500)))

(define (assign-index o parts)
  (let ((i (hash-table/count parts)))
    (hash-table/put! parts i o)
    i))

(define (prepare-range parts content from to)
  (let* ((cs (substream content from to))
	 (ps (prepare-parts cs parts)))
    (list ps
	  (if (< (length cs) (- to from))
	      (+ from (length cs))
	      (+ to 1000))
	  from to)))

(define (prepare-parts ps parts)
  (define (line label value)
    `(,(format #f "~a: " label)
      (:value ,(>line value) ,(assign-index value parts))
      "\n"))
  (append-map (lambda (p)
		(cond ((string? p) (list p))
		      ((symbol? p) (list (symbol->string p)))
		      (#t
		       (case (car p)
			 ((line) (apply line (cdr p)))
			 (else (error "Invalid part:" p))))))
	      ps))

(define (swank:inspect-nth-part _ index)
  (inspect-object (hash-table/get (istate.parts istate) index 'no-such-part)))

(define (swank:quit-inspector _)
  (reset-inspector))

(define (swank:inspector-pop _)
  (cond ((istate.previous istate)
	 (set! istate (istate.previous istate))
	 (istate>elisp istate))
	(else 'nil)))

(define (swank:inspector-next _)
  (cond ((istate.next istate)
	 (set! istate (istate.next istate))
	 (istate>elisp istate))
	(else 'nil)))

(define (swank:inspector-range _ from to)
  (prepare-range (istate.parts istate)
		 (istate.content istate)
		 from to))

(define-syntax stream*
  (syntax-rules ()
    ((stream* tail) tail)
    ((stream* e1 e2 ...) (cons-stream e1 (stream* e2 ...)))))

(define (iline label value) `(line ,label ,value))

(define-generic inspect (o))

(define-method inspect ((o <object>))
  (cond ((environment? o) (inspect-environment o))
	((vector? o) (inspect-vector o))
	((procedure? o) (inspect-procedure o))
	((compiled-code-block? o) (inspect-code-block o))
	;;((system-pair? o) (inspect-system-pair o))
	((probably-scode? o) (inspect-scode o))
	(else (inspect-fallback o))))

(define (inspect-fallback o)
  (let* ((class (object-class o))
	 (slots (class-slots class)))
    (stream*
     (iline "Class" class)
     (let loop ((slots slots))
       (cond ((null? slots) (stream))
	     (else
	      (let ((n (slot-name (car slots))))
		(stream* (iline n (slot-value o n))
			 (loop (cdr slots))))))))))

(define-method inspect ((o <pair>))
  (if (or (pair? (cdr o)) (null? (cdr o)))
      (inspect-list o)
      (inspect-cons o)))

(define (inspect-cons o)
  (stream (iline "car" (car o))
	  (iline "cdr" (cdr o))))

(define (inspect-list o)
  (let loop ((i 0) (o o))
    (cond ((null? o) (stream))
	  ((or (pair? (cdr o)) (null? (cdr o)))
	   (stream* (iline i (car o))
		    (loop (1+ i) (cdr o))))
	  (else 
	   (stream (iline i (car o))
		   (iline "tail" (cdr o)))))))

(define (inspect-environment o)
  (stream*
   (iline "(package)" (environment->package o))
   (let loop ((bs (environment-bindings o)))
     (cond ((null? bs)
	    (if (environment-has-parent? o) 
		(stream (iline "(<parent>)" (environment-parent o)))
		(stream)))
	   (else 
	    (let* ((b (car bs)) (s (car b)))
	      (cond ((null? (cdr b))
		     (stream* s " {" (environment-reference-type o s) "}\n"
			      (loop (cdr bs))))
		    (else 
		     (stream* (iline s (cadr b))
			      (loop (cdr bs)))))))))))

(define (inspect-vector o)
  (let ((len (vector-length o)))
    (let loop ((i 0))
      (cond ((= i len) (stream))
	    (else (stream* (iline i (vector-ref o i))
			   (loop (1+ i))))))))

(define (inspect-procedure o)
  (cond ((primitive-procedure? o)
	 (stream (iline "name" (primitive-procedure-name o))
		 (iline "arity" (primitive-procedure-arity o))
		 (iline "doc" (primitive-procedure-documentation o))))
	((compound-procedure? o)
	 (stream (iline "arity" (procedure-arity o))
		 (iline "lambda" (procedure-lambda o))
		 (iline "env" (ignore-errors
			       (lambda () (procedure-environment o))))))
	(else
	 (stream
	  (iline "block" (compiled-entry/block o))
	  (with-output-to-string (lambda () (compiler:disassemble o)))))))

(define (inspect-code-block o)
  (stream-append
   (let loop ((i (compiled-code-block/constants-start o)))
     (cond ((>= i (compiled-code-block/constants-end o)) (stream))
	   (else 
	    (stream* 
	     (iline i (system-vector-ref o i))
	     (loop (+ i compiled-code-block/bytes-per-object))))))
   (stream (iline "debuginfo" (compiled-code-block/debugging-info o))
	   (iline "env" (compiled-code-block/environment o))
	   (with-output-to-string (lambda () (compiler:disassemble o))))))

(define (inspect-scode o)
  (stream (pprint-to-string o)))

(define (probably-scode? o)
  (define tests (list access? assignment? combination? comment?
		      conditional? definition? delay? disjunction? lambda?
		      quotation? sequence? the-environment? variable?))
  (let loop ((tests tests))
    (cond ((null? tests) #f)
	  (((car tests) o))
	  (else (loop (cdr tests))))))

(define (inspect-system-pair o)
  (stream (iline "car" (system-pair-car o))
	  (iline "cdr" (system-pair-cdr o))))


;;;; Auxilary functions

(define nil '())
(define t 't)
(define (elisp-false? o) (member o '(nil ())))
(define (elisp-true? o) (not (elisp-false? o)))
(define (>line o) 
  (let ((r (write-to-string o 100)))
    (cond ((not (car r)) (cdr r))
	  (else (string-append (cdr r) " ..")))))
;; Must compile >line otherwise we can't write unassigend-reference-traps.
(set! >line (compile-procedure >line))
(define (read-from-string s) (with-input-from-string s read))
(define (pprint-to-string o) 
  (with-string-output-port 
      (lambda (p)
	(fluid-let ((*unparser-list-breadth-limit* 10)
		    (*unparser-list-depth-limit* 4)
		    (*unparser-string-length-limit* 100))
	  (pp o p)))))
;(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (package-lookup package name)
  (let ((p (if (package? package) package (find-package package))))
    (environment-lookup (package/environment p) name)))
(define log-port (current-output-port))
(define (log-event fstring . args)
  ;;(apply format log-port fstring args)
  #f
  )

;; Modified for Slimv:
;; - restart swank server in a loop
(let loop ()
 (swank 4005)
 (loop))

;;; swank-mit-scheme.scm ends here
