;;;; swank-kawa.scm --- Swank server for Kawa
;;;
;;; Copyright (C) 2007  Helmut Eller
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c for details).

;;;; Installation
;;
;; 1. You need Kawa (version 2.x) and a JVM with debugger support.
;;
;; 2. Compile this file and create swank-kawa.jar with:
;;      java -cp kawa.jar:$JAVA_HOME/lib/tools.jar \
;;           -Xss2M kawa.repl --r7rs -d classes -C swank-kawa.scm &&
;;      jar cf swank-kawa.jar -C classes .
;;
;; 3. Add something like this to your .emacs:
#|
;; Kawa, Swank, and the debugger classes (tools.jar) must be in the
;; classpath.  You also need to start the debug agent.
(setq slime-lisp-implementations
      '((kawa
         ("java"
          ;; needed jar files
          "-cp" "kawa-2.0.1.jar:swank-kawa.jar:/opt/jdk1.8.0/lib/tools.jar"
          ;; channel for debugger
          "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"
          ;; depending on JVM, compiler may need more stack
          "-Xss2M"
          ;; kawa without GUI
          "kawa.repl" "-s")
         :init kawa-slime-init)))

(defun kawa-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (format "%S\n"
          `(begin (import (swank-kawa))
                  (start-swank ,file)
                  ;; Optionally add source paths of your code so
                  ;; that M-. works better:
                  ;;(set! swank-java-source-path
                  ;;  (append
                  ;;   '(,(expand-file-name "~/lisp/slime/contrib/")
                  ;;     "/scratch/kawa")
                  ;;   swank-java-source-path))
                  )))

;; Optionally define a command to start it.
(defun kawa ()
  (interactive)
  (slime 'kawa))

|#
;; 4. Start everything with  M-- M-x slime kawa
;;
;;


;;; Code:

(define-library (swank macros)
    (export df fun seq set fin esc
            ! !! !s @ @s
            when unless while dotimes dolist for packing with pushf == assert
            mif mcase mlet mlet* typecase ignore-errors
            ferror
            )
    (import (scheme base)
            (only (kawa base)
                  syntax
                  quasisyntax
                  syntax-case
                  define-syntax-case
                  identifier?

                  invoke
                  invoke-static
                  field
                  static-field
                  instance?
                  try-finally
                  try-catch
                  primitive-throw

                  format
                  reverse!
                  as
                  ))
  (begin "
("

(define (ferror fstring #!rest args)
  (let ((err (<java.lang.Error>
              (as <java.lang.String> (apply format fstring args)))))
    (primitive-throw err)))

(define (rewrite-lambda-list args)
  (syntax-case args ()
    (() #`())
    ((rest x ...) (eq? #'rest #!rest) args)
    ((optional x ...) (eq? #'optional #!optional) args)
    ((var args ...) (identifier? #'var)
     #`(var #,@(rewrite-lambda-list #'(args ...))))
    (((var type) args ...) (identifier? #'var)
     #`((var :: type) #,@(rewrite-lambda-list #'(args ...))))))

(define-syntax df
  (lambda (stx)
    (syntax-case stx (=>)
      ((df name (args ... => return-type) body ...)
       #`(define (name #,@(rewrite-lambda-list #'(args ...))) :: return-type
                 (seq body ...)))
      ((df name (args ...) body ...)
       #`(define (name #,@(rewrite-lambda-list #'(args ...)))
           (seq body ...))))))

(define-syntax fun
  (lambda (stx)
    (syntax-case stx (=>)
      ((fun (args ... => return-type) body ...)
       #`(lambda #,(rewrite-lambda-list #'(args ...)) :: return-type
                 (seq body ...)))
      ((fun (args ...) body ...)
       #`(lambda #,(rewrite-lambda-list #'(args ...))
           (seq body ...))))))

(define-syntax fin
  (syntax-rules ()
    ((fin body handler ...)
     (try-finally body (seq handler ...)))))

(define-syntax seq
  (syntax-rules ()
    ((seq)
     (begin #!void))
    ((seq body ...)
     (begin body ...))))

(define-syntax esc
  (syntax-rules ()
    ((esc abort body ...)
     (let* ((key (<symbol>))
            (abort (lambda (val) (throw key val))))
       (catch key
              (lambda () body ...)
              (lambda (key val) val))))))

(define-syntax !
  (syntax-rules ()
    ((! name obj args ...)
     (invoke obj 'name args ...))))

(define-syntax !!
  (syntax-rules ()
    ((!! name1 name2 obj args ...)
     (! name1 (! name2 obj args ...)))))

(define-syntax !s
  (syntax-rules ()
    ((! class name args ...)
     (invoke-static class 'name args ...))))

(define-syntax @
    (syntax-rules ()
      ((@ name obj)
       (field obj 'name))))

(define-syntax @s
  (syntax-rules (quote)
    ((@s class name)
     (static-field class (quote name)))))

(define-syntax while
  (syntax-rules ()
    ((while exp body ...)
     (do () ((not exp)) body ...))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (i n result) body ...)
     (let ((max :: <int> n))
       (do ((i :: <int> 0 (as <int> (+ i 1))))
           ((= i max) result)
         body ...)))
    ((dotimes (i n) body ...)
     (dotimes (i n #f) body ...))))

(define-syntax dolist
  (syntax-rules ()
    ((dolist (e list) body ... )
     (for ((e list)) body ...))))

(define-syntax for
  (syntax-rules ()
    ((for ((var iterable)) body ...)
     (let ((iter (! iterator iterable)))
       (while (! has-next iter)
         ((lambda (var) body ...)
          (! next iter)))))))

(define-syntax packing
  (syntax-rules ()
    ((packing (var) body ...)
     (let ((var :: <list> '()))
       (let ((var (lambda (v) (set! var (cons v var)))))
         body ...)
       (reverse! var)))))

;;(define-syntax loop
;;  (syntax-rules (for = then collect until)
;;    ((loop for var = init then step until test collect exp)
;;     (packing (pack)
;;       (do ((var init step))
;;           (test)
;;         (pack exp))))
;;    ((loop while test collect exp)
;;     (packing (pack) (while test (pack exp))))))

(define-syntax with
  (syntax-rules ()
    ((with (vars ... (f args ...)) body ...)
     (f args ... (lambda (vars ...) body ...)))))

(define-syntax pushf
  (syntax-rules ()
    ((pushf value var)
     (set! var (cons value var)))))

(define-syntax ==
  (syntax-rules ()
    ((== x y)
     (eq? x y))))

(define-syntax set
  (syntax-rules ()
    ((set x y)
     (let ((tmp y))
       (set! x tmp)
       tmp))
    ((set x y more ...)
     (begin (set! x y) (set more ...)))))

(define-syntax assert
  (syntax-rules ()
    ((assert test)
     (seq
       (when (not test)
         (error "Assertion failed" 'test))
       'ok))
    ((assert test fstring args ...)
     (seq
       (when (not test)
         (error "Assertion failed" 'test (format #f fstring args ...)))
       'ok))))

(define-syntax mif
  (syntax-rules (quote unquote _)
    ((mif ('x value) then else)
     (if (equal? 'x value) then else))
    ((mif (,x value) then else)
     (if (eq? x value) then else))
    ((mif (() value) then else)
     (if (eq? value '()) then else))
    #|  This variant produces no lambdas but breaks the compiler
    ((mif ((p . ps) value) then else)
     (let ((tmp value)
           (fail? :: <int> 0)
           (result #!null))
       (if (instance? tmp <pair>)
           (let ((tmp :: <pair> tmp))
             (mif (p (! get-car tmp))
                  (mif (ps (! get-cdr tmp))
                       (set! result then)
                       (set! fail? -1))
                  (set! fail? -1)))
           (set! fail? -1))
       (if (= fail? 0) result else)))
    |#
    ((mif ((p . ps) value) then else)
     (let ((fail (lambda () else))
           (tmp value))
       (if (instance? tmp <pair>)
           (let ((tmp :: <pair> tmp))
             (mif (p (! get-car tmp))
                  (mif (ps (! get-cdr tmp))
                       then
                       (fail))
                  (fail)))
           (fail))))
    ((mif (_ value) then else)
     then)
    ((mif (var value) then else)
     (let ((var value)) then))
    ((mif (pattern value) then)
     (mif (pattern value) then (values)))))

(define-syntax mcase
  (syntax-rules ()
    ((mcase exp (pattern body ...) more ...)
     (let ((tmp exp))
       (mif (pattern tmp)
            (begin body ...)
            (mcase tmp more ...))))
    ((mcase exp) (ferror "mcase failed ~s\n~a" 'exp exp))))

(define-syntax mlet
  (syntax-rules ()
    ((mlet (pattern value) body ...)
     (let ((tmp value))
       (mif (pattern tmp)
            (begin body ...)
            (error "mlet failed" tmp))))))

(define-syntax mlet*
  (syntax-rules ()
    ((mlet* () body ...) (begin body ...))
    ((mlet* ((pattern value) ms ...) body ...)
     (mlet (pattern value) (mlet* (ms ...) body ...)))))

(define-syntax typecase%
  (syntax-rules (eql or satisfies)
    ((typecase% var (#t body ...) more ...)
     (seq body ...))
    ((typecase% var ((eql value) body ...) more ...)
     (cond ((eqv? var 'value) body ...)
           (else (typecase% var more ...))))
    ((typecase% var ((satisfies predicate) body ...) more ...)
     (cond ((predicate var) body ...)
           (else (typecase% var more ...))))
    ((typecase% var ((or type) body ...) more ...)
     (typecase% var (type body ...) more ...))
    ((typecase% var ((or type ...) body ...) more ...)
     (let ((f (lambda (var) body ...)))
       (typecase% var
                  (type (f var)) ...
                  (#t (typecase% var more ...)))))
    ((typecase% var (type body ...) more ...)
     (cond ((instance? var type)
            (let ((var :: type (as type var)))
              body ...))
           (else (typecase% var more ...))))
    ((typecase% var)
     (error "typecase% failed" var
            (! getClass (as <object> var))))))

(define-syntax typecase
  (lambda (stx)
    (syntax-case stx ()
      ((_ exp more ...) (identifier? (syntax exp))
       #`(typecase% exp more ...))
      ((_ exp more ...)
       #`(let ((tmp exp))
           (typecase% tmp more ...))))))

(define-syntax ignore-errors
  (syntax-rules ()
    ((ignore-errors body ...)
     (try-catch (seq body ...)
                (v <java.lang.Error> #f)
                (v <java.lang.Exception> #f)))))

))

(define-library (swank-kawa)
    (export start-swank
            create-swank-server
            swank-java-source-path
            break)
    (import (scheme base)
            (scheme file)
            (scheme repl)
            (scheme read)
            (scheme write)
            (scheme eval)
            (scheme process-context)
            (swank macros)
            (only (kawa base)

                  define-alias
                  define-variable

                  define-simple-class
                  this

                  invoke-special
                  instance?
                  as

                  primitive-throw
                  try-finally
                  try-catch
                  synchronized

                  call-with-input-string
                  call-with-output-string
                  force-output
                  format

                  make-process
                  command-parse

                  runnable

                  scheme-implementation-version
                  reverse!
                  )
            (rnrs hashtables)
            (only (gnu kawa slib syntaxutils) expand)
            (only (kawa regex) regex-match))
  (begin "
("


;;(define-syntax dc
;;  (syntax-rules ()
;;    ((dc name () %% (props ...) prop more ...)
;;     (dc name () %% (props ... (prop <object>)) more ...))
;;    ;;((dc name () %% (props ...) (prop type) more ...)
;;    ;; (dc name () %% (props ... (prop type)) more ...))
;;    ((dc name () %% ((prop type) ...))
;;     (define-simple-class name ()
;;                          ((*init* (prop :: type) ...)
;;                           (set (field (this) 'prop) prop) ...)
;;                          (prop :type type) ...))
;;    ((dc name () props ...)
;;     (dc name () %% () props ...))))


;;;; Aliases

(define-alias <server-socket> java.net.ServerSocket)
(define-alias <socket> java.net.Socket)
(define-alias <in> java.io.InputStreamReader)
(define-alias <out> java.io.OutputStreamWriter)
(define-alias <in-port> gnu.kawa.io.InPort)
(define-alias <out-port> gnu.kawa.io.OutPort)
(define-alias <file> java.io.File)
(define-alias <str> java.lang.String)
(define-alias <builder> java.lang.StringBuilder)
(define-alias <throwable> java.lang.Throwable)
(define-alias <source-error> gnu.text.SourceError)
(define-alias <module-info> gnu.expr.ModuleInfo)
(define-alias <iterable> java.lang.Iterable)
(define-alias <thread> java.lang.Thread)
(define-alias <queue> java.util.concurrent.LinkedBlockingQueue)
(define-alias <exchanger> java.util.concurrent.Exchanger)
(define-alias <timeunit> java.util.concurrent.TimeUnit)
(define-alias <vm> com.sun.jdi.VirtualMachine)
(define-alias <mirror> com.sun.jdi.Mirror)
(define-alias <value> com.sun.jdi.Value)
(define-alias <thread-ref> com.sun.jdi.ThreadReference)
(define-alias <obj-ref> com.sun.jdi.ObjectReference)
(define-alias <array-ref> com.sun.jdi.ArrayReference)
(define-alias <str-ref> com.sun.jdi.StringReference)
(define-alias <meth-ref> com.sun.jdi.Method)
(define-alias <class-type> com.sun.jdi.ClassType)
(define-alias <ref-type> com.sun.jdi.ReferenceType)
(define-alias <frame> com.sun.jdi.StackFrame)
(define-alias <field> com.sun.jdi.Field)
(define-alias <local-var> com.sun.jdi.LocalVariable)
(define-alias <location> com.sun.jdi.Location)
(define-alias <absent-exc> com.sun.jdi.AbsentInformationException)
(define-alias <event> com.sun.jdi.event.Event)
(define-alias <exception-event> com.sun.jdi.event.ExceptionEvent)
(define-alias <step-event> com.sun.jdi.event.StepEvent)
(define-alias <breakpoint-event> com.sun.jdi.event.BreakpointEvent)
(define-alias <env> gnu.mapping.Environment)

(define-simple-class <chan> ()
  (owner :: <thread> #:init (!s java.lang.Thread currentThread))
  (peer :: <chan>)
  (queue :: <queue> #:init (<queue>))
  (lock #:init (<object>)))


;;;; Entry Points

(df create-swank-server (port-number)
  (setup-server port-number announce-port))

(df start-swank (port-file)
  (let ((announce (fun ((socket <server-socket>))
                    (with (f (call-with-output-file port-file))
                      (format f "~d\n" (! get-local-port socket))))))
    (spawn (fun ()
             (setup-server 0 announce)))))

(df setup-server ((port-number <int>) announce)
  (! set-name (current-thread) "swank")
  (let ((s (<server-socket> port-number)))
    (announce s)
    (let ((c (! accept s)))
      (! close s)
      (log "connection: ~s\n"  c)
      (fin (dispatch-events c)
        (log "closing socket: ~a\n" s)
        (! close c)))))

(df announce-port ((socket <server-socket>))
  (log "Listening on port: ~d\n" (! get-local-port socket)))


;;;; Event dispatcher

(define-variable *the-vm* #f)
(define-variable *last-exception* #f)
(define-variable *last-stacktrace* #f)
(df %vm (=> <vm>) *the-vm*)

;; FIXME: this needs factorization.  But I guess the whole idea of
;; using bidirectional channels just sucks.  Mailboxes owned by a
;; single thread to which everybody can send are much easier to use.

(df dispatch-events ((s <socket>))
  (mlet* ((charset "iso-8859-1")
          (ins (<in> (! getInputStream s) charset))
          (outs (<out> (! getOutputStream s) charset))
          ((in . _) (spawn/chan/catch (fun (c) (reader ins c))))
          ((out . _) (spawn/chan/catch (fun (c) (writer outs c))))
          ((dbg . _) (spawn/chan/catch vm-monitor))
          (user-env  (interaction-environment))
          (x (seq
               (! set-flag user-env #t #|<env>:THREAD_SAFE|# 8)
               (! set-flag user-env #f #|<env>:DIRECT_INHERITED_ON_SET|# 16)
               #f))
          ((listener . _)
           (spawn/chan (fun (c) (listener c user-env))))
          (inspector #f)
          (threads '())
          (repl-thread #f)
          (extra '())
          (vm (let ((vm #f)) (fun () (or vm (rpc dbg `(get-vm)))))))
    (while #t
      (mlet ((c . event) (recv* (append (list in out dbg listener)
                                        (if inspector (list inspector) '())
                                        (map car threads)
                                        extra)))
        ;;(log "event: ~s\n" event)
        (mcase (list c event)
          ((_ (':emacs-rex ('|swank:debugger-info-for-emacs| from to)
                           pkg thread id))
           (send dbg `(debug-info ,thread ,from ,to ,id)))
          ((_ (':emacs-rex ('|swank:throw-to-toplevel|) pkg thread id))
           (send dbg `(throw-to-toplevel ,thread ,id)))
          ((_ (':emacs-rex ('|swank:sldb-continue|) pkg thread id))
           (send dbg `(thread-continue ,thread ,id)))
          ((_ (':emacs-rex ('|swank:frame-source-location| frame)
                           pkg thread id))
           (send dbg `(frame-src-loc ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:frame-locals-and-catch-tags| frame)
                           pkg thread id))
           (send dbg `(frame-details ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:sldb-disassemble| frame)
                           pkg thread id))
           (send dbg `(disassemble-frame ,thread ,frame ,id)))
          ((_ (':emacs-rex ('|swank:backtrace| from to) pkg thread id))
           (send dbg `(thread-frames ,thread ,from ,to ,id)))
          ((_ (':emacs-rex ('|swank:list-threads|) pkg thread id))
           (send dbg `(list-threads ,id)))
          ((_ (':emacs-rex ('|swank:debug-nth-thread| n) _  _ _))
           (send dbg `(debug-nth-thread ,n)))
          ((_ (':emacs-rex ('|swank:quit-thread-browser|) _  _ id))
           (send dbg `(quit-thread-browser ,id)))
          ((_ (':emacs-rex ('|swank:init-inspector| str . _) pkg _ id))
           (set inspector (make-inspector user-env (vm)))
           (send inspector `(init ,str ,id)))
          ((_ (':emacs-rex ('|swank:inspect-frame-var| frame var)
                           pkg thread id))
           (mlet ((im . ex) (chan))
             (set inspector (make-inspector user-env (vm)))
             (send dbg `(get-local ,ex ,thread ,frame ,var))
             (send inspector `(init-mirror ,im ,id))))
          ((_ (':emacs-rex ('|swank:inspect-current-condition|) pkg thread id))
           (mlet ((im . ex) (chan))
             (set inspector (make-inspector user-env (vm)))
             (send dbg `(get-exception ,ex ,thread))
             (send inspector `(init-mirror ,im ,id))))
          ((_ (':emacs-rex ('|swank:inspect-nth-part| n) pkg _ id))
           (send inspector `(inspect-part ,n ,id)))
          ((_ (':emacs-rex ('|swank:inspector-pop|) pkg _ id))
           (send inspector `(pop ,id)))
          ((_ (':emacs-rex ('|swank:quit-inspector|) pkg _ id))
           (send inspector `(quit ,id)))
          ((_ (':emacs-interrupt id))
           (let* ((vm (vm))
                  (t (find-thread id (map cdr threads) repl-thread vm)))
             (send dbg `(interrupt-thread ,t))))
          ((_ (':emacs-rex form _ _ id))
           (send listener `(,form ,id)))
          ((_ ('get-vm c))
           (send dbg `(get-vm ,c)))
          ((_ ('get-channel c))
           (mlet ((im . ex) (chan))
             (pushf im extra)
             (send c ex)))
          ((_ ('forward x))
           (send out x))
          ((_ ('set-listener x))
           (set repl-thread x))
          ((_ ('publish-vm vm))
           (set *the-vm* vm))
          )))))

(df find-thread (id threads listener (vm <vm>))
  (cond ((== id ':repl-thread) listener)
        ((== id 't) listener
         ;;(if (null? threads)
         ;;    listener
         ;;    (vm-mirror vm (car threads)))
         )
        (#t
         (let ((f (find-if threads
                      (fun (t :: <thread>)
                        (= id (! uniqueID
                                 (as <thread-ref> (vm-mirror vm t)))))
                      #f)))
           (cond (f (vm-mirror vm f))
                 (#t listener))))))


;;;; Reader thread

(df reader ((in <in>) (c <chan>))
  (! set-name (current-thread) "swank-net-reader")
  (let ((rt (!s gnu.kawa.lispexpr.ReadTable createInitial))) ; ':' not special
    (while #t
      (send c (decode-message in rt)))))

(df decode-message ((in <in>) (rt  <gnu.kawa.lispexpr.ReadTable>) => <list>)
  (let* ((header (read-chunk in 6))
         (len (!s java.lang.Integer parseInt header 16)))
    (call-with-input-string (read-chunk in len)
                            (fun ((port <input-port>))
                              (%read port rt)))))

(df read-chunk ((in <in>) (len <int>) => <str>)
  (let ((chars (<char[]> #:length len)))
    (let loop ((offset :: <int> 0))
      (cond ((= offset len) (<str> chars))
            (#t (let ((count (! read in chars offset (- len offset))))
                  (assert (not (= count -1)) "partial packet")
                  (loop (+ offset count))))))))

;;; FIXME: not thread safe
(df %read ((port <in-port>) (table <gnu.kawa.lispexpr.ReadTable>))
  (let ((old (!s gnu.kawa.lispexpr.ReadTable getCurrent)))
    (try-finally
     (seq (!s gnu.kawa.lispexpr.ReadTable setCurrent table)
          (read port))
     (!s gnu.kawa.lispexpr.ReadTable setCurrent old))))


;;;; Writer thread

(df writer ((out <out>) (c <chan>))
  (! set-name (current-thread) "swank-net-writer")
  (while #t
    (encode-message out (recv c))))

(df encode-message ((out <out>) (message <list>))
  (let ((builder (<builder> (as <int> 512))))
    (print-for-emacs message builder)
    (! write out (! toString (format "~6,'0x" (! length builder))))
    (! write out builder)
    (! flush out)))

(df print-for-emacs (obj (out <builder>))
  (let ((pr (fun (o) (! append out (! toString (format "~s" o)))))
        (++ (fun ((s <string>)) (! append out (! toString s)))))
    (cond ((null? obj) (++ "nil"))
          ((string? obj) (pr obj))
          ((number? obj) (pr obj))
          ;;((keyword? obj) (++ ":") (! append out (to-str obj)))
          ((symbol? obj) (pr obj))
          ((pair? obj)
           (++ "(")
           (let loop ((obj obj))
             (print-for-emacs (car obj) out)
             (let ((cdr (cdr obj)))
               (cond ((null? cdr) (++ ")"))
                     ((pair? cdr) (++ " ") (loop cdr))
                     (#t (++ " . ") (print-for-emacs cdr out) (++ ")"))))))
          (#t (error "Unprintable object" obj)))))

;;;; SLIME-EVAL

(df eval-for-emacs ((form <list>) env (id <int>) (c <chan>))
  ;;(! set-uncaught-exception-handler (current-thread)
  ;;   (<ucex-handler> (fun (t e) (reply-abort c id))))
  (reply c (%eval form env) id))

(define-variable *slime-funs*)
(set *slime-funs* (tab))

(df %eval (form env)
  (apply (lookup-slimefun (car form) *slime-funs*) env (cdr form)))

(df lookup-slimefun ((name <symbol>) tab)
  ;; name looks like '|swank:connection-info|
  (or (get tab name #f)
      (ferror "~a not implemented" name)))

(df %defslimefun ((name <symbol>) (fun <procedure>))
  (let ((string (symbol->string name)))
    (cond ((regex-match #/:/ string)
           (put *slime-funs* name fun))
          (#t
           (let ((qname (string->symbol (string-append "swank:" string))))
             (put *slime-funs* qname fun))))))

(define-syntax defslimefun
  (syntax-rules ()
    ((defslimefun name (args ...) body ...)
     (seq
       (df name (args ...) body ...)
       (%defslimefun 'name name)))))

(defslimefun connection-info ((env <env>))
  (let ((prop (fun (name) (!s java.lang.System getProperty name))))
  `(:pid
    0
    :style :spawn
    :lisp-implementation (:type "Kawa" :name "kawa"
                                :version ,(scheme-implementation-version))
    :machine (:instance ,(prop "java.vm.name") :type ,(prop "os.name")
                        :version ,(prop "java.runtime.version"))
    :features ()
    :package (:name "??" :prompt ,(! getName env))
    :encoding (:coding-systems ("iso-8859-1"))
    )))


;;;; Listener

(df listener ((c <chan>) (env <env>))
  (! set-name (current-thread) "swank-listener")
  (log "listener: ~s ~s ~s ~s\n"
       (current-thread) (! hashCode (current-thread)) c env)
  (let ((out (make-swank-outport (rpc c `(get-channel)))))
    (set (current-output-port) out)
    (let ((vm (as <vm> (rpc c `(get-vm)))))
      (send c `(set-listener ,(vm-mirror vm (current-thread))))
      (request-uncaught-exception-events vm)
      ;;stack snaphost are too expensive
      ;;(request-caught-exception-events vm)
      )
    (rpc c `(get-vm))
    (listener-loop c env out)))

(define-simple-class <listener-abort> (<throwable>)
  ((*init*)
   (invoke-special <throwable> (this) '*init* ))
  ((abort) :: void
   (primitive-throw (this))))

(df listener-loop ((c <chan>) (env <env>) port)
  (while (not (nul? c))
    ;;(log "listener-loop: ~s ~s\n" (current-thread) c)
    (mlet ((form id) (recv c))
      (let ((restart (fun ()
                       (close-port port)
                       (reply-abort c id)
                       (send (car (spawn/chan
                                   (fun (cc)
                                     (listener (recv cc) env))))
                             c)
                       (set c #!null))))
        (! set-uncaught-exception-handler (current-thread)
           (<ucex-handler> (fun (t e) (restart))))
        (try-catch
         (let* ((val (%eval form env)))
           (force-output)
           (reply c val id))
         (ex <java.lang.Exception> (invoke-debugger ex) (restart))
         (ex <java.lang.Error> (invoke-debugger ex) (restart))
         (ex <listener-abort>
             (let ((flag (!s java.lang.Thread interrupted)))
               (log "listener-abort: ~s ~a\n" ex flag))
             (restart))
         )))))

(df invoke-debugger (condition)
  ;;(log "should now invoke debugger: ~a" condition)
  (try-catch
   (break condition)
   (ex <listener-abort> (seq))))

(defslimefun |swank-repl:create-repl| (env #!rest _)
  (list "user" "user"))

(defslimefun interactive-eval (env str)
  (values-for-echo-area (eval (read-from-string str) env)))

(defslimefun interactive-eval-region (env (s <string>))
  (with (port (call-with-input-string s))
    (values-for-echo-area
     (let next ((result (values)))
       (let ((form (read port)))
         (cond ((== form #!eof) result)
               (#t (next (eval form env)))))))))

(defslimefun |swank-repl:listener-eval| (env string)
  (let* ((form (read-from-string string))
         (list (values-to-list (eval form env))))
  `(:values ,@(map pprint-to-string list))))

(defslimefun pprint-eval (env string)
  (let* ((form (read-from-string string))
         (l (values-to-list (eval form env))))
    (apply cat (map pprint-to-string l))))

(defslimefun eval-and-grab-output (env string)
  (let ((form (read (open-input-string string))))
    (let-values ((values (eval form env)))
      (list ""
            (format #f "~{~S~^~%~}" values)))))

(df call-with-abort (f)
  (try-catch (f) (ex <throwable> (exception-message ex))))

(df exception-message ((ex <throwable>))
  (typecase ex
    (<kawa.lang.NamedException> (! to-string ex))
    (<throwable> (format "~a: ~a"
                         (class-name-sans-package ex)
                         (! getMessage ex)))))

(df values-for-echo-area (values)
  (let ((values (values-to-list values)))
    (cond ((null? values) "; No value")
          (#t (format "~{~a~^, ~}" (map pprint-to-string values))))))

;;;; Compilation

(defslimefun compile-file-for-emacs (env (filename <str>) load?
                                         #!optional options)
  (let ((jar (cat (path-sans-extension (filepath filename)) ".jar")))
    (wrap-compilation
     (fun ((m <gnu.text.SourceMessages>))
       (!s kawa.lang.CompileFile read filename m))
     jar (if (lisp-bool load?) env #f) #f)))

(df wrap-compilation (f jar env delete?)
  (let ((start-time (current-time))
        (messages (<gnu.text.SourceMessages>)))
    (try-catch
     (let ((c (as <gnu.expr.Compilation> (f messages))))
       (set (@ explicit c) #t)
       (! compile-to-archive c (! get-module c) jar))
     (ex <throwable>
         (log "error during compilation: ~a\n~a" ex (! getStackTrace ex))
         (! error messages (as <char> #\f)
            (to-str (exception-message ex)) #!null)
         #f))
    (log "compilation done.\n")
    (let ((success? (zero? (! get-error-count messages))))
      (when (and env success?)
        (log "loading ...\n")
        (eval `(load ,jar) env)
        (log "loading ... done.\n"))
      (when delete?
        (ignore-errors (delete-file jar) #f))
      (let ((end-time (current-time)))
        (list ':compilation-result
              (compiler-notes-for-emacs messages)
              (if success? 't 'nil)
              (/ (- end-time start-time) 1000.0))))))

(defslimefun compile-string-for-emacs (env string buffer offset dir)
  (wrap-compilation
   (fun ((m <gnu.text.SourceMessages>))
     (let ((c (as <gnu.expr.Compilation>
                  (call-with-input-string
                   string
                   (fun ((p <in-port>))
                     (! set-path p
                        (format "~s"
                                `(buffer ,buffer offset ,offset str ,string)))
                     (!s kawa.lang.CompileFile read p m))))))
       (let ((o (@ currentOptions c)))
         (! set o "warn-invoke-unknown-method" #t)
         (! set o "warn-undefined-variable" #t))
       (let ((m (! getModule c)))
         (! set-name m (format "<emacs>:~a/~a" buffer (current-time))))
       c))
   "/tmp/kawa-tmp.zip" env #t))

(df compiler-notes-for-emacs ((messages <gnu.text.SourceMessages>))
  (packing (pack)
    (do ((e (! get-errors messages) (@ next e)))
        ((nul? e))
      (pack (source-error>elisp e)))))

(df source-error>elisp ((e <source-error>) => <list>)
  (list ':message (to-string (@ message e))
        ':severity (case (integer->char (@ severity e))
                    ((#\e #\f) ':error)
                    ((#\w) ':warning)
                    (else ':note))
        ':location (error-loc>elisp e)))

(df error-loc>elisp ((e <source-error>))
  (cond ((nul? (@ filename e)) `(:error "No source location"))
        ((! starts-with (@ filename e) "(buffer ")
         (mlet (('buffer b 'offset ('quote ((:position o) _)) 'str s)
                (read-from-string (@ filename e)))
           (let ((off (line>offset (1- (@ line e)) s))
                 (col (1- (@ column e))))
             `(:location (:buffer ,b) (:position ,(+ o off col)) nil))))
        (#t
         `(:location (:file ,(to-string (@ filename e)))
                     (:line ,(@ line e) ,(1- (@ column e)))
                     nil))))

(df line>offset ((line <int>) (s <str>) => <int>)
  (let ((offset :: <int> 0))
    (dotimes (i line)
      (set offset (! index-of s (as <char> #\newline) offset))
      (assert (>= offset 0))
      (set offset (as <int> (+ offset 1))))
    (log "line=~a offset=~a\n" line offset)
    offset))

(defslimefun load-file (env filename)
  (format "Loaded: ~a => ~s" filename (eval `(load ,filename) env)))

;;;; Completion

(defslimefun simple-completions (env (pattern <str>) _)
  (let* ((env (as <gnu.mapping.InheritingEnvironment> env))
         (matches (packing (pack)
                    (let ((iter (! enumerate-all-locations env)))
                      (while (! has-next iter)
                        (let ((l (! next-location iter)))
                          (typecase l
                            (<gnu.mapping.NamedLocation>
                             (let ((name (!! get-name get-key-symbol l)))
                               (when (! starts-with name pattern)
                                 (pack name)))))))))))
    `(,matches ,(cond ((null? matches) pattern)
                      (#t (fold+ common-prefix matches))))))

(df common-prefix ((s1 <str>) (s2 <str>) => <str>)
  (let ((limit (min (! length s1) (! length s2))))
    (let loop ((i 0))
      (cond ((or (= i limit)
                 (not (== (! char-at s1 i)
                          (! char-at s2 i))))
             (! substring s1 0 i))
            (#t (loop (1+ i)))))))

(df fold+ (f list)
  (let loop ((s (car list))
             (l (cdr list)))
    (cond ((null? l) s)
          (#t (loop (f s (car l)) (cdr l))))))

;;; Quit

(defslimefun quit-lisp (env)
  (exit))

;;(defslimefun set-default-directory (env newdir))


;;;; Dummy defs

(defslimefun buffer-first-change (#!rest y) '())
(defslimefun swank-require (#!rest y) '())
(defslimefun frame-package-name (#!rest y) '())

;;;; arglist

(defslimefun operator-arglist (env name #!rest _)
  (mcase (try-catch `(ok ,(eval (read-from-string name) env))
                    (ex <throwable> 'nil))
    (('ok obj)
     (mcase (arglist obj)
       ('#f 'nil)
       ((args rtype)
        (format "(~a~{~^ ~a~})~a" name
                (map (fun (e)
                       (if (equal (cadr e) "java.lang.Object") (car e) e))
                     args)
                (if (equal rtype "java.lang.Object")
                    ""
                    (format " => ~a" rtype))))))
    (_ 'nil)))

(df arglist (obj)
  (typecase obj
    (<gnu.expr.ModuleMethod>
     (let* ((mref (module-method>meth-ref obj)))
       (list (mapi (! arguments mref)
                   (fun ((v <local-var>))
                     (list (! name v) (! typeName v))))
             (! returnTypeName mref))))
    (<object> #f)))

;;;; M-.

(defslimefun find-definitions-for-emacs (env name)
  (mcase (try-catch `(ok ,(eval (read-from-string name) env))
                    (ex <throwable> `(error ,(exception-message ex))))
    (('ok obj) (mapi (all-definitions obj)
                     (fun (d)
                       `(,(format "~a" d) ,(src-loc>elisp (src-loc d))))))
    (('error msg) `((,name (:error ,msg))))))

(define-simple-class <swank-location> (<location>)
  (file #:init #f)
  (line #:init #f)
  ((*init* file name)
   (set (@ file (this)) file)
   (set (@ line (this)) line))
  ((lineNumber) :: <int> (or line (absent)))
  ((lineNumber (s :: <str>)) :: int (! lineNumber (this)))
  ((method) :: <meth-ref> (absent))
  ((sourcePath) :: <str> (or file (absent)))
  ((sourcePath (s :: <str>)) :: <str> (! sourcePath (this)))
  ((sourceName) :: <str> (absent))
  ((sourceName (s :: <str>)) :: <str> (! sourceName (this)))
  ((declaringType) :: <ref-type> (absent))
  ((codeIndex) :: <long> -1)
  ((virtualMachine) :: <vm> *the-vm*)
  ((compareTo o) :: <int>
   (typecase o
     (<location> (- (! codeIndex (this)) (! codeIndex o))))))

(df absent () (primitive-throw (<absent-exc>)))

(df all-definitions (o)
  (typecase o
    (<gnu.expr.ModuleMethod> (list o))
    (<gnu.expr.PrimProcedure> (list o))
    (<gnu.expr.GenericProc> (append (mappend all-definitions (gf-methods o))
                                    (let ((s (! get-setter o)))
                                      (if s (all-definitions s) '()))))
    (<java.lang.Class> (list o))
    (<gnu.mapping.Procedure> (all-definitions (! get-class o)))
    (<kawa.lang.Macro> (list o))
    (<gnu.bytecode.ObjectType> (all-definitions (! getReflectClass o)))
    (<java.lang.Object> '())
    ))

(df gf-methods ((f <gnu.expr.GenericProc>))
  (let* ((o :: <obj-ref> (vm-mirror *the-vm* f))
         (f (! field-by-name (! reference-type o) "methods"))
         (ms (vm-demirror *the-vm* (! get-value o f))))
    (filter (array-to-list ms) (fun (x) (not (nul? x))))))

(df src-loc (o => <location>)
  (typecase o
    (<gnu.expr.PrimProcedure> (src-loc (@ method o)))
    (<gnu.expr.ModuleMethod> (module-method>src-loc o))
    (<gnu.expr.GenericProc> (<swank-location> #f #f))
    (<java.lang.Class> (class>src-loc o))
    (<kawa.lang.Macro> (<swank-location> #f #f))
    (<gnu.bytecode.Method> (bytemethod>src-loc o))))

(df module-method>src-loc ((f <gnu.expr.ModuleMethod>))
  (! location (module-method>meth-ref f)))

(df module-method>meth-ref ((f <gnu.expr.ModuleMethod>) => <meth-ref>)
  (let* ((module (! reference-type
                    (as <obj-ref> (vm-mirror *the-vm* (@ module f)))))
         (1st-method-by-name (fun (name)
                               (let ((i (! methods-by-name module name)))
                                 (cond ((! is-empty i) #f)
                                       (#t (1st i)))))))
    (as <meth-ref> (or (1st-method-by-name (! get-name f))
                       (let ((mangled (mangled-name f)))
                         (or (1st-method-by-name mangled)
                             (1st-method-by-name (cat mangled "$V"))
                             (1st-method-by-name (cat mangled "$X"))))))))

(df mangled-name ((f <gnu.expr.ModuleMethod>))
  (let* ((name0 (! get-name f))
         (name (cond ((nul? name0) (format "lambda~d" (@ selector f)))
                     (#t (!s gnu.expr.Compilation mangleName name0)))))
    name))

(df class>src-loc ((c <java.lang.Class>) => <location>)
  (let* ((type (class>ref-type c))
         (locs (! all-line-locations type)))
    (cond ((not (! isEmpty locs)) (1st locs))
          (#t (<swank-location> (1st (! source-paths type "Java"))
                                #f)))))

(df class>ref-type ((class <java.lang.Class>) => <ref-type>)
  (! reflectedType (as <com.sun.jdi.ClassObjectReference>
                       (vm-mirror *the-vm* class))))

(df class>class-type ((class <java.lang.Class>) => <class-type>)
  (as <class-type> (class>ref-type class)))

(df bytemethod>src-loc ((m <gnu.bytecode.Method>) => <location>)
  (let* ((cls (class>class-type (! get-reflect-class
                                   (! get-declaring-class m))))
         (name (! get-name m))
         (sig (! get-signature m))
         (meth (! concrete-method-by-name cls name sig)))
    (! location meth)))

(df src-loc>elisp ((l <location>))
  (df src-loc>list ((l <location>))
    (list (ignore-errors (! source-name l "Java"))
          (ignore-errors (! source-path l "Java"))
          (ignore-errors (! line-number l "Java"))))
  (mcase (src-loc>list l)
    ((name path line)
     (cond ((not path)
            `(:error ,(call-with-abort (fun () (! source-path l)))))
           ((! starts-with (as <str> path) "(buffer ")
            (mlet (('buffer b 'offset o 'str s) (read-from-string path))
              `(:location (:buffer ,b)
                          (:position ,(+ o (line>offset line s)))
                          nil)))
           (#t
            `(:location ,(or (find-file-in-path name (source-path))
                             (find-file-in-path path (source-path))
                             (ferror "Can't find source-path: ~s ~s ~a"
                                     path name (source-path)))
                        (:line ,(or line -1)) ()))))))

(df src-loc>str ((l <location>))
  (cond ((nul? l) "<null-location>")
        (#t (format "~a ~a ~a"
                    (or (ignore-errors (! source-path l))
                        (ignore-errors (! source-name l))
                        (ignore-errors (!! name declaring-type l)))
                    (ignore-errors (!! name method l))
                    (ignore-errors (! lineNumber l))))))

;;;;;; class-path hacking

;; (find-file-in-path "kawa/lib/kawa/hashtable.scm" (source-path))

(df find-file-in-path ((filename <str>) (path <list>))
  (let ((f (<file> filename)))
    (cond ((! isAbsolute f) `(:file ,filename))
          (#t (let ((result #f))
                (find-if path (fun (dir)
                                (let ((x (find-file-in-dir f dir)))
                                  (set result x)))
                         #f)
                result)))))

(df find-file-in-dir ((file <file>) (dir <str>))
  (let ((filename :: <str> (! getPath file)))
    (or (let ((child (<file> (<file> dir) filename)))
          (and (! exists child)
               `(:file ,(! getPath child))))
        (try-catch
         (and (not (nul? (! getEntry (<java.util.zip.ZipFile> dir) filename)))
              `(:zip ,dir ,filename))
         (ex <throwable> #f)))))

(define swank-java-source-path
  (let* ((jre-home :: <str> (!s <java.lang.System> getProperty "java.home"))
         (parent :: <str> (! get-parent (<file> jre-home))))
    (list (! get-path (<file> parent "src.zip")))))

(df source-path ()
  (mlet ((base) (search-path-prop "user.dir"))
    (append
     (list base)
     (map (fun ((s <str>))
            (let ((f (<file> s))
                  (base :: <str> (as <str> base)))
               (cond ((! isAbsolute f) s)
                     (#t (! getPath (<file> base s))))))
          (class-path))
     swank-java-source-path)))

(df class-path ()
  (append (search-path-prop "java.class.path")
          (search-path-prop "sun.boot.class.path")))

(df search-path-prop ((name <str>))
  (array-to-list (! split (!s java.lang.System getProperty name)
                    (@s <file> pathSeparator))))

;;;; Disassemble

(defslimefun disassemble-form (env form)
  (mcase (read-from-string form)
    (('quote name)
     (let ((f (eval name env)))
       (typecase f
         (<gnu.expr.ModuleMethod>
          (disassemble-to-string (module-method>meth-ref f))))))))

(df disassemble-to-string ((mr <meth-ref>) => <str>)
  (with-sink #f (fun (out) (disassemble-meth-ref mr out))))

(df disassemble-meth-ref ((mr <meth-ref>) (out <java.io.PrintWriter>))
  (let* ((t (! declaring-type mr)))
    (disas-header mr out)
    (disas-code (! constant-pool t)
                (! constant-pool-count t)
                (! bytecodes mr)
                out)))

(df disas-header ((mr <meth-ref>) (out <java.io.PrintWriter>))
  (let* ((++ (fun ((str <str>)) (! write out str)))
         (? (fun (flag str) (if flag (++ str)))))
    (? (! is-static mr) "static ")
    (? (! is-final mr) "final ")
    (? (! is-private mr) "private ")
    (? (! is-protected mr) "protected ")
    (? (! is-public mr) "public ")
    (++ (! name mr)) (++ (! signature mr)) (++ "\n")))

(df disas-code ((cpool <byte[]>) (cpoolcount <int>) (bytecode <byte[]>)
                (out <java.io.PrintWriter>))
  (let* ((ct (<gnu.bytecode.ClassType> "foo"))
         (met (! addMethod ct "bar" 0))
         (ca (<gnu.bytecode.CodeAttr> met))
         (constants (let* ((bs (<java.io.ByteArrayOutputStream>))
                           (s (<java.io.DataOutputStream> bs)))
                      (! write-short s cpoolcount)
                      (! write s cpool)
                      (! flush s)
                      (! toByteArray bs))))
    (vm-set-slot *the-vm* ct "constants"
                 (<gnu.bytecode.ConstantPool>
                  (<java.io.DataInputStream>
                   (<java.io.ByteArrayInputStream>
                    constants))))
    (! setCode ca bytecode)
    (let ((w (<gnu.bytecode.ClassTypeWriter> ct out 0)))
      (! print ca w)
      (! flush w))))

(df with-sink (sink (f <function>))
  (cond ((instance? sink <java.io.PrintWriter>) (f sink))
        ((== sink #t) (f (as <java.io.PrintWriter> (current-output-port))))
        ((== sink #f)
         (let* ((buffer (<java.io.StringWriter>))
                (out (<java.io.PrintWriter> buffer)))
           (f out)
           (! flush out)
           (! toString buffer)))
        (#t (ferror "Invalid sink designator: ~s" sink))))

(df test-disas ((c <str>) (m <str>))
  (let* ((vm (as <vm> *the-vm*))
         (c (as <ref-type> (1st (! classes-by-name vm c))))
         (m (as <meth-ref> (1st (! methods-by-name c m)))))
    (with-sink #f (fun (out) (disassemble-meth-ref m out)))))

;; (test-disas "java.lang.Class" "toString")


;;;; Macroexpansion

(defslimefun swank-expand-1 (env s) (%swank-macroexpand s env))
(defslimefun swank-expand (env s) (%swank-macroexpand s env))
(defslimefun swank-expand-all (env s) (%swank-macroexpand s env))

(df %swank-macroexpand (string env)
  (pprint-to-string (%macroexpand (read-from-string string) env)))

(df %macroexpand (sexp env) (expand sexp #:env env))


;;;; Inspector

(define-simple-class <inspector-state> ()
  (object #:init #!null)
  (parts :: <java.util.ArrayList> #:init (<java.util.ArrayList>) )
  (stack :: <list> #:init '())
  (content :: <list> #:init '()))

(df make-inspector (env (vm <vm>) => <chan>)
  (car (spawn/chan (fun (c) (inspector c env vm)))))

(df inspector ((c <chan>) env (vm <vm>))
  (! set-name (current-thread) "inspector")
  (let ((state :: <inspector-state> (<inspector-state>))
        (open #t))
    (while open
      (mcase (recv c)
        (('init str id)
         (set state (<inspector-state>))
         (let ((obj (try-catch (eval (read-from-string str) env)
                               (ex <throwable> ex))))
           (reply c (inspect-object obj state vm) id)))
        (('init-mirror cc id)
         (set state (<inspector-state>))
         (let* ((mirror (recv cc))
                (obj (vm-demirror vm mirror)))
           (reply c (inspect-object obj state vm) id)))
        (('inspect-part n id)
         (let ((part (! get (@ parts state) n)))
           (reply c (inspect-object part state vm) id)))
        (('pop id)
         (reply c (inspector-pop state vm) id))
        (('quit id)
         (reply c 'nil id)
         (set open #f))))))

(df inspect-object (obj (state <inspector-state>) (vm <vm>))
  (set (@ object state) obj)
  (set (@ parts state) (<java.util.ArrayList>))
  (pushf obj (@ stack state))
  (set (@ content state) (inspector-content
                          `("class: " (:value ,(! getClass obj)) "\n"
                            ,@(inspect obj vm))
                          state))
  (cond ((nul? obj) (list ':title "#!null" ':id 0 ':content `()))
        (#t
         (list ':title (pprint-to-string obj)
               ':id (assign-index obj state)
               ':content (let ((c (@ content state)))
                           (content-range  c 0 (len c)))))))

(df inspect (obj vm)
  (let ((obj (as <obj-ref> (vm-mirror vm obj))))
    (typecase obj
      (<array-ref> (inspect-array-ref vm obj))
      (<obj-ref> (inspect-obj-ref vm obj)))))

(df inspect-array-ref ((vm <vm>) (obj <array-ref>))
  (packing (pack)
    (let ((i 0))
      (for (((v :: <value>) (! getValues obj)))
        (pack (format "~d: " i))
        (pack `(:value ,(vm-demirror vm v)))
        (pack "\n")
        (set i (1+ i))))))

(df inspect-obj-ref ((vm <vm>) (obj <obj-ref>))
  (let* ((type (! referenceType obj))
         (fields (! allFields type))
         (values (! getValues obj fields))
         (ifields '()) (sfields '()) (imeths '()) (smeths '())
         (frob (lambda (lists) (apply append (reverse lists)))))
    (for (((f :: <field>) fields))
      (let* ((val (as <value> (! get values f)))
             (l `(,(! name f) ": " (:value ,(vm-demirror vm val)) "\n")))
        (if (! is-static f)
            (pushf l sfields)
            (pushf l ifields))))
    (for (((m :: <meth-ref>) (! allMethods type)))
      (let ((l `(,(! name m) ,(! signature m) "\n")))
        (if (! is-static m)
            (pushf l smeths)
            (pushf l imeths))))
    `(,@(frob ifields)
      "--- static fields ---\n" ,@(frob sfields)
      "--- methods ---\n" ,@(frob imeths)
      "--- static methods ---\n" ,@(frob smeths))))

(df inspector-content (content (state <inspector-state>))
  (map (fun (part)
         (mcase part
           ((':value val)
            `(:value ,(pprint-to-string val) ,(assign-index val state)))
           (x (to-string x))))
       content))

(df assign-index (obj (state <inspector-state>) => <int>)
  (! add (@ parts state) obj)
  (1- (! size  (@ parts state))))

(df content-range (l start end)
  (let* ((len (length l)) (end (min len end)))
    (list (subseq l start end) len start end)))

(df inspector-pop ((state <inspector-state>) vm)
  (cond ((<= 2 (len (@ stack state)))
         (let ((obj (cadr (@ stack state))))
           (set (@ stack state) (cddr (@ stack state)))
           (inspect-object obj state vm)))
        (#t 'nil)))

;;;; IO redirection

(define-simple-class <swank-writer> (<java.io.Writer>)
  (q :: <queue> #:init (<queue> (as <int> 100)))
  ((*init*) (invoke-special <java.io.Writer> (this) '*init*))
  ((write (buffer :: <char[]>) (from :: <int>) (to :: <int>)) :: <void>
   (synchronized (this)
     (assert (not (== q #!null)))
     (! put q `(write ,(<str> buffer from to)))))
  ((close) :: <void>
   (synchronized (this)
     (! put q 'close)
     (set! q #!null)))
  ((flush) :: <void>
   (synchronized (this)
     (assert (not (== q #!null)))
     (let ((ex (<exchanger>)))
       (! put q `(flush ,ex))
       (! exchange ex #!null)))))

(df swank-writer ((in <chan>) (q <queue>))
  (! set-name (current-thread) "swank-redirect-thread")
  (let* ((out (as <chan> (recv in)))
         (builder (<builder>))
         (flush (fun ()
                  (unless (zero? (! length builder))
                    (send out `(forward (:write-string ,(<str> builder))))
                    (! setLength builder 0))))
         (closed #f))
    (while (not closed)
      (mcase (! poll q (as long 200) (@s <timeunit> MILLISECONDS))
        ('#!null (flush))
        (('write s)
         (! append builder (as <str> s))
         (when (> (! length builder) 4000)
           (flush)))
        (('flush ex)
         (flush)
         (! exchange (as <exchanger> ex) #!null))
        ('close
         (set closed #t)
         (flush))))))

(df make-swank-outport ((out <chan>))
  (let ((w (<swank-writer>)))
    (mlet ((in . _) (spawn/chan (fun (c) (swank-writer c (@ q w)))))
      (send in out))
    (<out-port> w  #t #t)))


;;;; Monitor

;;(define-simple-class <monitorstate> ()
;;  (threadmap type: (tab)))

(df vm-monitor ((c <chan>))
  (! set-name (current-thread) "swank-vm-monitor")
  (let ((vm (vm-attach)))
    (log-vm-props vm)
    (request-breakpoint vm)
    (mlet* (((ev . _) (spawn/chan/catch
                       (fun (c)
                         (let ((q (! eventQueue vm)))
                           (while #t
                             (send c `(vm-event ,(to-list (! remove q)))))))))
            (to-string (vm-to-string vm))
            (state (tab)))
      (send c `(publish-vm ,vm))
      (while #t
        (mcase (recv* (list c ev))
          ((_ . ('get-vm cc))
           (send cc vm))
          ((,c . ('debug-info thread from to id))
           (reply c (debug-info thread from to state) id))
          ((,c . ('throw-to-toplevel thread id))
           (set state (throw-to-toplevel thread id c state)))
          ((,c . ('thread-continue thread id))
           (set state (thread-continue thread id c state)))
          ((,c . ('frame-src-loc thread frame id))
           (reply c (frame-src-loc thread frame state) id))
          ((,c . ('frame-details thread frame id))
           (reply c (list (frame-locals thread frame state) '()) id))
          ((,c . ('disassemble-frame thread frame id))
           (reply c (disassemble-frame thread frame state) id))
          ((,c . ('thread-frames thread from to id))
           (reply c (thread-frames thread from to state) id))
          ((,c . ('list-threads id))
           (reply c (list-threads vm state) id))
          ((,c . ('interrupt-thread ref))
           (set state (interrupt-thread ref state c)))
          ((,c . ('debug-nth-thread n))
           (let ((t (nth (get state 'all-threads #f) n)))
             ;;(log "thread ~d : ~a\n" n t)
             (set state (interrupt-thread t state c))))
          ((,c . ('quit-thread-browser id))
           (reply c 't id)
           (set state (del state 'all-threads)))
          ((,ev . ('vm-event es))
           ;;(log "vm-events: len=~a\n" (len es))
           (for (((e :: <event>) (as <list> es)))
             (set state (process-vm-event e c state))))
          ((_ . ('get-exception from tid))
           (mlet ((_ _ es) (get state tid #f))
             (send from (let ((e (car es)))
                          (typecase e
                            (<exception-event> (! exception e))
                            (<event> e))))))
          ((_ . ('get-local rc tid frame var))
           (send rc (frame-local-var tid frame var state)))
          )))))

(df reply ((c <chan>) value id)
  (send c `(forward (:return (:ok ,value) ,id))))

(df reply-abort ((c <chan>) id)
  (send c `(forward (:return (:abort nil) ,id))))

(df process-vm-event ((e <event>) (c <chan>) state)
  ;;(log "vm-event: ~s\n" e)
  (typecase e
    (<exception-event>
     ;;(log "exception: ~s\n" (! exception e))
     ;;(log "exception-message: ~s\n"
     ;;     (exception-message (vm-demirror *the-vm* (! exception e))))
     ;;(log "exception-location: ~s\n" (src-loc>str (! location e)))
     ;;(log "exception-catch-location: ~s\n" (src-loc>str (! catch-location e)))
     (cond ((! notifyUncaught (as <com.sun.jdi.request.ExceptionRequest>
                                  (! request e)))
            (process-exception e c state))
           (#t
            (let* ((t (! thread e))
                   (r (! request e))
                   (ex (! exception e)))
              (unless (eq? *last-exception* ex)
                (set *last-exception* ex)
                (set *last-stacktrace*  (copy-stack t)))
              (! resume t))
            state)))
    (<step-event>
     (let* ((r (! request e))
            (k (! get-property r 'continuation)))
       (! disable r)
       (log "k: ~s\n" k)
       (k e))
     state)
    (<breakpoint-event>
     (log "breakpoint event: ~a\n" e)
     (debug-thread (! thread e) e state c))
    ))

(df process-exception ((e <exception-event>) (c <chan>) state)
    (let* ((tref (! thread e))
           (tid (! uniqueID tref))
           (s (get state tid #f)))
      (mcase s
        ('#f
         ;; XXX redundant in debug-thread
         (let* ((level 1)
                (state (put state tid (list tref level (list e)))))
           (send c `(forward (:debug ,tid ,level
                                     ,@(debug-info tid 0 15 state))))
           (send c `(forward (:debug-activate ,tid ,level)))
           state))
        ((_ level exs)
         (send c `(forward (:debug-activate ,(! uniqueID tref) ,level)))
         (put state tid (list tref (1+ level) (cons e exs)))))))

(define-simple-class <faked-frame> ()
  (loc :: <location>)
  (args)
  (names)
  (values :: <java.util.Map>)
  (self)
  ((*init* (loc :: <location>) args names (values :: <java.util.Map>) self)
   (set (@ loc (this)) loc)
   (set (@ args (this)) args)
   (set (@ names (this)) names)
   (set (@ values (this)) values)
   (set (@ self (this)) self))
  ((toString) :: <str>
   (format "#<ff ~a>" (src-loc>str loc))))

(df copy-stack ((t <thread-ref>))
  (packing (pack)
    (iter (! frames t)
          (fun ((f <frame>))
            (let ((vars (ignore-errors (! visibleVariables f))))
              (pack (<faked-frame>
                     (or (ignore-errors (! location f)) #!null)
                     (ignore-errors (! getArgumentValues f))
                     (or vars #!null)
                     (or (and vars (ignore-errors (! get-values f vars)))
                         #!null)
                     (ignore-errors (! thisObject f)))))))))

(define-simple-class <interrupt-event> (<event>)
  (thread :: <thread-ref>)
  ((*init* (thread :: <thread-ref>)) (set (@ thread (this)) thread))
  ((request) :: <com.sun.jdi.request.EventRequest> #!null)
  ((virtualMachine) :: <vm> (! virtualMachine thread)))

(df break (#!optional condition)
  ((breakpoint condition)))

;; We set a breakpoint on this function.  It returns a function which
;; specifies what the debuggee should do next (the actual return value
;; is set via JDI).  Lets hope that the compiler doesn't optimize this
;; away.
(df breakpoint (condition => <function>)
  (fun () #!null))

;; Enable breakpoints event on the breakpoint function.
(df request-breakpoint ((vm <vm>))
  (let* ((swank-classes (! classesByName vm "swank-kawa"))
         (swank-classes-legacy (! classesByName vm "swank$Mnkawa"))
         (class :: <class-type> (1st (if (= (length swank-classes) 0)
                                         swank-classes-legacy
                                         swank-classes)))
         (meth :: <meth-ref> (1st (! methodsByName class "breakpoint")))
         (erm (! eventRequestManager vm))
         (req (! createBreakpointRequest erm (! location meth))))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! put-property req 'swank #t)
    (! put-property req 'argname "condition")
    (! enable req)))

(df log-vm-props ((vm <vm>))
  (letrec-syntax ((p (syntax-rules ()
                       ((p name) (log "~s: ~s\n" 'name (! name vm)))))
                  (p* (syntax-rules ()
                        ((p* n ...) (seq (p n) ...)))))
    (p* canBeModified
        canRedefineClasses
        canAddMethod
        canUnrestrictedlyRedefineClasses
        canGetBytecodes
        canGetConstantPool
        canGetSyntheticAttribute
        canGetSourceDebugExtension
        canPopFrames
        canForceEarlyReturn
        canGetMethodReturnValues
        canGetInstanceInfo
        )))

;;;;; Debugger

(df debug-thread ((tref <thread-ref>) (ev <event>) state (c <chan>))
  (unless (! is-suspended tref)
    (! suspend tref))
  (let* ((id (! uniqueID tref))
         (level 1)
         (state (put state id (list tref level (list ev)))))
    (send c `(forward (:debug ,id ,level ,@(debug-info id 0 10 state))))
    (send c `(forward (:debug-activate ,id ,level)))
    state))

(df interrupt-thread ((tref <thread-ref>) state (c <chan>))
  (debug-thread tref (<interrupt-event> tref) state c))

(df debug-info ((tid <int>) (from <int>) to state)
  (mlet ((thread-ref level evs) (get state tid #f))
    (let* ((tref (as <thread-ref> thread-ref))
           (vm (! virtualMachine tref))
           (ev (as <event> (car evs)))
           (ex (typecase ev
                 (<breakpoint-event> (breakpoint-condition ev))
                 (<exception-event> (! exception ev))
                 (<interrupt-event> (<java.lang.Exception> "Interrupt"))))
           (desc (typecase ex
                   (<obj-ref>
                    ;;(log "ex: ~a ~a\n" ex (vm-demirror vm ex))
                    (! toString (vm-demirror vm ex)))
                   (<java.lang.Throwable> (! toString ex))))
           (type (format "  [type ~a]"
                         (typecase ex
                           (<obj-ref> (! name (! referenceType ex)))
                           (<object> (!! getName getClass ex)))))
           (bt (thread-frames tid from to state)))
      `((,desc ,type nil) (("quit" "terminate current thread")) ,bt ()))))

(df breakpoint-condition ((e <breakpoint-event>) => <obj-ref>)
  (let ((frame (! frame (! thread e) 0)))
    (1st (! get-argument-values frame))))

(df thread-frames ((tid <int>) (from <int>) to state)
  (mlet ((thread level evs) (get state tid #f))
    (let* ((thread (as <thread-ref> thread))
           (fcount (! frameCount thread))
           (stacktrace (event-stacktrace (car evs)))
           (missing (cond ((zero? (len stacktrace)) 0)
                          (#t (- (len stacktrace) fcount))))
           (fstart (max (- from missing) 0))
           (flen (max (- to from missing) 0))
           (frames (! frames thread fstart (min flen (- fcount fstart)))))
      (packing (pack)
        (let ((i from))
          (dotimes (_ (max (- missing from) 0))
            (pack (list i (format "~a" (stacktrace i))))
            (set i (1+ i)))
          (iter frames (fun ((f <frame>))
                         (let ((s (frame-to-string f)))
                           (pack (list i s))
                           (set i (1+ i))))))))))

(df event-stacktrace ((ev <event>))
  (let ((nothing (fun () (<java.lang.StackTraceElement[]>)))
        (vm (! virtualMachine ev)))
    (typecase ev
      (<breakpoint-event>
       (let ((condition (vm-demirror vm (breakpoint-condition ev))))
         (cond ((instance? condition <throwable>)
                (throwable-stacktrace vm condition))
               (#t (nothing)))))
      (<exception-event>
       (throwable-stacktrace vm (vm-demirror vm (! exception ev))))
      (<event> (nothing)))))

(df throwable-stacktrace ((vm <vm>) (ex <throwable>))
  (cond ((== ex (ignore-errors (vm-demirror vm *last-exception*)))
         *last-stacktrace*)
        (#t
         (! getStackTrace ex))))

(df frame-to-string ((f <frame>))
  (let ((loc (! location f))
        (vm (! virtualMachine f)))
    (format "~a (~a)" (!! name method loc)
            (call-with-abort
             (fun () (format "~{~a~^ ~}"
                             (mapi (! getArgumentValues f)
                                   (fun (arg)
                                     (pprint-to-string
                                      (vm-demirror vm arg))))))))))

(df frame-src-loc ((tid <int>) (n <int>) state)
  (try-catch
   (mlet* (((frame vm) (nth-frame tid n state))
           (vm (as <vm> vm)))
     (src-loc>elisp
      (typecase frame
        (<frame> (! location frame))
        (<faked-frame> (@ loc frame))
        (<java.lang.StackTraceElement>
         (let* ((classname (! getClassName frame))
                (classes (! classesByName vm classname))
                (t (as <ref-type> (1st classes))))
           (1st (! locationsOfLine t (! getLineNumber frame))))))))
   (ex <throwable>
       (let ((msg (! getMessage ex)))
         `(:error ,(if (== msg #!null)
                       (! toString ex)
                       msg))))))

(df nth-frame ((tid <int>) (n <int>) state)
  (mlet ((tref level evs) (get state tid #f))
    (let* ((thread (as <thread-ref> tref))
           (fcount (! frameCount thread))
           (stacktrace (event-stacktrace (car evs)))
           (missing (cond ((zero? (len stacktrace)) 0)
                          (#t (- (len stacktrace) fcount))))
           (vm (! virtualMachine thread))
           (frame (cond ((< n missing)
                         (stacktrace n))
                        (#t (! frame thread (- n missing))))))
      (list frame vm))))

;;;;; Locals

(df frame-locals ((tid <int>) (n <int>) state)
  (mlet ((thread _ _) (get state tid #f))
    (let* ((thread (as <thread-ref> thread))
           (vm (! virtualMachine thread))
           (p (fun (x) (pprint-to-string
                        (call-with-abort (fun () (vm-demirror vm x)))))))
      (map (fun (x)
             (mlet ((name value) x)
               (list ':name name ':value (p value) ':id 0)))
           (%frame-locals tid n state)))))

(df frame-local-var ((tid <int>) (frame <int>) (var <int>) state => <mirror>)
  (cadr (nth (%frame-locals tid frame state) var)))

(df %frame-locals ((tid <int>) (n <int>) state)
  (mlet ((frame _) (nth-frame tid n state))
    (typecase frame
      (<frame>
       (let* ((visible (try-catch (! visibleVariables frame)
                                  (ex <com.sun.jdi.AbsentInformationException>
                                      '())))
              (map (! getValues frame visible))
              (p (fun (x) x)))
         (packing (pack)
           (let ((self (ignore-errors (! thisObject frame))))
             (when self
               (pack (list "this" (p self)))))
           (iter (! entrySet map)
                 (fun ((e <java.util.Map$Entry>))
                   (let ((var (as <local-var> (! getKey e)))
                         (val (as <value> (! getValue e))))
                     (pack (list (! name var) (p val)))))))))
      (<faked-frame>
       (packing (pack)
         (when (@ self frame)
           (pack (list "this" (@ self frame))))
         (iter (! entrySet (@ values frame))
               (fun ((e <java.util.Map$Entry>))
                 (let ((var (as <local-var> (! getKey e)))
                       (val (as <value> (! getValue e))))
                   (pack (list (! name var) val)))))))
      (<java.lang.StackTraceElement> '()))))

(df disassemble-frame ((tid <int>) (frame <int>) state)
  (mlet ((frame _) (nth-frame tid frame state))
    (typecase frame
      (<java.lang.StackTraceElement> "<??>")
      (<frame>
       (let* ((l (! location frame))
              (m (! method l))
              (c (! declaringType l)))
          (disassemble-to-string m))))))

;;;;; Restarts

;; FIXME: factorize
(df throw-to-toplevel ((tid <int>) (id <int>) (c <chan>) state)
  (mlet ((tref level exc) (get state tid #f))
    (let* ((t (as <thread-ref> tref))
           (ev (car exc)))
      (typecase ev
        (<exception-event> ; actually uncaughtException
         (! resume t)
         (reply-abort c id)
         ;;(send-debug-return c tid state)
         (do ((level level (1- level))
              (exc exc (cdr exc)))
             ((null? exc))
           (send c `(forward (:debug-return ,tid ,level nil))))
         (del state tid))
        (<breakpoint-event>
         ;; XXX race condition?
         (log "resume from from break (suspendCount: ~d)\n" (! suspendCount t))
         (let ((vm (! virtualMachine t))
               (k (fun () (primitive-throw (<listener-abort>)))))
           (reply-abort c id)
           (! force-early-return t (vm-mirror vm k))
           (! resume t)
           (do ((level level (1- level))
                (exc exc (cdr exc)))
               ((null? exc))
             (send c `(forward (:debug-return ,tid ,level nil))))
           (del state tid)))
        (<interrupt-event>
         (log "resume from from interrupt\n")
         (let ((vm (! virtualMachine t)))
           (! stop t (vm-mirror vm (<listener-abort>)))
           (! resume t)
           (reply-abort c id)
           (do ((level level (1- level))
                (exc exc (cdr exc)))
               ((null? exc))
             (send c `(forward (:debug-return ,tid ,level nil))))
           (del state tid))
         )))))

(df thread-continue ((tid <int>) (id <int>) (c <chan>) state)
  (mlet ((tref level exc) (get state tid #f))
    (log "thread-continue: ~a ~a ~a \n" tref level exc)
    (let* ((t (as <thread-ref> tref)))
       (! resume t))
    (reply-abort c id)
    (do ((level level (1- level))
         (exc exc (cdr exc)))
        ((null? exc))
      (send c `(forward (:debug-return ,tid ,level nil))))
    (del state tid)))

(df thread-step ((t <thread-ref>) k)
  (let* ((vm (! virtual-machine t))
         (erm (! eventRequestManager vm))
         (<sr> <com.sun.jdi.request.StepRequest>)
         (req (! createStepRequest erm t
                 (@s <sr> STEP_MIN)
                 (@s <sr> STEP_OVER))))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addCountFilter req 1)
    (! put-property req 'continuation k)
    (! enable req)))

(df eval-in-thread ((t <thread-ref>) sexp
                    #!optional (env :: <env> (!s <env> current)))
  (let* ((vm (! virtualMachine t))
         (sc :: <class-type>
             (1st (! classes-by-name vm "kawa.standard.Scheme")))
         (ev :: <meth-ref>
             (1st (! methods-by-name sc "eval"
                     (cat "(Ljava/lang/Object;Lgnu/mapping/Environment;)"
                          "Ljava/lang/Object;")))))
    (! invokeMethod sc t ev (list sexp env)
       (@s <class-type> INVOKE_SINGLE_THREADED))))

;;;;; Threads

(df list-threads (vm :: <vm> state)
  (let* ((threads (! allThreads vm)))
    (put state 'all-threads threads)
    (packing (pack)
      (pack '(\:id \:name \:status \:priority))
      (iter threads (fun ((t <thread-ref>))
                      (pack (list (! uniqueID t)
                                  (! name t)
                                  (let ((s (thread-status t)))
                                    (if (! is-suspended t)
                                        (cat "SUSPENDED/" s)
                                        s))
                                  0)))))))

(df thread-status (t :: <thread-ref>)
  (let ((s (! status t)))
    (cond ((= s (@s <thread-ref> THREAD_STATUS_UNKNOWN)) "UNKNOWN")
          ((= s (@s <thread-ref> THREAD_STATUS_ZOMBIE)) "ZOMBIE")
          ((= s (@s <thread-ref> THREAD_STATUS_RUNNING)) "RUNNING")
          ((= s (@s <thread-ref> THREAD_STATUS_SLEEPING)) "SLEEPING")
          ((= s (@s <thread-ref> THREAD_STATUS_MONITOR)) "MONITOR")
          ((= s (@s <thread-ref> THREAD_STATUS_WAIT)) "WAIT")
          ((= s (@s <thread-ref> THREAD_STATUS_NOT_STARTED)) "NOT_STARTED")
          (#t "<bug>"))))

;;;;; Bootstrap

(df vm-attach (=> <vm>)
  (attach (getpid) 20))

(df attach (pid timeout)
  (log "attaching: ~a ~a\n" pid timeout)
  (let* ((<ac> <com.sun.jdi.connect.AttachingConnector>)
         (<arg> <com.sun.jdi.connect.Connector$Argument>)
         (vmm (!s com.sun.jdi.Bootstrap virtualMachineManager))
         (pa (as <ac>
                 (or
                  (find-if (! attaching-connectors vmm)
                           (fun (x :: <ac>)
                             (! equals (! name x) "com.sun.jdi.ProcessAttach"))
                           #f)
                  (error "ProcessAttach connector not found"))))
         (args (! default-arguments pa)))
    (! set-value (as <arg> (! get args (to-str "pid"))) pid)
    (when timeout
      (! set-value (as <arg> (! get args (to-str "timeout"))) timeout))
    (log "attaching2: ~a ~a\n" pa args)
    (! attach pa args)))

(df getpid ()
  (let ((p (make-process (command-parse "echo $PPID") #!null)))
    (! waitFor p)
    (! read-line (<java.io.BufferedReader> (<in> (! get-input-stream p))))))

(df request-uncaught-exception-events ((vm <vm>))
  (let* ((erm (! eventRequestManager vm))
         (req (! createExceptionRequest erm #!null #f #t)))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addThreadFilter req (vm-mirror vm (current-thread)))
    (! enable req)))


(df request-caught-exception-events ((vm <vm>))
  (let* ((erm (! eventRequestManager vm))
         (req (! createExceptionRequest erm #!null #t #f)))
    (! setSuspendPolicy req (@ SUSPEND_EVENT_THREAD req))
    (! addThreadFilter req (vm-mirror vm (current-thread)))
    (! addClassExclusionFilter req "java.lang.ClassLoader")
    (! addClassExclusionFilter req "java.net.URLClassLoader")
    (! addClassExclusionFilter req "java.net.URLClassLoader$1")
    (! enable req)))

(df set-stacktrace-recording ((vm <vm>) (flag <boolean>))
  (for (((e :: <com.sun.jdi.request.ExceptionRequest>)
         (!! exceptionRequests eventRequestManager vm)))
    (when (! notify-caught e)
      (! setEnabled e flag))))

;; (set-stacktrace-recording *the-vm* #f)

(df vm-to-string ((vm <vm>))
  (let* ((obj (as <ref-type> (1st (! classesByName vm "java.lang.Object"))))
         (met (as <meth-ref> (1st (! methodsByName obj "toString")))))
    (fun ((o <obj-ref>) (t <thread-ref>))
      (! value
         (as <str-ref>
             (! invokeMethod o t met '()
                (@s <obj-ref> INVOKE_SINGLE_THREADED)))))))

(define-simple-class <swank-global-variable> ()
  (var #:allocation 'static))

(define-variable *global-get-mirror* #!null)
(define-variable *global-set-mirror* #!null)
(define-variable *global-get-raw* #!null)
(define-variable *global-set-raw* #!null)

(df init-global-field ((vm <vm>))
  (when (nul? *global-get-mirror*)
    (set (@s <swank-global-variable> var) #!null) ; prepare class
    (let* ((swank-global-variable-classes
            (! classes-by-name vm "swank-global-variable"))
           (swank-global-variable-classes-legacy
            (! classes-by-name vm "swank$Mnglobal$Mnvariable"))
           (c (as <com.sun.jdi.ClassType>
                  (1st (if (= (length swank-global-variable-classes) 0)
                           swank-global-variable-classes-legacy
                           swank-global-variable-classes))))
           (f (! fieldByName c "var")))
      (set *global-get-mirror* (fun () (! getValue c f)))
      (set *global-set-mirror* (fun ((v <obj-ref>)) (! setValue c f v))))
    (set *global-get-raw* (fun () '() (@s <swank-global-variable> var)))
    (set *global-set-raw* (fun (x)
                            (set (@s <swank-global-variable> var) x)))))

(df vm-mirror ((vm <vm>) obj)
  (synchronized vm
    (init-global-field vm)
    (*global-set-raw* obj)
    (*global-get-mirror*)))

(df vm-demirror ((vm <vm>) (v <value>))
  (synchronized vm
    (if (== v #!null)
      #!null
      (typecase v
        (<obj-ref> (init-global-field vm)
                   (*global-set-mirror* v)
                   (*global-get-raw*))
        (<com.sun.jdi.IntegerValue> (! value v))
        (<com.sun.jdi.LongValue> (! value v))
        (<com.sun.jdi.CharValue> (! value v))
        (<com.sun.jdi.ByteValue> (! value v))
        (<com.sun.jdi.BooleanValue> (! value v))
        (<com.sun.jdi.ShortValue> (! value v))
        (<com.sun.jdi.FloatValue> (! value v))
        (<com.sun.jdi.DoubleValue> (! value v))))))

(df vm-set-slot ((vm <vm>) (o <object>) (name <str>) value)
  (let* ((o (as <obj-ref> (vm-mirror vm o)))
         (t (! reference-type o))
         (f (! field-by-name t name)))
    (! set-value o f (vm-mirror vm value))))

(define-simple-class <ucex-handler>
    (<java.lang.Thread$UncaughtExceptionHandler>)
  (f :: <gnu.mapping.Procedure>)
  ((*init* (f :: <gnu.mapping.Procedure>)) (set (@ f (this)) f))
  ((uncaughtException (t :: <thread>) (e :: <throwable>))
   :: <void>
   (! println (@s java.lang.System err) (to-str "uhexc:::"))
   (! apply2 f t e)
   #!void))

;;;; Channels

(df spawn (f)
  (let ((thread (<thread> (%%runnable f))))
    (! start thread)
    thread))


;; gnu.mapping.RunnableClosure uses the try{...}catch(Throwable){...}
;; idiom which defeats all attempts to use a break-on-error-style
;; debugger.  Previously I had my own version of RunnableClosure
;; without that deficiency but something in upstream changed and it no
;; longer worked. Now we use the normal RunnableClosure and at the
;; cost of taking stack snapshots on every throw.
(df %%runnable (f => <java.lang.Runnable>)
  ;;(<runnable> f)
  ;;(<gnu.mapping.RunnableClosure> f)
  ;;(runnable f)
  (%runnable f)
  )

(df %runnable (f => <java.lang.Runnable>)
  (runnable
   (fun ()
     (try-catch (f)
                (ex <throwable>
                    (log "exception in thread ~s: ~s" (current-thread)
                          ex)
                    (! printStackTrace ex))))))

(df chan ()
  (let ((lock (<object>))
        (im (<chan>))
        (ex (<chan>)))
    (set (@ lock im) lock)
    (set (@ lock ex) lock)
    (set (@ peer im) ex)
    (set (@ peer ex) im)
    (cons im ex)))

(df immutable? (obj)
  (or (== obj #!null)
      (symbol? obj)
      (number? obj)
      (char? obj)
      (instance? obj <str>)
      (null? obj)))

(df send ((c <chan>) value => <void>)
  (df pass (obj)
    (cond ((immutable? obj) obj)
          ((string? obj) (! to-string obj))
          ((pair? obj)
           (let loop ((r (list (pass (car obj))))
                      (o (cdr obj)))
             (cond ((null? o) (reverse! r))
                   ((pair? o) (loop (cons (pass (car o)) r) (cdr o)))
                   (#t (append (reverse! r) (pass o))))))
          ((instance? obj <chan>)
           (let ((o :: <chan> obj))
             (assert (== (@ owner o) (current-thread)))
             (synchronized (@ lock c)
               (set (@ owner o) (@ owner (@ peer c))))
             o))
          ((or (instance? obj <env>)
               (instance? obj <mirror>))
           ;; those can be shared, for pragmatic reasons
           obj
           )
          (#t (error "can't send" obj (class-name-sans-package obj)))))
  ;;(log "send: ~s ~s -> ~s\n" value (@ owner c) (@ owner (@ peer c)))
  (assert (== (@ owner c) (current-thread)))
  ;;(log "lock: ~s send\n" (@ owner (@ peer c)))
  (synchronized (@ owner (@ peer c))
    (! put (@ queue (@ peer c)) (pass value))
    (! notify (@ owner (@ peer c))))
  ;;(log "unlock: ~s send\n" (@ owner (@ peer c)))
  )

(df recv ((c <chan>))
  (cdr (recv/timeout (list c) 0)))

(df recv* ((cs <iterable>))
  (recv/timeout cs 0))

(df recv/timeout ((cs <iterable>) (timeout <long>))
  (let ((self (current-thread))
        (end (if (zero? timeout)
                 0
                 (+ (current-time) timeout))))
    ;;(log "lock: ~s recv\n" self)
    (synchronized self
      (let loop ()
        ;;(log "receive-loop: ~s\n" self)
        (let ((ready (find-if cs
                              (fun ((c <chan>))
                                (not (! is-empty (@ queue c))))
                              #f)))
          (cond (ready
                 ;;(log "unlock: ~s recv\n" self)
                 (cons ready (! take (@ queue (as <chan> ready)))))
                ((zero? timeout)
                 ;;(log "wait: ~s recv\n" self)
                 (! wait self) (loop))
                (#t
                 (let ((now (current-time)))
                   (cond ((<= end now)
                          'timeout)
                         (#t
                          ;;(log "wait: ~s recv\n" self)
                          (! wait self (- end now))
                          (loop)))))))))))

(df rpc ((c <chan>) msg)
  (mlet* (((im . ex) (chan))
          ((op . args) msg))
    (send c `(,op ,ex . ,args))
    (recv im)))

(df spawn/chan (f)
  (mlet ((im . ex) (chan))
    (let ((thread (<thread> (%%runnable (fun () (f ex))))))
      (set (@ owner ex) thread)
      (! start thread)
      (cons im thread))))

(df spawn/chan/catch (f)
  (spawn/chan
   (fun (c)
     (try-catch
      (f c)
      (ex <throwable>
          (send c `(error ,(! toString ex)
                          ,(class-name-sans-package ex)
                          ,(map (fun (e) (! to-string e))
                                (array-to-list (! get-stack-trace ex))))))))))

;;;; Logging

(define swank-log-port (current-error-port))
(df log (fstr #!rest args)
  (synchronized swank-log-port
    (apply format swank-log-port fstr args)
    (force-output swank-log-port))
  #!void)

;;;; Random helpers

(df 1+ (x) (+ x 1))
(df 1- (x) (- x 1))

(df len (x => <int>)
  (typecase x
    (<list> (length x))
    (<str> (! length x))
    (<string> (string-length x))
    (<vector> (vector-length x))
    (<java.util.List> (! size x))
    (<object[]> (@ length x))))

;;(df put (tab key value) (hash-table-set! tab key value) tab)
;;(df get (tab key default) (hash-table-ref/default tab key default))
;;(df del (tab key) (hash-table-delete! tab key) tab)
;;(df tab () (make-hash-table))

(df put (tab key value) (hashtable-set! tab key value) tab)
(df get (tab key default) (hashtable-ref tab key default))
(df del (tab key) (hashtable-delete! tab key) tab)
(df tab () (make-eqv-hashtable))

(df equal (x y => <boolean>) (equal? x y))

(df current-thread (=> <thread>) (!s java.lang.Thread currentThread))
(df current-time (=> <long>) (!s java.lang.System currentTimeMillis))

(df nul? (x) (== x #!null))

(df read-from-string (str)
  (call-with-input-string str read))

;;(df print-to-string (obj) (call-with-output-string (fun (p) (write obj p))))

(df pprint-to-string (obj)
  (let* ((w (<java.io.StringWriter>))
         (p (<out-port> w #t #f)))
    (try-catch (print-object obj p)
               (ex <throwable>
                   (format p "#<error while printing ~a ~a>"
                           ex (class-name-sans-package ex))))
    (! flush p)
    (to-string (! getBuffer w))))

(df print-object (obj stream)
  (typecase obj
    #;
    ((or (eql #!null) (eql #!eof)
         <list> <number> <character> <string> <vector> <procedure> <boolean>)
     (write obj stream))
    (#t
     #;(print-unreadable-object obj stream)
     (write obj stream)
     )))

(df print-unreadable-object ((o <object>) stream)
  (let* ((string (! to-string o))
         (class (! get-class o))
         (name (! get-name class))
         (simplename (! get-simple-name class)))
    (cond ((! starts-with string "#<")
           (format stream "~a" string))
          ((or (! starts-with string name)
               (! starts-with string simplename))
           (format stream "#<~a>" string))
          (#t
           (format stream "#<~a ~a>" name string)))))

(define cat string-append)

(df values-to-list (values)
  (typecase values
    (<gnu.mapping.Values> (array-to-list (! getValues values)))
    (<object> (list values))))

;; (to-list (as-list (values 1 2 2)))

(df array-to-list ((array <object[]>) => <list>)
  (packing (pack)
    (dotimes (i (@ length array))
      (pack (array i)))))

(df lisp-bool (obj)
  (cond ((== obj 'nil) #f)
        ((== obj 't) #t)
        (#t (error "Can't map lisp boolean" obj))))

(df path-sans-extension ((p path) => <string>)
  (let ((ex (! get-extension p))
        (str (! to-string p)))
    (to-string (cond ((not ex) str)
                     (#t (! substring str 0 (- (len str) (len ex) 1)))))))

(df class-name-sans-package ((obj <object>))
  (cond ((nul? obj) "<#!null>")
        (#t
         (try-catch
          (let* ((c (! get-class obj))
                 (n (! get-simple-name c)))
            (cond ((equal n "") (! get-name c))
                  (#t n)))
          (e <java.lang.Throwable>
             (format "#<~a: ~a>" e (! get-message e)))))))

(df list-env (#!optional (env :: <env> (!s <env> current)))
  (let ((enum (! enumerateAllLocations env)))
    (packing (pack)
      (while (! hasMoreElements enum)
        (pack (! nextLocation enum))))))

(df list-file (filename)
  (with (port (call-with-input-file filename))
    (let* ((lang (!s gnu.expr.Language getDefaultLanguage))
           (messages (<gnu.text.SourceMessages>))
           (comp (! parse lang (as <in-port> port) messages 0)))
      (! get-module comp))))

(df list-decls (file)
  (let* ((module (as <gnu.expr.ModuleExp> (list-file file))))
    (do ((decl :: <gnu.expr.Declaration>
               (! firstDecl module) (! nextDecl decl)))
        ((nul? decl))
      (format #t "~a ~a:~d:~d\n" decl
              (! getFileName decl)
              (! getLineNumber decl)
              (! getColumnNumber decl)
              ))))

(df %time (f)
  (define-alias <mf> <java.lang.management.ManagementFactory>)
  (define-alias <gc> <java.lang.management.GarbageCollectorMXBean>)
  (let* ((gcs (!s <mf> getGarbageCollectorMXBeans))
         (mem (!s <mf> getMemoryMXBean))
         (jit (!s <mf> getCompilationMXBean))
         (oldjit (! getTotalCompilationTime jit))
         (oldgc (packing (pack)
                  (iter gcs (fun ((gc <gc>))
                              (pack (cons gc
                                          (list (! getCollectionCount gc)
                                                (! getCollectionTime gc))))))))
         (heap (!! getUsed getHeapMemoryUsage mem))
         (nonheap (!! getUsed getNonHeapMemoryUsage mem))
         (start (!s java.lang.System nanoTime))
         (values (f))
         (end (!s java.lang.System nanoTime))
         (newheap (!! getUsed getHeapMemoryUsage mem))
         (newnonheap (!! getUsed getNonHeapMemoryUsage mem)))
    (format #t "~&")
    (let ((njit (! getTotalCompilationTime jit)))
      (format #t "; JIT compilation: ~:d ms (~:d)\n" (- njit oldjit) njit))
    (iter gcs (fun ((gc <gc>))
                (mlet ((_ count time) (assoc gc oldgc))
                  (format #t "; GC ~a: ~:d ms (~d)\n"
                          (! getName gc)
                          (- (! getCollectionTime gc) time)
                          (- (! getCollectionCount gc) count)))))
    (format #t "; Heap: ~@:d (~:d)\n" (- newheap heap) newheap)
    (format #t "; Non-Heap: ~@:d (~:d)\n" (- newnonheap nonheap) newnonheap)
    (format #t "; Elapsed time: ~:d us\n" (/ (- end start) 1000))
    values))

(define-syntax time
  (syntax-rules ()
    ((time form)
     (%time (lambda () form)))))

(df gc ()
  (let* ((mem (!s java.lang.management.ManagementFactory getMemoryMXBean))
         (oheap (!! getUsed getHeapMemoryUsage mem))
         (onheap (!! getUsed getNonHeapMemoryUsage mem))
         (_ (! gc mem))
         (heap (!! getUsed  getHeapMemoryUsage mem))
         (nheap (!! getUsed getNonHeapMemoryUsage mem)))
    (format #t "; heap: ~@:d (~:d) non-heap: ~@:d (~:d)\n"
             (- heap oheap) heap (- onheap nheap) nheap)))

(df room ()
  (let* ((pools (!s java.lang.management.ManagementFactory
                    getMemoryPoolMXBeans))
         (mem (!s java.lang.management.ManagementFactory getMemoryMXBean))
         (heap (!! getUsed  getHeapMemoryUsage mem))
         (nheap (!! getUsed getNonHeapMemoryUsage mem)))
    (iter pools (fun ((p <java.lang.management.MemoryPoolMXBean>))
                  (format #t "~&; ~a~1,16t: ~10:d\n"
                          (! getName p)
                          (!! getUsed getUsage p))))
    (format #t "; Heap~1,16t: ~10:d\n" heap)
    (format #t "; Non-Heap~1,16t: ~10:d\n" nheap)))

;; (df javap (class #!key method signature)
;;   (let* ((<is> <java.io.ByteArrayInputStream>)
;;          (bytes
;;           (typecase class
;;             (<string> (read-bytes (<java.io.FileInputStream> (to-str class))))
;;             (<byte[]> class)
;;             (<symbol> (read-class-file class))))
;;          (cdata (<sun.tools.javap.ClassData> (<is> bytes)))
;;          (p (<sun.tools.javap.JavapPrinter>
;;           (<is> bytes)
;;              (current-output-port)
;;              (<sun.tools.javap.JavapEnvironment>))))
;;     (cond (method
;;            (dolist ((m <sun.tools.javap.MethodData>)
;;                     (array-to-list (! getMethods cdata)))
;;              (when (and (equal (to-str method) (! getName m))
;;                         (or (not signature)
;;                             (equal signature (! getInternalSig m))))
;;                (! printMethodSignature p m (! getAccess m))
;;                (! printExceptions p m)
;;                (newline)
;;                (! printVerboseHeader p m)
;;                (! printcodeSequence p m))))
;;           (#t (p:print)))
;;     (values)))

(df read-bytes ((is <java.io.InputStream>) => <byte[]>)
  (let ((os (<java.io.ByteArrayOutputStream>)))
    (let loop ()
      (let ((c (! read is)))
        (cond ((= c -1))
              (#t (! write os c) (loop)))))
    (! to-byte-array os)))

(df read-class-file ((name <symbol>) => <byte[]>)
  (let ((f (cat (! replace (to-str name) (as <char> #\.) (as <char> #\/))
                ".class")))
    (mcase (find-file-in-path f (class-path))
      ('#f (ferror "Can't find classfile for ~s" name))
      ((:zip zipfile entry)
       (let* ((z (<java.util.zip.ZipFile> (as <str> zipfile)))
              (e (! getEntry z (as <str> entry))))
         (read-bytes (! getInputStream z e))))
      ((:file s) (read-bytes (<java.io.FileInputStream> (as <str> s)))))))

(df all-instances ((vm <vm>) (classname <str>))
  (mappend (fun ((c <class-type>)) (to-list (! instances c (as long 9999))))
           (%all-subclasses vm classname)))

(df %all-subclasses ((vm <vm>) (classname <str>))
  (mappend (fun ((c <class-type>)) (cons c (to-list (! subclasses c))))
           (to-list (! classes-by-name vm classname))))

(df with-output-to-string (thunk => <str>)
  (call-with-output-string
   (fun (s) (parameterize ((current-output-port s)) (thunk)))))

(df find-if ((i <iterable>) test default)
  (let ((iter (! iterator i))
        (found #f))
    (while (and (not found) (! has-next iter))
      (let ((e (! next iter)))
        (when (test e)
          (set found #t)
          (set default e))))
    default))

(df filter ((i <iterable>) test => <list>)
  (packing (pack)
    (for ((e i))
      (when (test e)
        (pack e)))))

(df iter ((i <iterable>) f)
  (for ((e i)) (f e)))

(df mapi ((i <iterable>) f => <list>)
  (packing (pack) (for ((e i)) (pack (f e)))))

(df nth ((i <iterable>) (n <int>))
  (let ((iter (! iterator i)))
    (dotimes (i n)
      (! next iter))
    (! next iter)))

(df 1st ((i <iterable>)) (!! next iterator i))

(df to-list ((i <iterable>) => <list>)
  (packing (pack) (for ((e i)) (pack e))))

(df as-list ((o <java.lang.Object[]>) => <java.util.List>)
  (!s java.util.Arrays asList o))

(df mappend (f list)
  (apply append (map f list)))

(df subseq (s from to)
  (typecase s
    (<list> (apply list (! sub-list s from to)))
    (<vector> (apply vector (! sub-list s from to)))
    (<str> (! substring s from to))
    (<byte[]> (let* ((len (as <int> (- to from)))
                     (t (<byte[]> #:length len)))
                (!s java.lang.System arraycopy s from t 0 len)
                t))))

(df to-string (obj => <string>)
  (typecase obj
    (<str> (<gnu.lists.FString> obj))
    ((satisfies string?) obj)
    ((satisfies symbol?) (symbol->string obj))
    (<java.lang.StringBuffer> (<gnu.lists.FString> obj))
    (<java.lang.StringBuilder> (<gnu.lists.FString> obj))
    (#t (error "Not a string designator" obj
               (class-name-sans-package obj)))))

(df to-str (obj => <str>)
  (cond ((instance? obj <str>) obj)
        ((string? obj) (! toString obj))
        ((symbol? obj) (! getName (as <gnu.mapping.Symbol> obj)))
        (#t (error "Not a string designator" obj
                   (class-name-sans-package obj)))))

))

;; Local Variables:
;; mode: goo
;; compile-command: "\
;;  rm -rf classes && \
;;  JAVA_OPTS=-Xss2M kawa --r7rs -d classes -C swank-kawa.scm && \
;;  jar cf swank-kawa.jar -C classes ."
;; End:
