;; swank-larceny.scm --- Swank server for Larceny
;;
;; License: Public Domain
;; Author: Helmut Eller
;;
;; In a shell execute:
;;   larceny -r6rs -program swank-larceny.scm
;; and then `M-x slime-connect' in Emacs.

(library (swank os)
    (export getpid make-server-socket accept local-port close-socket)
    (import (rnrs)
	    (primitives foreign-procedure 
			ffi/handle->address
			ffi/string->asciiz
			sizeof:pointer
			sizeof:int
			%set-pointer
			%get-int))

 (define getpid (foreign-procedure "getpid" '() 'int))
 (define fork (foreign-procedure "fork" '() 'int))
 (define close (foreign-procedure "close" '(int) 'int))
 (define dup2 (foreign-procedure "dup2" '(int int) 'int))

 (define bytevector-content-offset$ sizeof:pointer)

 (define execvp% (foreign-procedure "execvp" '(string boxed) 'int))
 (define (execvp file . args)
   (let* ((nargs (length args))
	  (argv (make-bytevector (* (+ nargs 1)
				    sizeof:pointer))))
     (do ((offset 0 (+ offset sizeof:pointer))
	  (as args (cdr as)))
	 ((null? as))
       (%set-pointer argv
		     offset
		     (+ (ffi/handle->address (ffi/string->asciiz (car as)))
			bytevector-content-offset$)))
     (%set-pointer argv (* nargs sizeof:pointer) 0)
     (execvp% file argv)))

 (define pipe% (foreign-procedure "pipe" '(boxed) 'int))
 (define (pipe)
   (let ((array (make-bytevector (* sizeof:int 2))))
     (let ((r (pipe% array)))
       (values r (%get-int array 0) (%get-int array sizeof:int)))))

 (define (fork/exec file . args)
   (let ((pid (fork)))
     (cond ((= pid 0)
	    (apply execvp file args))
	   (#t pid))))
 
 (define (start-process file . args)
   (let-values (((r1 down-out down-in) (pipe))
		((r2 up-out up-in) (pipe))
		((r3 err-out err-in) (pipe)))
     (assert (= 0 r1))
     (assert (= 0 r2))
     (assert (= 0 r3))
     (let ((pid (fork)))
       (case pid
	 ((-1)
	  (error "Failed to fork a subprocess."))
	 ((0)
	  (close up-out)
	  (close err-out)
	  (close down-in)
	  (dup2 down-out 0)
	  (dup2 up-in 1)
	  (dup2 err-in 2)
	  (apply execvp file args)
	  (exit 1))
	 (else 
	  (close down-out)
	  (close up-in)
	  (close err-in)
	  (list pid 
		(make-fd-io-stream up-out down-in)
		(make-fd-io-stream err-out err-out)))))))

 (define (make-fd-io-stream in out)
   (let ((write (lambda (bv start count) (fd-write out bv start count)))
	 (read (lambda (bv start count) (fd-read in bv start count)))
	 (closeit (lambda () (close in) (close out))))
     (make-custom-binary-input/output-port
      "fd-stream" read write #f #f closeit)))

 (define write% (foreign-procedure "write" '(int ulong int) 'int))
 (define (fd-write fd bytevector start count)
   (write% fd 
	   (+ (ffi/handle->address bytevector)
	      bytevector-content-offset$
	      start)
	   count))

 (define read% (foreign-procedure "read" '(int ulong int) 'int))
 (define (fd-read fd bytevector start count)
   ;;(printf "fd-read: ~a ~s ~a ~a\n" fd bytevector start count)
   (read% fd 
	  (+ (ffi/handle->address bytevector)
	     bytevector-content-offset$
	     start)
	  count))

 (define (make-server-socket port)
   (let* ((args `("/bin/bash" "bash" 
		  "-c" 
		  ,(string-append
		    "netcat -s 127.0.0.1 -q 0 -l -v "
		    (if port 
			(string-append "-p " (number->string port))
			""))))
	  (nc (apply start-process args))
	  (err (transcoded-port (list-ref nc 2)
				(make-transcoder (latin-1-codec))))
	  (line (get-line err))
	  (pos (last-index-of line '#\])))
     (cond (pos
	    (let* ((tail (substring line (+ pos 1) (string-length line)))
		   (port (get-datum (open-string-input-port tail))))
	      (list (car nc) (cadr nc) err port)))
	   (#t (error "netcat failed: " line)))))

 (define (accept socket codec)
   (let* ((line (get-line (caddr socket)))
	  (pos (last-index-of line #\])))
     (cond (pos 
	    (close-port (caddr socket))
	    (let ((stream (cadr socket)))
	      (let ((io (transcoded-port stream (make-transcoder codec))))
		(values io io))))
	   (else (error "accept failed: " line)))))

 (define (local-port socket)
   (list-ref socket 3))

 (define (last-index-of str chr)
   (let loop ((i (string-length str)))
     (cond ((<= i 0) #f)
	   (#t (let ((i (- i 1)))
		 (cond ((char=? (string-ref str i) chr)
			i)
		       (#t 
			(loop i))))))))

 (define (close-socket socket)
   ;;(close-port (cadr socket))
   #f
   )

 )

(library (swank sys)
    (export implementation-name eval-in-interaction-environment)
    (import (rnrs) 
	    (primitives system-features
			aeryn-evaluator))

 (define (implementation-name) "larceny")

 ;; see $LARCENY/r6rsmode.sch:
 ;;   Larceny's ERR5RS and R6RS modes.
 ;;   Code names:
 ;;       Aeryn    ERR5RS
 ;;       D'Argo   R6RS-compatible
 ;;       Spanky   R6RS-conforming (not yet implemented)
 (define (eval-in-interaction-environment form)
   (aeryn-evaluator form))

 )

(import (rnrs) (rnrs eval) (larceny load))
(load "swank-r6rs.scm")
(eval '(start-server #f) (environment '(swank)))
