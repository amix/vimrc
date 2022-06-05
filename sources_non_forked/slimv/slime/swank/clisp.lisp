;;;; -*- indent-tabs-mode: nil -*-

;;;; SWANK support for CLISP.

;;;; Copyright (C) 2003, 2004 W. Jenkner, V. Sedach

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2 of
;;;; the License, or (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public
;;;; License along with this program; if not, write to the Free
;;;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;;; MA 02111-1307, USA.

;;; This is work in progress, but it's already usable.  Many things
;;; are adapted from other swank-*.lisp, in particular from
;;; swank-allegro (I don't use allegro at all, but it's the shortest
;;; one and I found Helmut Eller's code there enlightening).

;;; This code will work better with recent versions of CLISP (say, the
;;; last release or CVS HEAD) while it may not work at all with older
;;; versions.  It is reasonable to expect it to work on platforms with
;;; a "SOCKET" package, in particular on GNU/Linux or Unix-like
;;; systems, but also on Win32.  This backend uses the portable xref
;;; from the CMU AI repository and metering.lisp from CLOCC [1], which
;;; are conveniently included in SLIME.

;;; [1] http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/tools/metering/

(defpackage swank/clisp
  (:use cl swank/backend))

(in-package swank/clisp)

(eval-when (:compile-toplevel)
  (unless (string< "2.44" (lisp-implementation-version))
    (error "Need at least CLISP version 2.44")))

(defimplementation gray-package-name ()
  "GRAY")

;;;; if this lisp has the complete CLOS then we use it, otherwise we
;;;; build up a "fake" swank-mop and then override the methods in the
;;;; inspector.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *have-mop*
    (and (find-package :clos)
         (eql :external
              (nth-value 1 (find-symbol (string ':standard-slot-definition)
                                        :clos))))
    "True in those CLISP images which have a complete MOP implementation."))

#+#.(cl:if swank/clisp::*have-mop* '(cl:and) '(cl:or))
(progn
  (import-swank-mop-symbols :clos '(:slot-definition-documentation))

  (defun swank-mop:slot-definition-documentation (slot)
    (clos::slot-definition-documentation slot)))

#-#.(cl:if swank/clisp::*have-mop* '(and) '(or))
(defclass swank-mop:standard-slot-definition ()
  ()
  (:documentation
   "Dummy class created so that swank.lisp will compile and load."))

(let ((getpid (or (find-symbol "PROCESS-ID" :system)
                  ;; old name prior to 2005-03-01, clisp <= 2.33.2
                  (find-symbol "PROGRAM-ID" :system)
                  #+win32 ; integrated into the above since 2005-02-24
                  (and (find-package :win32) ; optional modules/win32
                       (find-symbol "GetCurrentProcessId" :win32)))))
  (defimplementation getpid () ; a required interface
    (cond
      (getpid (funcall getpid))
      #+win32 ((ext:getenv "PID")) ; where does that come from?
      (t -1))))

(defimplementation call-with-user-break-handler (handler function)
  (handler-bind ((system::simple-interrupt-condition
                  (lambda (c)
                    (declare (ignore c))
                    (funcall handler)
                    (when (find-restart 'socket-status)
                      (invoke-restart (find-restart 'socket-status)))
                    (continue))))
    (funcall function)))

(defimplementation lisp-implementation-type-name ()
  "clisp")

(defimplementation set-default-directory (directory)
  (setf (ext:default-directory) directory)
  (namestring (setf *default-pathname-defaults* (ext:default-directory))))

(defimplementation filename-to-pathname (string)
  (cond ((member :cygwin *features*)
         (parse-cygwin-filename string))
        (t (parse-namestring string))))

(defun parse-cygwin-filename (string)
  (multiple-value-bind (match _ drive absolute)
      (regexp:match "^(([a-zA-Z\\]+):)?([\\/])?" string :extended t)
    (declare (ignore _))
    (assert (and match (if drive absolute t)) ()
            "Invalid filename syntax: ~a" string)
    (let* ((sans-prefix (subseq string (regexp:match-end match)))
           (path (remove "" (regexp:regexp-split "[\\/]" sans-prefix)))
           (path (loop for name in path collect
                       (cond ((equal name "..") ':back)
                             (t name))))
           (directoryp (or (equal string "")
                           (find (aref string (1- (length string))) "\\/"))))
      (multiple-value-bind (file type)
          (cond ((and (not directoryp) (last path))
                 (let* ((file (car (last path)))
                        (pos (position #\. file :from-end t)))
                   (cond ((and pos (> pos 0)) 
                          (values (subseq file 0 pos)
                                  (subseq file (1+ pos))))
                         (t file)))))
        (make-pathname :host nil
                       :device nil
                       :directory (cons 
                                   (if absolute :absolute :relative)
                                   (let ((path (if directoryp 
                                                   path 
                                                   (butlast path))))
                                     (if drive
                                         (cons 
                                          (regexp:match-string string drive)
                                          path)
                                         path)))
                       :name file 
                       :type type)))))

;;;; UTF 

(defimplementation string-to-utf8 (string)
  (let ((enc (load-time-value 
              (ext:make-encoding :charset "utf-8" :line-terminator :unix)
              t)))
    (ext:convert-string-to-bytes string enc)))

(defimplementation utf8-to-string (octets)
  (let ((enc (load-time-value 
              (ext:make-encoding :charset "utf-8" :line-terminator :unix)
              t)))
    (ext:convert-string-from-bytes octets enc)))

;;;; TCP Server

(defimplementation create-socket (host port &key backlog)
  (socket:socket-server port :interface host :backlog (or backlog 5)))

(defimplementation local-port (socket)
  (socket:socket-server-port socket))

(defimplementation close-socket (socket)
  (socket:socket-server-close socket))

(defimplementation accept-connection (socket
                                      &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (socket:socket-accept socket
                        :buffered buffering ;; XXX may not work if t
                        :element-type (if external-format 
                                          'character
                                          '(unsigned-byte 8))
                        :external-format (or external-format :default)))

#-win32
(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (let ((streams (mapcar (lambda (s) (list* s :input nil)) streams)))
    (loop
     (cond ((check-slime-interrupts) (return :interrupt))
           (timeout
            (socket:socket-status streams 0 0)
            (return (loop for (s nil . x) in streams
                          if x collect s)))
           (t
            (with-simple-restart (socket-status "Return from socket-status.")
              (socket:socket-status streams 0 500000))
            (let ((ready (loop for (s nil . x) in streams
                               if x collect s)))
              (when ready (return ready))))))))

#+win32
(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (loop
   (cond ((check-slime-interrupts) (return :interrupt))
         (t
          (let ((ready (remove-if-not #'input-available-p streams)))
            (when ready (return ready)))
          (when timeout (return nil))
          (sleep 0.1)))))

#+win32
;; Some facts to remember (for the next time we need to debug this):
;;  - interactive-sream-p returns t for socket-streams
;;  - listen returns nil for socket-streams
;;  - (type-of <socket-stream>) is 'stream
;;  - (type-of *terminal-io*) is 'two-way-stream
;;  - stream-element-type on our sockets is usually (UNSIGNED-BYTE 8)
;;  - calling socket:socket-status on non sockets signals an error,
;;    but seems to mess up something internally.
;;  - calling read-char-no-hang on sockets does not signal an error,
;;    but seems to mess up something internally.
(defun input-available-p (stream)
  (case (stream-element-type stream)
    (character
     (let ((c (read-char-no-hang stream nil nil)))
       (cond ((not c)
              nil)
             (t
              (unread-char c stream)
              t))))
    (t
     (eq (socket:socket-status (cons stream :input) 0 0)
         :input))))

;;;; Coding systems

(defvar *external-format-to-coding-system*
  '(((:charset "iso-8859-1" :line-terminator :unix)
     "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    ((:charset "iso-8859-1")
     "latin-1" "iso-latin-1" "iso-8859-1")
    ((:charset "utf-8") "utf-8")
    ((:charset "utf-8" :line-terminator :unix) "utf-8-unix")
    ((:charset "euc-jp") "euc-jp")
    ((:charset "euc-jp" :line-terminator :unix) "euc-jp-unix")
    ((:charset "us-ascii") "us-ascii")
    ((:charset "us-ascii" :line-terminator :unix) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (let ((args (car (rassoc-if (lambda (x)
                                (member coding-system x :test #'equal))
                              *external-format-to-coding-system*))))
    (and args (apply #'ext:make-encoding args))))


;;;; Swank functions

(defimplementation arglist (fname)
  (block nil
    (or (ignore-errors
          (let ((exp (function-lambda-expression fname)))
            (and exp (return (second exp)))))
        (ignore-errors
          (return (ext:arglist fname)))
        :not-available)))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (ext:expand-form form))

(defimplementation collect-macro-forms (form &optional env)
  ;; Currently detects only normal macros, not compiler macros.
  (declare (ignore env))
  (with-collected-macro-forms (macro-forms)
      (handler-bind ((warning #'muffle-warning))
        (ignore-errors
          (compile nil `(lambda () ,form))))
    (values macro-forms nil)))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result ()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push :variable (when (boundp symbol) (doc 'variable)))
      (when (fboundp symbol)
        (maybe-push
         ;; Report WHEN etc. as macros, even though they may be
         ;; implemented as special operators.
         (if (macro-function symbol) :macro
             (typecase (fdefinition symbol)
               (generic-function :generic-function)
               (function         :function)
               ;; (type-of 'progn) -> ext:special-operator
               (t                :special-operator)))
         (doc 'function)))
      (when (or (get symbol 'system::setf-function) ; e.g. #'(setf elt)
                (get symbol 'system::setf-expander)); defsetf
        (maybe-push :setf (doc 'setf)))
      (when (or (get symbol 'system::type-symbol); cf. clisp/src/describe.lisp
                (get symbol 'system::defstruct-description)
                (get symbol 'system::deftype-expander))
        (maybe-push :type (doc 'type))) ; even for 'structure
      (when (find-class symbol nil)
        (maybe-push :class (doc 'type)))
      ;; Let this code work compiled in images without FFI
      (let ((types (load-time-value
                    (and (find-package "FFI")
                         (symbol-value
                          (find-symbol "*C-TYPE-TABLE*" "FFI"))))))
        ;; Use ffi::*c-type-table* so as not to suffer the overhead of
        ;; (ignore-errors (ffi:parse-c-type symbol)) for 99.9% of symbols
        ;; which are not FFI type names.
        (when (and types (nth-value 1 (gethash symbol types)))
          ;; Maybe use (case (head (ffi:deparse-c-type)))
          ;; to distinguish struct and union types?
          (maybe-push :alien-type :not-documented)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable (describe symbol))
    (:macro (describe (macro-function symbol)))
    (:function (describe (symbol-function symbol)))
    (:class (describe (find-class symbol)))))

(defimplementation type-specifier-p (symbol)
  (or (ignore-errors
       (subtypep nil symbol))
      (not (eq (type-specifier-arglist symbol) :not-available))))

(defun fspec-pathname (spec)
  (let ((path spec)
	type
        lines)
    (when (consp path)
      (psetq type (car path)
	     path (cadr path)
             lines (cddr path)))
    (when (and path
               (member (pathname-type path)
                       custom:*compiled-file-types* :test #'equal))
      (setq path
            (loop for suffix in custom:*source-file-types*
               thereis (probe-file (make-pathname :defaults path
                                                  :type suffix)))))
    (values path type lines)))

(defun fspec-location (name fspec)
  (multiple-value-bind (file type lines)
      (fspec-pathname fspec)
    (list (if type (list name type) name)
	  (cond (file
		 (multiple-value-bind (truename c) 
                     (ignore-errors (truename file))
		   (cond (truename
			  (make-location 
                           (list :file (namestring truename))
                           (if (consp lines)
                               (list* :line lines)
                               (list :function-name (string name)))
                           (when (consp type)
                             (list :snippet (format nil "~A" type)))))
			 (t (list :error (princ-to-string c))))))
		(t (list :error 
                         (format nil "No source information available for: ~S"
                                 fspec)))))))

(defimplementation find-definitions (name)
  (mapcar #'(lambda (e) (fspec-location name e)) 
          (documentation name 'sys::file)))

(defun trim-whitespace (string)
  (string-trim #(#\newline #\space #\tab) string))

(defvar *sldb-backtrace*)

(defun sldb-backtrace ()
  "Return a list ((ADDRESS . DESCRIPTION) ...) of frames."
  (let* ((modes '((:all-stack-elements 1)
                  (:all-frames 2)
                  (:only-lexical-frames 3)
                  (:only-eval-and-apply-frames 4)
                  (:only-apply-frames 5)))
         (mode (cadr (assoc :all-stack-elements modes))))
    (do ((frames '())
         (last nil frame)
         (frame (sys::the-frame)
                (sys::frame-up 1 frame mode)))
        ((eq frame last) (nreverse frames))
      (unless (boring-frame-p frame)
        (push frame frames)))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* (;;(sys::*break-count* (1+ sys::*break-count*))
         ;;(sys::*driver* debugger-loop-fn)
         ;;(sys::*fasoutput-stream* nil)
         (*sldb-backtrace*
          (let* ((f (sys::the-frame))
                 (bt (sldb-backtrace))
                 (rest (member f bt)))
            (if rest (nthcdr 8 rest) bt))))
    (funcall debugger-loop-fn)))

(defun nth-frame (index)
  (nth index *sldb-backtrace*))

(defun boring-frame-p (frame)
  (member (frame-type frame) '(stack-value bind-var bind-env
                               compiled-tagbody compiled-block)))

(defun frame-to-string (frame)
  (with-output-to-string (s)
    (sys::describe-frame s frame)))

(defun frame-type (frame)
  ;; FIXME: should bind *print-length* etc. to small values.
  (frame-string-type (frame-to-string frame)))

;; FIXME: they changed the layout in 2.44 and not all patterns have
;; been updated.
(defvar *frame-prefixes*
  '(("\\[[0-9]\\+\\] frame binding variables" bind-var)
    ("<1> #<compiled-function" compiled-fun)
    ("<1> #<system-function" sys-fun)
    ("<1> #<special-operator" special-op)
    ("EVAL frame" eval)
    ("APPLY frame" apply)
    ("\\[[0-9]\\+\\] compiled tagbody frame" compiled-tagbody)
    ("\\[[0-9]\\+\\] compiled block frame" compiled-block)
    ("block frame" block)
    ("nested block frame" block)
    ("tagbody frame" tagbody)
    ("nested tagbody frame" tagbody)
    ("catch frame" catch)
    ("handler frame" handler)
    ("unwind-protect frame" unwind-protect)
    ("driver frame" driver)
    ("\\[[0-9]\\+\\] frame binding environments" bind-env)
    ("CALLBACK frame" callback)
    ("- " stack-value)
    ("<1> " fun)
    ("<2> " 2nd-frame)
    ))

(defun frame-string-type (string)
  (cadr (assoc-if (lambda (pattern) (is-prefix-p pattern string))
                  *frame-prefixes*)))

(defimplementation compute-backtrace (start end)
  (let* ((bt *sldb-backtrace*)
         (len (length bt)))
    (loop for f in (subseq bt start (min (or end len) len))
          collect f)))

(defimplementation print-frame (frame stream)
  (let* ((str (frame-to-string frame)))
    (write-string (extract-frame-line str)
                  stream)))

(defun extract-frame-line (frame-string)
  (let ((s frame-string))
    (trim-whitespace
     (case (frame-string-type s)
       ((eval special-op)
        (string-match "EVAL frame .*for form \\(.*\\)" s 1))
       (apply
        (string-match "APPLY frame for call \\(.*\\)" s 1))
       ((compiled-fun sys-fun fun)
        (extract-function-name s))
       (t s)))))

(defun extract-function-name (string)
  (let ((1st (car (split-frame-string string))))
    (or (string-match (format nil "^<1>[ ~%]*#<[-A-Za-z]* \\(.*\\)>")
                      1st
                      1)
        (string-match (format nil "^<1>[ ~%]*\\(.*\\)") 1st 1)
        1st)))

(defun split-frame-string (string)
  (let ((rx (format nil "~%\\(~{~A~^\\|~}\\)"
                    (mapcar #'car *frame-prefixes*))))
    (loop for pos = 0 then (1+ (regexp:match-start match))
          for match = (regexp:match rx string :start pos)
          if match collect (subseq string pos (regexp:match-start match))
          else collect (subseq string pos)
          while match)))

(defun string-match (pattern string n)
  (let* ((match (nth-value n (regexp:match pattern string))))
    (if match (regexp:match-string string match))))

(defimplementation eval-in-frame (form frame-number)
  (sys::eval-at (nth-frame frame-number) form))

(defimplementation frame-locals (frame-number)
  (let ((frame (nth-frame frame-number)))
    (loop for i below (%frame-count-vars frame)
          collect (list :name (%frame-var-name frame i)
                        :value (%frame-var-value frame i)
                        :id 0))))

(defimplementation frame-var-value (frame var)
  (%frame-var-value (nth-frame frame) var))

;;; Interpreter-Variablen-Environment has the shape
;;; NIL or #(v1 val1 ... vn valn NEXT-ENV).

(defun %frame-count-vars (frame)
  (cond ((sys::eval-frame-p frame)
         (do ((venv (frame-venv frame) (next-venv venv))
              (count 0 (+ count (/ (1- (length venv)) 2))))
             ((not venv) count)))
        ((member (frame-type frame) '(compiled-fun sys-fun fun special-op))
         (length (%parse-stack-values frame)))
        (t 0)))

(defun %frame-var-name (frame i)
  (cond ((sys::eval-frame-p frame)
         (nth-value 0 (venv-ref (frame-venv frame) i)))
        (t (format nil "~D" i))))

(defun %frame-var-value (frame i)
  (cond ((sys::eval-frame-p frame)
         (let ((name (venv-ref (frame-venv frame) i)))
           (multiple-value-bind (v c) (ignore-errors (sys::eval-at frame name))
             (if c
                 (format-sldb-condition c)
                 v))))
        ((member (frame-type frame) '(compiled-fun sys-fun fun special-op))
         (let ((str (nth i (%parse-stack-values frame))))
           (trim-whitespace (subseq str 2))))
        (t (break "Not implemented"))))

(defun frame-venv (frame)
  (let ((env (sys::eval-at frame '(sys::the-environment))))
    (svref env 0)))

(defun next-venv (venv) (svref venv (1- (length venv))))

(defun venv-ref (env i)
  "Reference the Ith binding in ENV.
Return two values: NAME and VALUE"
  (let ((idx (* i 2)))
    (if (< idx (1- (length env)))
        (values (svref env idx) (svref env (1+ idx)))
        (venv-ref (next-venv env) (- i (/ (1- (length env)) 2))))))

(defun %parse-stack-values (frame)
  (labels ((next (fp) (sys::frame-down 1 fp 1))
           (parse (fp accu)
             (let ((str (frame-to-string fp)))
               (cond ((is-prefix-p "- " str)
                      (parse  (next fp) (cons str accu)))
                     ((is-prefix-p "<1> " str)
                      ;;(when (eq (frame-type frame) 'compiled-fun)
                      ;;  (pop accu))
                      (dolist (str (cdr (split-frame-string str)))
                        (when (is-prefix-p "- " str)
                          (push str accu)))
                      (nreverse accu))
                     (t (parse (next fp) accu))))))
    (parse (next frame) '())))

(defun is-prefix-p (regexp string)
  (if (regexp:match (concatenate 'string "^" regexp) string) t))

(defimplementation return-from-frame (index form)
  (sys::return-from-eval-frame (nth-frame index) form))

(defimplementation restart-frame (index)
  (sys::redo-eval-frame (nth-frame index)))

(defimplementation frame-source-location (index)
  `(:error
    ,(format nil "frame-source-location not implemented. (frame: ~A)"
             (nth-frame index))))

;;;; Profiling

(defimplementation profile (fname)
  (eval `(swank-monitor:monitor ,fname)))         ;monitor is a macro

(defimplementation profiled-functions ()
  swank-monitor:*monitored-functions*)

(defimplementation unprofile (fname)
  (eval `(swank-monitor:unmonitor ,fname)))       ;unmonitor is a macro

(defimplementation unprofile-all ()
  (swank-monitor:unmonitor))

(defimplementation profile-report ()
  (swank-monitor:report-monitoring))

(defimplementation profile-reset ()
  (swank-monitor:reset-all-monitoring))

(defimplementation profile-package (package callers-p methods)
  (declare (ignore callers-p methods))
  (swank-monitor:monitor-all package))

;;;; Handle compiler conditions (find out location of error etc.)

(defmacro compile-file-frobbing-notes ((&rest args) &body body)
  "Pass ARGS to COMPILE-FILE, send the compiler notes to
*STANDARD-INPUT* and frob them in BODY."
  `(let ((*error-output* (make-string-output-stream))
         (*compile-verbose* t))
     (multiple-value-prog1
      (compile-file ,@args)
      (handler-case
       (with-input-from-string
        (*standard-input* (get-output-stream-string *error-output*))
        ,@body)
       (sys::simple-end-of-file () nil)))))

(defvar *orig-c-warn* (symbol-function 'system::c-warn))
(defvar *orig-c-style-warn* (symbol-function 'system::c-style-warn))
(defvar *orig-c-error* (symbol-function 'system::c-error))
(defvar *orig-c-report-problems* (symbol-function 'system::c-report-problems))

(defmacro dynamic-flet (names-functions &body body)
  "(dynamic-flet ((NAME FUNCTION) ...) BODY ...)
Execute BODY with NAME's function slot set to FUNCTION."
  `(ext:letf* ,(loop for (name function) in names-functions
                     collect `((symbol-function ',name) ,function))
    ,@body))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)

(defun compiler-note-location ()
  "Return the current compiler location."
  (let ((lineno1 sys::*compile-file-lineno1*)
        (lineno2 sys::*compile-file-lineno2*)
        (file sys::*compile-file-truename*))
    (cond ((and file lineno1 lineno2)
           (make-location (list ':file (namestring file))
                          (list ':line lineno1)))
          (*buffer-name*
           (make-location (list ':buffer *buffer-name*)
                          (list ':offset *buffer-offset* 0)))
          (t
           (list :error "No error location available")))))

(defun signal-compiler-warning (cstring args severity orig-fn)
  (signal 'compiler-condition
          :severity severity
          :message (apply #'format nil cstring args)
          :location (compiler-note-location))
  (apply orig-fn cstring args))

(defun c-warn (cstring &rest args)
  (signal-compiler-warning cstring args :warning *orig-c-warn*))

(defun c-style-warn (cstring &rest args)
  (dynamic-flet ((sys::c-warn *orig-c-warn*))
    (signal-compiler-warning cstring args :style-warning *orig-c-style-warn*)))

(defun c-error (&rest args)
  (signal 'compiler-condition
          :severity :error
          :message (apply #'format nil
                          (if (= (length args) 3)
                              (cdr args)
                              args))
          :location (compiler-note-location))
  (apply *orig-c-error* args))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-notification-condition))
    (dynamic-flet ((system::c-warn #'c-warn)
                   (system::c-style-warn #'c-style-warn)
                   (system::c-error #'c-error))
      (funcall function))))

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (signal 'compiler-condition
          :original-condition condition
          :severity :warning
          :message (princ-to-string condition)
          :location (compiler-note-location)))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (with-compilation-unit ()
      (multiple-value-bind (fasl-file warningsp failurep)
          (compile-file input-file 
                        :output-file output-file
                        :external-format external-format)
        (values fasl-file warningsp
                (or failurep 
                    (and load-p 
                         (not (load fasl-file)))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore filename line column policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-offset* position))
      (funcall (compile nil (read-from-string
                             (format nil "(~S () ~A)" 'lambda string))))
      t)))

;;;; Portable XREF from the CMU AI repository.

(setq pxref::*handle-package-forms* '(cl:in-package))

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      pxref:list-callers)
(defxref who-references pxref:list-readers)
(defxref who-binds      pxref:list-setters)
(defxref who-sets       pxref:list-setters)
(defxref list-callers   pxref:list-callers)
(defxref list-callees   pxref:list-callees)

(defun xref-results (symbols)
  (let ((xrefs '()))
    (dolist (symbol symbols)
      (push (fspec-location symbol symbol) xrefs))
    xrefs))

(when (find-package :swank-loader)
  (setf (symbol-function (intern "USER-INIT-FILE" :swank-loader))
        (lambda ()
          (let ((home (user-homedir-pathname)))
            (and (ext:probe-directory home)
                 (probe-file (format nil "~A/.swank.lisp"
                                     (namestring (truename home)))))))))

;;; Don't set *debugger-hook* to nil on break.
(ext:without-package-lock ()
 (defun break (&optional (format-string "Break") &rest args)
   (if (not sys::*use-clcs*)
       (progn
         (terpri *error-output*)
         (apply #'format *error-output*
                (concatenate 'string "*** - " format-string)
                args)
         (funcall ext:*break-driver* t))
       (let ((condition
              (make-condition 'simple-condition
                              :format-control format-string
                              :format-arguments args))
             ;;(*debugger-hook* nil)
             ;; Issue 91
             )
         (ext:with-restarts
             ((continue
               :report (lambda (stream)
                         (format stream (sys::text "Return from ~S loop")
                                 'break))
               ()))
           (with-condition-restarts condition (list (find-restart 'continue))
                                    (invoke-debugger condition)))))
   nil))

;;;; Inspecting

(defmethod emacs-inspect ((o t))
  (let* ((*print-array* nil) (*print-pretty* t)
         (*print-circle* t) (*print-escape* t)
         (*print-lines* custom:*inspect-print-lines*)
         (*print-level* custom:*inspect-print-level*)
         (*print-length* custom:*inspect-print-length*)
         (sys::*inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
         (tmp-pack (make-package (gensym "INSPECT-TMP-PACKAGE-")))
         (*package* tmp-pack)
         (sys::*inspect-unbound-value* (intern "#<unbound>" tmp-pack)))
    (let ((inspection (sys::inspect-backend o)))
      (append (list
               (format nil "~S~% ~A~{~%~A~}~%" o
                      (sys::insp-title inspection)
                      (sys::insp-blurb inspection)))
              (loop with count = (sys::insp-num-slots inspection)
                    for i below count
                    append (multiple-value-bind (value name)
                               (funcall (sys::insp-nth-slot inspection)
                                        i)
                             `((:value ,name) " = " (:value ,value)
                               (:newline))))))))

(defimplementation quit-lisp ()
  #+lisp=cl (ext:quit)
  #-lisp=cl (lisp:quit))


(defimplementation preferred-communication-style ()
  nil)

;;; FIXME
;;;
;;; Clisp 2.48 added experimental support for threads. Basically, you
;;; can use :SPAWN now, BUT:
;;; 
;;;   - there are problems with GC, and threads stuffed into weak
;;;     hash-tables as is the case for *THREAD-PLIST-TABLE*.
;;;
;;;     See test case at
;;;       http://thread.gmane.org/gmane.lisp.clisp.devel/20429
;;;
;;;     Even though said to be fixed, it's not:
;;;
;;;       http://thread.gmane.org/gmane.lisp.clisp.devel/20429/focus=20443
;;;
;;;   - The DYNAMIC-FLET above is an implementation technique that's
;;;     probably not sustainable in light of threads. This got to be
;;;     rewritten.
;;;
;;; TCR (2009-07-30)

#+#.(cl:if (cl:find-package "MP") '(:and) '(:or)) 
(progn
  (defimplementation spawn (fn &key name)
    (mp:make-thread fn :name name))

  (defvar *thread-plist-table-lock*
    (mp:make-mutex :name "THREAD-PLIST-TABLE-LOCK"))

  (defvar *thread-plist-table* (make-hash-table :weak :key)
    "A hashtable mapping threads to a plist.")

  (defvar *thread-id-counter* 0)

  (defimplementation thread-id (thread)
    (mp:with-mutex-lock (*thread-plist-table-lock*)
      (or (getf (gethash thread *thread-plist-table*) 'thread-id)
          (setf (getf (gethash thread *thread-plist-table*) 'thread-id)
                (incf *thread-id-counter*)))))

  (defimplementation find-thread (id)
    (find id (all-threads)
          :key (lambda (thread)
                 (getf (gethash thread *thread-plist-table*) 'thread-id))))

  (defimplementation thread-name (thread)
    ;; To guard against returning #<UNBOUND>.
    (princ-to-string (mp:thread-name thread)))

  (defimplementation thread-status (thread)
    (if (thread-alive-p thread)
        "RUNNING"
        "STOPPED"))

  (defimplementation make-lock (&key name)
    (mp:make-mutex :name name :recursive-p t))

  (defimplementation call-with-lock-held (lock function)
    (mp:with-mutex-lock (lock)
      (funcall function)))

  (defimplementation current-thread ()
    (mp:current-thread))

  (defimplementation all-threads ()
    (mp:list-threads))

  (defimplementation interrupt-thread (thread fn)
    (mp:thread-interrupt thread :function fn))

  (defimplementation kill-thread (thread)
    (mp:thread-interrupt thread :function t))

  (defimplementation thread-alive-p (thread)
    (mp:thread-active-p thread))

  (defvar *mailboxes-lock* (make-lock :name "MAILBOXES-LOCK"))
  (defvar *mailboxes* (list))

  (defstruct (mailbox (:conc-name mailbox.))
    thread
    (lock (make-lock :name "MAILBOX.LOCK"))
    (waitqueue  (mp:make-exemption :name "MAILBOX.WAITQUEUE"))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-mutex-lock (*mailboxes-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (lock (mailbox.lock mbox)))
      (mp:with-mutex-lock (lock)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (mp:exemption-broadcast (mailbox.waitqueue mbox)))))

  (defimplementation receive-if (test &optional timeout)
    (let* ((mbox (mailbox (current-thread)))
           (lock (mailbox.lock mbox)))
      (assert (or (not timeout) (eq timeout t)))
      (loop
       (check-slime-interrupts)
       (mp:with-mutex-lock (lock)
         (let* ((q (mailbox.queue mbox))
                (tail (member-if test q)))
           (when tail 
             (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
             (return (car tail))))
         (when (eq timeout t) (return (values nil t)))
         (mp:exemption-wait (mailbox.waitqueue mbox) lock :timeout 0.2))))))
 

;;;; Weak hashtables

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak :value args))

(defimplementation save-image (filename &optional restart-function)
  (let ((args `(,filename 
                ,@(if restart-function 
                      `((:init-function ,restart-function))))))
    (apply #'ext:saveinitmem args)))
