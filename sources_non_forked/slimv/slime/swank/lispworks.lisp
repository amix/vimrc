;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-lispworks.lisp --- LispWorks specific code for SLIME.
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage swank/lispworks
  (:use cl swank/backend))

(in-package swank/lispworks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defimplementation gray-package-name ()
  "STREAM")

(import-swank-mop-symbols :clos '(:slot-definition-documentation
                                  :slot-boundp-using-class
                                  :slot-value-using-class
                                  :slot-makunbound-using-class
                                  :eql-specializer
                                  :eql-specializer-object
                                  :compute-applicable-methods-using-classes))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))

(defun swank-mop:slot-boundp-using-class (class object slotd)
  (clos:slot-boundp-using-class class object
                                (clos:slot-definition-name slotd)))

(defun swank-mop:slot-value-using-class (class object slotd)
  (clos:slot-value-using-class class object
                               (clos:slot-definition-name slotd)))

(defun (setf swank-mop:slot-value-using-class) (value class object slotd)
  (setf (clos:slot-value-using-class class object
                                     (clos:slot-definition-name slotd))
        value))

(defun swank-mop:slot-makunbound-using-class (class object slotd)
  (clos:slot-makunbound-using-class class object
                                    (clos:slot-definition-name slotd)))

(defun swank-mop:compute-applicable-methods-using-classes (gf classes)
  (clos::compute-applicable-methods-from-classes gf classes))

;; lispworks doesn't have the eql-specializer class, it represents
;; them as a list of `(EQL ,OBJECT)
(deftype swank-mop:eql-specializer () 'cons)

(defun swank-mop:eql-specializer-object (eql-spec)
  (second eql-spec))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar *original-defimplementation* (macro-function 'defimplementation))
  (defmacro defimplementation (&whole whole name args &body body 
                               &environment env)
    (declare (ignore args body))
    `(progn
       (dspec:record-definition '(defun ,name) (dspec:location)
                                :check-redefinition-p nil)
       ,(funcall *original-defimplementation* whole env))))

;;; UTF8

(defimplementation string-to-utf8 (string)
  (ef:encode-lisp-string string '(:utf-8 :eol-style :lf)))

(defimplementation utf8-to-string (octets)
  (ef:decode-external-string octets '(:utf-8 :eol-style :lf)))

;;; TCP server

(defimplementation preferred-communication-style ()
  :spawn)

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (comm:socket-stream (comm:socket-stream-socket socket))))

(defimplementation create-socket (host port &key backlog)
  (multiple-value-bind (socket where errno)
      #-(or lispworks4.1 (and macosx lispworks4.3))
      (comm::create-tcp-socket-for-service port :address host
                                           :backlog (or backlog 5))
      #+(or lispworks4.1 (and macosx lispworks4.3))
      (comm::create-tcp-socket-for-service port)
    (cond (socket socket)
          (t (error 'network-error 
              :format-control "~A failed: ~A (~D)"
              :format-arguments (list where 
                                      (list #+unix (lw:get-unix-error errno))
                                      errno))))))

(defimplementation local-port (socket)
  (nth-value 1 (comm:get-socket-address (socket-fd socket))))

(defimplementation close-socket (socket)
  (comm::close-socket (socket-fd socket)))

(defimplementation accept-connection (socket 
                                      &key external-format buffering timeout)
  (declare (ignore buffering))
  (let* ((fd (comm::get-fd-from-socket socket)))
    (assert (/= fd -1))
    (cond ((not external-format)
           (make-instance 'comm:socket-stream
                          :socket fd
                          :direction :io
                          :read-timeout timeout
                          :element-type '(unsigned-byte 8)))
          (t
           (assert (valid-external-format-p external-format))
           (ecase (first external-format)
             ((:latin-1 :ascii)
              (make-instance 'comm:socket-stream
                             :socket fd
                             :direction :io
                             :read-timeout timeout
                             :element-type 'base-char))
             (:utf-8
              (make-flexi-stream 
               (make-instance 'comm:socket-stream
                              :socket fd
                              :direction :io
                              :read-timeout timeout
                              :element-type '(unsigned-byte 8))
               external-format)))))))

(defun make-flexi-stream (stream external-format)
  (unless (member :flexi-streams *features*)
    (error "Cannot use external format ~A~
            without having installed flexi-streams in the inferior-lisp."
           external-format))
  (funcall (read-from-string "FLEXI-STREAMS:MAKE-FLEXI-STREAM")
           stream
           :external-format
           (apply (read-from-string "FLEXI-STREAMS:MAKE-EXTERNAL-FORMAT")
                  external-format)))

;;; Coding Systems

(defun valid-external-format-p (external-format)
  (member external-format *external-format-to-coding-system*
          :test #'equal :key #'car))

(defvar *external-format-to-coding-system*
  '(((:latin-1 :eol-style :lf) 
     "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    ;;((:latin-1) "latin-1" "iso-latin-1" "iso-8859-1")
    ;;((:utf-8) "utf-8")
    ((:utf-8 :eol-style :lf) "utf-8-unix")
    ;;((:euc-jp) "euc-jp")
    ((:euc-jp :eol-style :lf) "euc-jp-unix")
    ;;((:ascii) "us-ascii")
    ((:ascii :eol-style :lf) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

;;; Unix signals

(defun sigint-handler ()
  (with-simple-restart  (continue "Continue from SIGINT handler.")
    (invoke-debugger "SIGINT")))

(defun make-sigint-handler (process)
  (lambda (&rest args)
    (declare (ignore args))
    (mp:process-interrupt process #'sigint-handler)))

(defun set-sigint-handler ()
  ;; Set SIGINT handler on Swank request handler thread.
  #-win32
  (sys::set-signal-handler +sigint+ 
                           (make-sigint-handler mp:*current-process*)))

#-win32 
(defimplementation install-sigint-handler (handler)
  (sys::set-signal-handler +sigint+
                           (let ((self mp:*current-process*))
                             (lambda (&rest args)
                               (declare (ignore args))
                               (mp:process-interrupt self handler)))))

(defimplementation getpid ()
  #+win32 (win32:get-current-process-id)
  #-win32 (system::getpid))

(defimplementation lisp-implementation-type-name ()
  "lispworks")

(defimplementation set-default-directory (directory)
  (namestring (hcl:change-directory directory)))

;;;; Documentation

(defun map-list (function list)
  "Map over proper and not proper lists."
  (loop for (car . cdr) on list
        collect (funcall function car) into result
        when (null cdr) return result
        when (atom cdr) return (nconc result (funcall function cdr))))

(defun replace-strings-with-symbols (tree)
  (map-list
   (lambda (x)
     (typecase x
       (list
        (replace-strings-with-symbols x))
       (symbol
        x)
       (string
        (intern x))
       (t
        (intern (write-to-string x)))))
   tree))
               
(defimplementation arglist (symbol-or-function)
  (let ((arglist (lw:function-lambda-list symbol-or-function)))
    (etypecase arglist
      ((member :dont-know) 
       :not-available)
      (list
       (replace-strings-with-symbols arglist)))))

(defimplementation function-name (function)
  (nth-value 2 (function-lambda-expression function)))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (walker:walk-form form))

(defun generic-function-p (object)
  (typep object 'generic-function))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((first-line (string) 
               (let ((pos (position #\newline string)))
                 (if (null pos) string (subseq string 0 pos))))
             (doc (kind &optional (sym symbol))
               (let ((string (or (documentation sym kind))))
                 (if string 
                     (first-line string)
                     :not-documented)))
             (maybe-push (property value)
               (when value
                 (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :generic-function (if (and (fboundp symbol)
                                  (generic-function-p (fdefinition symbol)))
                             (doc 'function)))
      (maybe-push
       :function (if (and (fboundp symbol)
                          (not (generic-function-p (fdefinition symbol))))
                     (doc 'function)))
      (maybe-push
       :setf (let ((setf-name (sys:underlying-setf-name `(setf ,symbol))))
               (if (fboundp setf-name)
                   (doc 'setf))))
      (maybe-push
       :class (if (find-class symbol nil) 
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol type)
  (ecase type
    (:variable (describe-symbol symbol))
    (:class (describe (find-class symbol)))
    ((:function :generic-function) (describe-function symbol))
    (:setf (describe-function (sys:underlying-setf-name `(setf ,symbol))))))

(defun describe-function (symbol)
  (cond ((fboundp symbol)
         (format t "(~A ~/pprint-fill/)~%~%~:[(not documented)~;~:*~A~]~%"
                 symbol
                 (lispworks:function-lambda-list symbol)
                 (documentation symbol 'function))
         (describe (fdefinition symbol)))
        (t (format t "~S is not fbound" symbol))))

(defun describe-symbol (sym)
  (format t "~A is a symbol in package ~A." sym (symbol-package sym))
  (when (boundp sym)
    (format t "~%~%Value: ~A" (symbol-value sym)))
  (let ((doc (documentation sym 'variable)))
    (when doc 
      (format t "~%~%Variable documentation:~%~A"  doc)))
  (when (fboundp sym)
    (describe-function sym)))

(defimplementation type-specifier-p (symbol)
  (or (ignore-errors
       (subtypep nil symbol))
      (not (eq (type-specifier-arglist symbol) :not-available))))

;;; Debugging

(defclass slime-env (env:environment) 
  ((debugger-hook :initarg :debugger-hoook)))

(defun slime-env (hook io-bindings) 
  (make-instance 'slime-env :name "SLIME Environment" 
                 :io-bindings io-bindings
                 :debugger-hoook hook))

(defmethod env-internals:environment-display-notifier 
    ((env slime-env) &key restarts condition)
  (declare (ignore restarts condition))
  (swank:swank-debugger-hook condition *debugger-hook*))

(defmethod env-internals:environment-display-debugger ((env slime-env))
  *debug-io*)

(defmethod env-internals:confirm-p ((e slime-env) &optional msg &rest args)
  (apply #'swank:y-or-n-p-in-emacs msg args))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook))
    (env:with-environment ((slime-env hook '()))
      (funcall fun))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setf (env:environment) (slime-env function '())))

(defvar *sldb-top-frame*)

(defun interesting-frame-p (frame)
  (cond ((or (dbg::call-frame-p frame)
             (dbg::derived-call-frame-p frame)
             (dbg::foreign-frame-p frame)
             (dbg::interpreted-call-frame-p frame))
         t)
        ((dbg::catch-frame-p frame) dbg:*print-catch-frames*)
        ((dbg::binding-frame-p frame) dbg:*print-binding-frames*)
        ((dbg::handler-frame-p frame) dbg:*print-handler-frames*)
        ((dbg::restart-frame-p frame) dbg:*print-restart-frames*)
        (t nil)))

(defun nth-next-frame (frame n)
  "Unwind FRAME N times."
  (do ((frame frame (dbg::frame-next frame))
       (i n (if (interesting-frame-p frame) (1- i) i)))
      ((or (not frame)
           (and (interesting-frame-p frame) (zerop i)))
       frame)))

(defun nth-frame (index)
  (nth-next-frame *sldb-top-frame* index))

(defun find-top-frame ()
  "Return the most suitable top-frame for the debugger."
  (flet ((find-named-frame (name)
           (do ((frame (dbg::debugger-stack-current-frame
                        dbg::*debugger-stack*)
                       (nth-next-frame frame 1)))
               ((or (null frame)        ; no frame found!
                    (and (dbg::call-frame-p frame)
                         (eq (dbg::call-frame-function-name frame) 
                             name)))
                (nth-next-frame frame 1)))))
    (or (find-named-frame 'invoke-debugger)
        (find-named-frame 'swank::safe-backtrace)
        ;; if we can't find a likely top frame, take any old frame
        ;; at the top
        (dbg::debugger-stack-current-frame dbg::*debugger-stack*))))
  
(defimplementation call-with-debugging-environment (fn)
  (dbg::with-debugger-stack ()
    (let ((*sldb-top-frame* (find-top-frame)))
      (funcall fn))))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum))
	(backtrace '()))
    (do ((frame (nth-frame start) (dbg::frame-next frame))
	 (i start))
	((or (not frame) (= i end)) (nreverse backtrace))
      (when (interesting-frame-p frame)
	(incf i)
	(push frame backtrace)))))

(defun frame-actual-args (frame)
  (let ((*break-on-signals* nil)
        (kind nil))
    (loop for arg in (dbg::call-frame-arglist frame)
          if (eq kind '&rest)
          nconc (handler-case
                    (dbg::dbg-eval arg frame)
                  (error (e) (list (format nil "<~A>" arg))))
          and do (loop-finish)
          else
          if (member arg '(&rest &optional &key))
          do (setq kind arg)
          else
          nconc
          (handler-case
              (nconc (and (eq kind '&key)
                          (list (cond ((symbolp arg)
                                       (intern (symbol-name arg) :keyword))
                                      ((and (consp arg) (symbolp (car arg)))
                                       (intern (symbol-name (car arg))
                                               :keyword))
                                      (t (caar arg)))))
                     (list (dbg::dbg-eval
                            (cond ((symbolp arg) arg)
                                  ((and (consp arg) (symbolp (car arg)))
                                   (car arg))
                                  (t (cadar arg)))
                            frame)))
            (error (e) (list (format nil "<~A>" arg)))))))

(defimplementation print-frame (frame stream)
  (cond ((dbg::call-frame-p frame)
         (prin1 (cons (dbg::call-frame-function-name frame)
                      (frame-actual-args frame))
                stream))
        (t (princ frame stream))))

(defun frame-vars (frame)
  (first (dbg::frame-locals-format-list frame #'list 75 0)))

(defimplementation frame-locals (n)
  (let ((frame (nth-frame n)))
    (if (dbg::call-frame-p frame)
        (mapcar (lambda (var)
                  (destructuring-bind (name value symbol location) var
                    (declare (ignore name location))
                    (list :name symbol :id 0
                          :value value)))
                (frame-vars frame)))))

(defimplementation frame-var-value (frame var)
  (let ((frame (nth-frame frame)))
    (destructuring-bind (_n value _s _l) (nth var (frame-vars frame))
      (declare (ignore _n _s _l))
      value)))

(defimplementation frame-source-location (frame)
  (let ((frame (nth-frame frame))
        (callee (if (plusp frame) (nth-frame (1- frame)))))
    (if (dbg::call-frame-p frame)
	(let ((dspec (dbg::call-frame-function-name frame))
              (cname (and (dbg::call-frame-p callee)
                          (dbg::call-frame-function-name callee)))
              (path (and (dbg::call-frame-p frame)
                         (dbg::call-frame-edit-path frame))))
	  (if dspec
              (frame-location dspec cname path))))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::dbg-eval form frame)))

(defun function-name-package (name)
  (typecase name
    (null nil)
    (symbol (symbol-package name))
    ((cons (eql hcl:subfunction))
     (destructuring-bind (name parent) (cdr name)
       (declare (ignore name))
       (function-name-package parent)))
    ((cons (eql lw:top-level-form)) nil)
    (t nil)))

(defimplementation frame-package (frame-number)
  (let ((frame (nth-frame frame-number)))
    (if (dbg::call-frame-p frame)
        (function-name-package (dbg::call-frame-function-name frame)))))

(defimplementation return-from-frame (frame-number form)
  (let* ((frame (nth-frame frame-number))
         (return-frame (dbg::find-frame-for-return frame)))
    (dbg::dbg-return-from-call-frame frame form return-frame
                                     dbg::*debugger-stack*)))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::restart-frame frame :same-args t)))

(defimplementation disassemble-frame (frame-number)
  (let* ((frame (nth-frame frame-number)))
    (when (dbg::call-frame-p frame)
      (let ((function (dbg::get-call-frame-function frame)))
        (disassemble function)))))

;;; Definition finding

(defun frame-location (dspec callee-name edit-path)
  (let ((infos (dspec:find-dspec-locations dspec)))
    (cond (infos 
           (destructuring-bind ((rdspec location) &rest _) infos
             (declare (ignore _))
             (let ((name (and callee-name (symbolp callee-name)
                              (string callee-name)))
                   (path (edit-path-to-cmucl-source-path edit-path)))
               (make-dspec-location rdspec location
                                    `(:call-site ,name :edit-path ,path)))))
          (t 
           (list :error (format nil "Source location not available for: ~S" 
                                dspec))))))

;; dbg::call-frame-edit-path is not documented but lets assume the
;; binary representation of the integer EDIT-PATH should be
;; interpreted as a sequence of CAR or CDR.  #b1111010 is roughly the
;; same as cadadddr.  Something is odd with the highest bit.
(defun edit-path-to-cmucl-source-path (edit-path)
  (and edit-path
       (cons 0
             (let ((n -1))
               (loop for i from (1- (integer-length edit-path)) downto 0
                     if (logbitp i edit-path) do (incf n)
                     else collect (prog1 n (setq n 0)))))))

;; (edit-path-to-cmucl-source-path #b1111010) => (0 3 1)

(defimplementation find-definitions (name)
  (let ((locations (dspec:find-name-locations dspec:*dspec-classes* name)))
    (loop for (dspec location) in locations
          collect (list dspec (make-dspec-location dspec location)))))


;;; Compilation 

(defmacro with-swank-compilation-unit ((location &rest options) &body body)
  (lw:rebinding (location)
    `(let ((compiler::*error-database* '()))
       (with-compilation-unit ,options
         (multiple-value-prog1 (progn ,@body)
           (signal-error-data-base compiler::*error-database* 
                                   ,location)
           (signal-undefined-functions compiler::*unknown-functions* 
                                       ,location))))))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (with-swank-compilation-unit (input-file)
    (compile-file input-file 
                  :output-file output-file
                  :load load-p 
                  :external-format external-format)))

(defvar *within-call-with-compilation-hooks* nil
  "Whether COMPILE-FILE was called from within CALL-WITH-COMPILATION-HOOKS.")

(defvar *undefined-functions-hash* nil
  "Hash table to map info about undefined functions to pathnames.")

(lw:defadvice (compile-file compile-file-and-collect-notes :around)
    (pathname &rest rest)
  (multiple-value-prog1 (apply #'lw:call-next-advice pathname rest)
    (when *within-call-with-compilation-hooks*
      (maphash (lambda (unfun dspecs)
                 (dolist (dspec dspecs)
                   (let ((unfun-info (list unfun dspec)))
                     (unless (gethash unfun-info *undefined-functions-hash*)
                       (setf (gethash unfun-info *undefined-functions-hash*)
                               pathname)))))
               compiler::*unknown-functions*))))

(defimplementation call-with-compilation-hooks (function)
  (let ((compiler::*error-database* '())
        (*undefined-functions-hash* (make-hash-table :test 'equal))
        (*within-call-with-compilation-hooks* t))
    (with-compilation-unit ()
      (prog1 (funcall function)
        (signal-error-data-base compiler::*error-database*)
        (signal-undefined-functions compiler::*unknown-functions*)))))

(defun map-error-database (database fn)
  (loop for (filename . defs) in database do
	(loop for (dspec . conditions) in defs do
	      (dolist (c conditions)
                (multiple-value-bind (condition path)
                    (if (consp c) (values (car c) (cdr c)) (values c nil))
                  (funcall fn filename dspec condition path))))))

(defun lispworks-severity (condition)
  (cond ((not condition) :warning)
	(t (etypecase condition
             #-(or lispworks4 lispworks5)
             (conditions:compiler-note :note)
	     (error :error)
	     (style-warning :warning)
	     (warning :warning)))))

(defun signal-compiler-condition (message location condition)
  (check-type message string)
  (signal 
   (make-instance 'compiler-condition :message message 
		  :severity (lispworks-severity condition) 
		  :location location
		  :original-condition condition)))

(defvar *temp-file-format* '(:utf-8 :eol-style :lf))

(defun compile-from-temp-file (string filename)
  (unwind-protect
       (progn
	 (with-open-file (s filename :direction :output
                                     :if-exists :supersede
                                     :external-format *temp-file-format*)

	   (write-string string s)
	   (finish-output s))
         (multiple-value-bind (binary-filename warnings? failure?)
             (compile-file filename :load t
                           :external-format *temp-file-format*)
           (declare (ignore warnings?))
           (when binary-filename
             (delete-file binary-filename))
           (not failure?)))
    (delete-file filename)))

(defun dspec-function-name-position (dspec fallback)
  (etypecase dspec
    (cons (let ((name (dspec:dspec-primary-name dspec)))
            (typecase name
              ((or symbol string) 
               (list :function-name (string name)))
              (t fallback))))
    (null fallback)
    (symbol (list :function-name (string dspec)))))

(defmacro with-fairly-standard-io-syntax (&body body)
  "Like WITH-STANDARD-IO-SYNTAX but preserve *PACKAGE* and *READTABLE*."
  (let ((package (gensym))
        (readtable (gensym)))
    `(let ((,package *package*)
           (,readtable *readtable*))
      (with-standard-io-syntax
        (let ((*package* ,package)
              (*readtable* ,readtable))
          ,@body)))))

(defun skip-comments (stream)
  (let ((pos0 (file-position stream)))
    (cond ((equal (ignore-errors (list (read-delimited-list #\( stream)))
                  '(()))
           (file-position stream (1- (file-position stream))))
          (t (file-position stream pos0)))))

#-(or lispworks4.1 lispworks4.2) ; no dspec:parse-form-dspec prior to 4.3
(defun dspec-stream-position (stream dspec)
  (with-fairly-standard-io-syntax
    (loop (let* ((pos (progn (skip-comments stream) (file-position stream)))
                 (form (read stream nil '#1=#:eof)))
            (when (eq form '#1#)
              (return nil))
            (labels ((check-dspec (form)
                       (when (consp form)
                         (let ((operator (car form)))
                           (case operator
                             ((progn)
                              (mapcar #'check-dspec
                                      (cdr form)))
                             ((eval-when locally macrolet symbol-macrolet)
                              (mapcar #'check-dspec
                                      (cddr form)))
                             ((in-package)
                              (let ((package (find-package (second form))))
                                (when package
                                  (setq *package* package))))
                             (otherwise
                              (let ((form-dspec (dspec:parse-form-dspec form)))
                                (when (dspec:dspec-equal dspec form-dspec)
                                  (return pos)))))))))
              (check-dspec form))))))

(defun dspec-file-position (file dspec)
  (let* ((*compile-file-pathname* (pathname file))
         (*compile-file-truename* (truename *compile-file-pathname*))
         (*load-pathname* *compile-file-pathname*)
         (*load-truename* *compile-file-truename*))
    (with-open-file (stream file)
      (let ((pos 
             #-(or lispworks4.1 lispworks4.2)
             (ignore-errors (dspec-stream-position stream dspec))))
        (if pos
            (list :position (1+ pos))
            (dspec-function-name-position dspec `(:position 1)))))))

(defun emacs-buffer-location-p (location)
  (and (consp location)
       (eq (car location) :emacs-buffer)))

(defun make-dspec-location (dspec location &optional hints)
  (etypecase location
    ((or pathname string)
     (multiple-value-bind (file err) 
         (ignore-errors (namestring (truename location)))
       (if err
           (list :error (princ-to-string err))
           (make-location `(:file ,file)
                          (dspec-file-position file dspec)
                          hints))))
    (symbol 
     `(:error ,(format nil "Cannot resolve location: ~S" location)))
    ((satisfies emacs-buffer-location-p)
     (destructuring-bind (_ buffer offset) location
       (declare (ignore _))
       (make-location `(:buffer ,buffer)
                      (dspec-function-name-position dspec `(:offset ,offset 0))
                      hints)))))

(defun make-dspec-progenitor-location (dspec location edit-path)
  (let ((canon-dspec (dspec:canonicalize-dspec dspec)))
    (make-dspec-location
     (if canon-dspec
         (if (dspec:local-dspec-p canon-dspec)
             (dspec:dspec-progenitor canon-dspec)
             canon-dspec)
         nil)
     location
     (if edit-path
         (list :edit-path (edit-path-to-cmucl-source-path edit-path))))))

(defun signal-error-data-base (database &optional location)
  (map-error-database 
   database
   (lambda (filename dspec condition edit-path)
     (signal-compiler-condition
      (format nil "~A" condition)
      (make-dspec-progenitor-location dspec (or location filename) edit-path)
      condition))))

(defun unmangle-unfun (symbol)
  "Converts symbols like 'SETF::|\"CL-USER\" \"GET\"| to
function names like \(SETF GET)."
  (cond ((sys::setf-symbol-p symbol)
         (sys::setf-pair-from-underlying-name symbol))
        (t symbol)))
                    
(defun signal-undefined-functions (htab &optional filename)
  (maphash (lambda (unfun dspecs)
	     (dolist (dspec dspecs)
	       (signal-compiler-condition 
		(format nil "Undefined function ~A" (unmangle-unfun unfun))
		(make-dspec-progenitor-location 
                 dspec
                 (or filename
                     (gethash (list unfun dspec) *undefined-functions-hash*))
                 nil)
		nil)))
	   htab))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore filename line column policy))
  (assert buffer)
  (assert position)
  (let* ((location (list :emacs-buffer buffer position))
         (tmpname (hcl:make-temp-file nil "lisp")))
    (with-swank-compilation-unit (location)
      (compile-from-temp-file 
       (with-output-to-string (s)
         (let ((*print-radix* t))
           (print `(eval-when (:compile-toplevel)
                     (setq dspec::*location* (list ,@location)))
                  s))
         (write-string string s))
       tmpname))))

;;; xref

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      hcl:who-calls)
(defxref who-macroexpands hcl:who-calls) ; macros are in the calls table too
(defxref calls-who      hcl:calls-who)
(defxref list-callers   list-callers-internal)
(defxref list-callees   list-callees-internal)

(defun list-callers-internal (name)
  (let ((callers (make-array 100
                             :fill-pointer 0
                             :adjustable t)))
    (hcl:sweep-all-objects
     #'(lambda (object)
         (when (and #+Harlequin-PC-Lisp (low:compiled-code-p object)
                    #+Harlequin-Unix-Lisp (sys:callablep object)
                    #-(or Harlequin-PC-Lisp Harlequin-Unix-Lisp) 
                    (sys:compiled-code-p object)
                    (system::find-constant$funcallable name object))
           (vector-push-extend object callers))))
    ;; Delay dspec:object-dspec until after sweep-all-objects
    ;; to reduce allocation problems.
    (loop for object across callers
          collect (if (symbolp object)
		      (list 'function object)
                      (or (dspec:object-dspec object) object)))))

(defun list-callees-internal (name)
  (let ((callees '()))
    (system::find-constant$funcallable
     'junk name
     :test #'(lambda (junk constant)
               (declare (ignore junk))
               (when (and (symbolp constant)
                          (fboundp constant))
                 (pushnew (list 'function constant) callees :test 'equal))
               ;; Return nil so we iterate over all constants.
               nil))
    callees))

;; only for lispworks 4.2 and above
#-lispworks4.1
(progn
  (defxref who-references hcl:who-references)
  (defxref who-binds      hcl:who-binds)
  (defxref who-sets       hcl:who-sets))

(defimplementation who-specializes (classname)
  (let ((class (find-class classname nil)))
    (when class
      (let ((methods (clos:class-direct-methods class)))
        (xref-results (mapcar #'dspec:object-dspec methods))))))

(defun xref-results (dspecs)
  (flet ((frob-locs (dspec locs)
           (cond (locs
                  (loop for (name loc) in locs
                        collect (list name (make-dspec-location name loc))))
                 (t `((,dspec (:error "Source location not available")))))))
    (loop for dspec in dspecs
          append (frob-locs dspec (dspec:dspec-definition-locations dspec)))))

;;; Inspector

(defmethod emacs-inspect ((o t))
  (lispworks-inspect o))

(defmethod emacs-inspect ((o function))
  (lispworks-inspect o))

;; FIXME: slot-boundp-using-class in LW works with names so we can't
;; use our method in swank.lisp.
(defmethod emacs-inspect ((o standard-object))
  (lispworks-inspect o))

(defun lispworks-inspect (o)
  (multiple-value-bind (names values _getter _setter type)
      (lw:get-inspector-values o nil)
    (declare (ignore _getter _setter))
            (append 
             (label-value-line "Type" type)
             (loop for name in names
                   for value in values
                   append (label-value-line name value)))))

;;; Miscellaneous

(defimplementation quit-lisp ()
  (lispworks:quit))

;;; Tracing

(defun parse-fspec (fspec)
  "Return a dspec for FSPEC."
  (ecase (car fspec)
    ((:defmethod) `(method ,(cdr fspec)))))

(defun tracedp (dspec) 
  (member dspec (eval '(trace)) :test #'equal))

(defun toggle-trace-aux (dspec)
  (cond ((tracedp dspec)
         (eval `(untrace ,dspec))
         (format nil "~S is now untraced." dspec))
        (t
         (eval `(trace (,dspec)))
         (format nil "~S is now traced." dspec))))

(defimplementation toggle-trace (fspec)
  (toggle-trace-aux (parse-fspec fspec)))

;;; Multithreading

(defimplementation initialize-multiprocessing (continuation)
  (cond ((not mp::*multiprocessing*)
         (push (list "Initialize SLIME" '() continuation) 
               mp:*initial-processes*)
         (mp:initialize-multiprocessing))
        (t (funcall continuation))))

(defimplementation spawn (fn &key name)
  (mp:process-run-function name () fn))

(defvar *id-lock* (mp:make-lock))
(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (mp:with-lock (*id-lock*)
    (or (getf (mp:process-plist thread) 'id)
        (setf (getf (mp:process-plist thread) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id (mp:list-all-processes) 
        :key (lambda (p) (getf (mp:process-plist p) 'id))))

(defimplementation thread-name (thread)
  (mp:process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A ~D" 
          (mp:process-whostate thread)
          (mp:process-priority thread)))

(defimplementation make-lock (&key name)
  (mp:make-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (mp:with-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mp:*current-process*)

(defimplementation all-threads ()
  (mp:list-all-processes))

(defimplementation interrupt-thread (thread fn)
  (mp:process-interrupt thread fn))

(defimplementation kill-thread (thread)
  (mp:process-kill thread))

(defimplementation thread-alive-p (thread)
  (mp:process-alive-p thread))

(defstruct (mailbox (:conc-name mailbox.)) 
  (mutex (mp:make-lock :name "thread mailbox"))
  (queue '() :type list))

(defvar *mailbox-lock* (mp:make-lock))

(defun mailbox (thread)
  (mp:with-lock (*mailbox-lock*)
    (or (getf (mp:process-plist thread) 'mailbox)
        (setf (getf (mp:process-plist thread) 'mailbox)
              (make-mailbox)))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox mp:*current-process*))
         (lock (mailbox.mutex mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
     (check-slime-interrupts)
     (mp:with-lock (lock "receive-if/try")
       (let* ((q (mailbox.queue mbox))
              (tail (member-if test q)))
         (when tail
           (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
           (return (car tail)))))
     (when (eq timeout t) (return (values nil t)))
     (mp:process-wait-with-timeout 
      "receive-if" 0.3 (lambda () (some test (mailbox.queue mbox)))))))

(defimplementation send (thread message)
  (let ((mbox (mailbox thread)))
    (mp:with-lock ((mailbox.mutex mbox))
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(let ((alist '())
      (lock (mp:make-lock :name "register-thread")))

  (defimplementation register-thread (name thread)
    (declare (type symbol name))
    (mp:with-lock (lock)
      (etypecase thread
        (null 
         (setf alist (delete name alist :key #'car)))
        (mp:process
         (let ((probe (assoc name alist)))
           (cond (probe (setf (cdr probe) thread))
                 (t (setf alist (acons name thread alist))))))))
    nil)

  (defimplementation find-registered (name)
    (mp:with-lock (lock)
      (cdr (assoc name alist)))))


(defimplementation set-default-initial-binding (var form)
  (setq mp:*process-initial-bindings* 
        (acons var `(eval (quote ,form))
               mp:*process-initial-bindings* )))

(defimplementation thread-attributes (thread)
  (list :priority (mp:process-priority thread)
        :idle (mp:process-idle-time thread)))


;;;; Weak hashtables

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak-kind :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak-kind :value args))
