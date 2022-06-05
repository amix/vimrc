;;;;                  -*- indent-tabs-mode: nil; outline-regexp: ";;;;;* "; -*-
;;;
;;; swank-allegro.lisp --- Allegro CL specific code for SLIME.
;;;
;;; Created 2003
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage swank/allegro
  (:use cl swank/backend))

(in-package swank/allegro)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)
  (require :process)
  #+(version>= 8 2)
  (require 'lldb))

(defimplementation gray-package-name ()
  '#:excl)

;;; swank-mop

(import-swank-mop-symbols :clos '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))


;;;; UTF8

(define-symbol-macro utf8-ef
    (load-time-value
     (excl:crlf-base-ef (excl:find-external-format :utf-8))
     t))

(defimplementation string-to-utf8 (s)
  (excl:string-to-octets s :external-format utf8-ef
                         :null-terminate nil))

(defimplementation utf8-to-string (u)
  (excl:octets-to-string u :external-format utf8-ef))


;;;; TCP Server

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port &key backlog)
  (socket:make-socket :connect :passive :local-port port
                      :local-host host :reuse-address t
                      :backlog (or backlog 5)))

(defimplementation local-port (socket)
  (socket:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket &key external-format buffering
                                             timeout)
  (declare (ignore buffering timeout))
  (let ((s (socket:accept-connection socket :wait t)))
    (when external-format
      (setf (stream-external-format s) external-format))
    s))

(defimplementation socket-fd (stream)
  (excl::stream-input-handle stream))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix"
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")
    (:euc-jp "euc-jp" "euc-jp-unix")
    (:us-ascii "us-ascii" "us-ascii-unix")
    (:emacs-mule "emacs-mule" "emacs-mule-unix")))

(defimplementation find-external-format (coding-system)
  (let ((e (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                      *external-format-to-coding-system*)))
    (and e (excl:crlf-base-ef
            (excl:find-external-format (car e)
                                       :try-variant t)))))

;;;; Unix signals

(defimplementation getpid ()
  (excl::getpid))

(defimplementation lisp-implementation-type-name ()
  "allegro")

(defimplementation set-default-directory (directory)
  (let* ((dir (namestring (truename (merge-pathnames directory)))))
    (setf *default-pathname-defaults* (pathname (excl:chdir dir)))
    dir))

(defimplementation default-directory ()
  (namestring (excl:current-directory)))

;;;; Misc

(defimplementation arglist (symbol)
  (handler-case (excl:arglist symbol)
    (simple-error () :not-available)))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  #+(version>= 8 0)
  (excl::walk-form form)
  #-(version>= 8 0)
  (excl::walk form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil)
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

(defimplementation type-specifier-p (symbol)
  (or (ignore-errors
       (subtypep nil symbol))
      (not (eq (type-specifier-arglist symbol) :not-available))))

(defimplementation function-name (f)
  (check-type f function)
  (cross-reference::object-to-function-name f))

;;;; Debugger

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*sldb-topframe* (find-topframe))
        (excl::*break-hook* nil))
    (funcall debugger-loop-fn)))

(defimplementation sldb-break-at-start (fname)
  ;; :print-before is kind of mis-used but we just want to stuff our
  ;; break form somewhere. This does not work for setf, :before and
  ;; :after methods, which need special syntax in the trace call, see
  ;; ACL's doc/debugging.htm chapter 10.
  (eval `(trace (,fname
                 :print-before
                 ((break "Function start breakpoint of ~A" ',fname)))))
  `(:ok ,(format nil "Set breakpoint at start of ~S" fname)))

(defun find-topframe ()
  (let ((magic-symbol (intern (symbol-name :swank-debugger-hook)
                              (find-package :swank)))
        (top-frame (excl::int-newest-frame (excl::current-thread))))
    (loop for frame = top-frame then (next-frame frame)
          for i from 0
          while (and frame (< i 30))
          when (eq (debugger:frame-name frame) magic-symbol)
            return (next-frame frame)
          finally (return top-frame))))

(defun next-frame (frame)
  (let ((next (excl::int-next-older-frame frame)))
    (cond ((not next) nil)
          ((debugger:frame-visible-p next) next)
          (t (next-frame next)))))

(defun nth-frame (index)
  (do ((frame *sldb-topframe* (next-frame frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (next-frame f)
	  for i from start below end
	  while f collect f)))

(defimplementation print-frame (frame stream)
  (debugger:output-frame stream frame :moderate))

(defimplementation frame-locals (index)
  (let ((frame (nth-frame index)))
    (loop for i from 0 below (debugger:frame-number-vars frame)
	  collect (list :name (debugger:frame-var-name frame i)
			:id 0
			:value (debugger:frame-var-value frame i)))))

(defimplementation frame-var-value (frame var)
  (let ((frame (nth-frame frame)))
    (debugger:frame-var-value frame var)))

(defimplementation disassemble-frame (index)
  (let ((frame (nth-frame index)))
    (multiple-value-bind (x fun xx xxx pc) (debugger::dyn-fd-analyze frame)
      (format t "pc: ~d (~s ~s ~s)~%fun: ~a~%" pc x xx xxx fun)
      (disassemble (debugger:frame-function frame)))))

(defimplementation frame-source-location (index)
  (let* ((frame (nth-frame index)))
    (multiple-value-bind (x fun xx xxx pc) (debugger::dyn-fd-analyze frame)
      (declare (ignore x xx xxx))
      (cond ((and pc
                  #+(version>= 8 2)
                  (pc-source-location fun pc)
                  #-(version>= 8 2)
                  (function-source-location fun)))
            (t ; frames for unbound functions etc end up here
             (cadr (car (fspec-definition-locations
                         (car (debugger:frame-expression frame))))))))))

(defun function-source-location (fun)
  (cadr (car (fspec-definition-locations
              (xref::object-to-function-name fun)))))

#+(version>= 8 2)
(defun pc-source-location (fun pc)
  (let* ((debug-info (excl::function-source-debug-info fun)))
    (cond ((not debug-info)
           (function-source-location fun))
          (t
           (let* ((code-loc (find-if (lambda (c)
                                       (<= (- pc (sys::natural-width))
                                           (let ((x (excl::ldb-code-pc c)))
                                             (or x -1))
                                           pc))
                                     debug-info)))
             (cond ((not code-loc)
                    (ldb-code-to-src-loc (aref debug-info 0)))
                   (t
                    (ldb-code-to-src-loc code-loc))))))))

#+(version>= 8 2)
(defun ldb-code-to-src-loc (code)
  (declare (optimize debug))
  (let* ((func (excl::ldb-code-func code))
         (debug-info (excl::function-source-debug-info func))
         (start (loop for i from (excl::ldb-code-index code) downto 0
                      for bpt = (aref debug-info i)
                      for start = (excl::ldb-code-start-char bpt)
                      when start
                        return (if (listp start)
                                   (first start)
                                   start)))
         (src-file (excl:source-file func)))
    (cond (start
           (buffer-or-file-location src-file start))
          (func
           (let* ((debug-info (excl::function-source-debug-info func))
                  (whole (aref debug-info 0))
                  (paths (source-paths-of (excl::ldb-code-source whole)
                                          (excl::ldb-code-source code)))
                  (path (if paths (longest-common-prefix paths) '()))
                  (start 0))
             (buffer-or-file
              src-file
              (lambda (file)
                (make-location `(:file ,file)
                               `(:source-path (0 . ,path) ,start)))
              (lambda (buffer bstart)
                (make-location `(:buffer ,buffer)
                               `(:source-path (0 . ,path)
                                              ,(+ bstart start)))))))
          (t
           nil))))

(defun longest-common-prefix (sequences)
  (assert sequences)
  (flet ((common-prefix (s1 s2)
           (let ((diff-pos (mismatch s1 s2)))
             (if diff-pos (subseq s1 0 diff-pos) s1))))
    (reduce #'common-prefix sequences)))

(defun source-paths-of (whole part)
  (let ((result '()))
    (labels ((walk (form path)
               (cond ((eq form part)
                      (push (reverse path) result))
                     ((consp form)
                      (loop for i from 0 while (consp form) do
                            (walk (pop form) (cons i path)))))))
      (walk whole '())
      (reverse result))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    ;; let-bind lexical variables
    (let ((vars (loop for i below (debugger:frame-number-vars frame)
                      for name = (debugger:frame-var-name frame i)
                      if (typep name '(and symbol (not null) (not keyword)))
                      collect `(,name ',(debugger:frame-var-value frame i)))))
      (debugger:eval-form-in-context
       `(let* ,vars ,form)
       (debugger:environment-of-frame frame)))))

(defimplementation frame-package (frame-number)
  (let* ((frame (nth-frame frame-number))
         (exp (debugger:frame-expression frame)))
    (typecase exp
      ((cons symbol) (symbol-package (car exp)))
      ((cons (cons (eql :internal) (cons symbol)))
       (symbol-package (cadar exp))))))

(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return
      frame (debugger:eval-form-in-context
             form
             (debugger:environment-of-frame frame)))))

(defimplementation frame-restartable-p (frame)
  (handler-case (debugger:frame-retryable-p frame)
    (serious-condition (c)
      (funcall (read-from-string "swank::background-message")
               "~a ~a" frame (princ-to-string c))
      nil)))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (cond ((debugger:frame-retryable-p frame)
           (apply #'debugger:frame-retry frame (debugger:frame-function frame)
                  (cdr (debugger:frame-expression frame))))
          (t "Frame is not retryable"))))

;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename* nil)

(defun compiler-note-p (object)
  (member (type-of object) '(excl::compiler-note compiler::compiler-note)))

(defun redefinition-p (condition)
  (and (typep condition 'style-warning)
       (every #'char-equal "redefin" (princ-to-string condition))))

(defun compiler-undefined-functions-called-warning-p (object)
  (typep object 'excl:compiler-undefined-functions-called-warning))

(deftype compiler-note ()
  `(satisfies compiler-note-p))

(deftype redefinition ()
  `(satisfies redefinition-p))

(defun signal-compiler-condition (&rest args)
  (apply #'signal 'compiler-condition args))

(defun handle-compiler-warning (condition)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (cond ((and #-(version>= 10 0) (not *buffer-name*)
              (compiler-undefined-functions-called-warning-p condition))
         (handle-undefined-functions-warning condition))
        ((and (typep condition 'excl::compiler-note)
              (let ((format (slot-value condition 'excl::format-control)))
                (and (search "Closure" format)
                     (search "will be stack allocated" format))))
         ;; Ignore "Closure <foo> will be stack allocated" notes.
         ;; That occurs often but is usually uninteresting.
         )
        (t
         (signal-compiler-condition
          :original-condition condition
          :severity (etypecase condition
                      (redefinition  :redefinition)
                      (style-warning :style-warning)
                      (warning       :warning)
                      (compiler-note :note)
                      (reader-error  :read-error)
                      (error         :error))
          :message (format nil "~A" condition)
          :location (compiler-warning-location condition)))))

(defun condition-pathname-and-position (condition)
  (let* ((context #+(version>= 10 0)
                  (getf (slot-value condition 'excl::plist)
                        :source-context))
         (location-available (and context
                                  (excl::source-context-start-char context))))
    (cond (location-available
           (values (excl::source-context-pathname context)
                   (when-let (start-char (excl::source-context-start-char context))
                     (let ((position (if (listp start-char) ; HACK
                                         (first start-char)
                                         start-char)))
                       (if (typep condition 'excl::compiler-free-reference-warning)
                           position
                           (1+ position))))))
          ((typep condition 'reader-error)
           (let ((pos  (car (last (slot-value condition 'excl::format-arguments))))
                 (file (pathname (stream-error-stream condition))))
             (when (integerp pos)
               (values file pos))))
          (t
           (let ((loc (getf (slot-value condition 'excl::plist) :loc)))
             (when loc
               (destructuring-bind (file . pos) loc
                 (let ((start (if (consp pos) ; 8.2 and newer
                                  #+(version>= 10 1)
                                  (if (typep condition 'excl::compiler-inconsistent-name-usage-warning)
                                      (second pos)
                                      (first pos))
                                  #-(version>= 10 1)
                                  (first pos)
                                  pos)))
                   (values file start)))))))))

(defun compiler-warning-location (condition)
  (multiple-value-bind (pathname position)
      (condition-pathname-and-position condition)
    (cond (*buffer-name*
           (make-location
            (list :buffer *buffer-name*)
            (if position
                (list :offset 1 (1- position))
                (list :offset *buffer-start-position* 0))))
          (pathname
           (make-location
            (list :file (namestring (truename pathname)))
            #+(version>= 10 1)
            (list :offset 1 position)
            #-(version>= 10 1)
            (list :position (1+ position))))
          (t
           (make-error-location "No error location available.")))))

;; TODO: report it as a bug to Franz that the condition's plist
;; slot contains (:loc nil).
(defun handle-undefined-functions-warning (condition)
  (let ((fargs (slot-value condition 'excl::format-arguments)))
    (loop for (fname . locs) in (car fargs) do
          (dolist (loc locs)
            (multiple-value-bind (pos file) (ecase (length loc)
                                              (2 (values-list loc))
                                              (3 (destructuring-bind
                                                       (start end file) loc
                                                   (declare (ignore end))
                                                   (values start file))))
              (signal-compiler-condition
               :original-condition condition
               :severity :warning
               :message (format nil "Undefined function referenced: ~S"
                                fname)
               :location (make-location (list :file file)
                                        #+(version>= 9 0)
                                        (list :offset 1 pos)
                                        #-(version>= 9 0)
                                        (list :position (1+ pos)))))))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning       #'handle-compiler-warning)
                 (compiler-note #'handle-compiler-warning)
                 (reader-error  #'handle-compiler-warning))
    (funcall function)))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (handler-case
      (with-compilation-hooks ()
        (let ((*buffer-name* nil)
              (*compile-filename* input-file)
              #+(version>= 8 2)
              (compiler:save-source-level-debug-info-switch t)
              (excl:*load-source-file-info* t)
              #+(version>= 8 2)
              (excl:*load-source-debug-info* t))
          (compile-file *compile-filename*
                        :output-file output-file
                        :load-after-compile load-p
                        :external-format external-format)))
    (reader-error () (values nil nil t))))

(defun call-with-temp-file (fn)
  (let ((tmpname (system:make-temp-file-name)))
    (unwind-protect
         (with-open-file (file tmpname :direction :output :if-exists :error)
           (funcall fn file tmpname))
      (delete-file tmpname))))

(defvar *temp-file-map* (make-hash-table :test #'equal)
  "A mapping from tempfile names to Emacs buffer names.")

(defun write-tracking-preamble (stream file file-offset)
  "Instrument the top of the temporary file to be compiled.

The header tells allegro that any definitions compiled in the temp
file should be found in FILE exactly at FILE-OFFSET.  To get Allegro
to do this, this factors in the length of the inserted header itself."
  (with-standard-io-syntax
    (let* ((*package* (find-package :keyword))
           (source-pathname-form
             `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
                (cl:setq excl::*source-pathname*
                         (pathname ,(sys::frob-source-file file)))))
           (source-pathname-string (write-to-string source-pathname-form))
           (position-form-length-bound 160) ; should be enough for everyone
           (header-length (+ (length source-pathname-string)
                             position-form-length-bound))
           (position-form
             `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
                (cl:setq excl::*partial-source-file-p* ,(- file-offset
                                                           header-length
                                                           1 ; for the newline
                                                           ))))
           (position-form-string (write-to-string position-form))
           (padding-string (make-string (- position-form-length-bound
                                           (length position-form-string))
                                        :initial-element #\;)))
      (write-string source-pathname-string stream)
      (write-string position-form-string stream)
      (write-string padding-string stream)
      (write-char #\newline stream))))

(defun compile-from-temp-file (string buffer offset file)
  (call-with-temp-file
   (lambda (stream filename)
     (when (and file offset (probe-file file))
       (write-tracking-preamble stream file offset))
     (write-string string stream)
     (finish-output stream)
     (multiple-value-bind (binary-filename warnings? failure?)
         (let ((sys:*source-file-types* '(nil)) ; suppress .lisp extension
               #+(version>= 8 2)
               (compiler:save-source-level-debug-info-switch t)
               (excl:*redefinition-warnings* nil))
           (compile-file filename))
       (declare (ignore warnings?))
       (when binary-filename
         (let ((excl:*load-source-file-info* t)
               #+(version>= 8 2)
               (excl:*load-source-debug-info* t))
           excl::*source-pathname*
           (load binary-filename))
         (when (and buffer offset (or (not file)
                                      (not (probe-file file))))
           (setf (gethash (pathname stream) *temp-file-map*)
                 (list buffer offset)))
         (delete-file binary-filename))
       (not failure?)))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore line column policy))
  (handler-case
      (with-compilation-hooks ()
        (let ((*buffer-name* buffer)
              (*buffer-start-position* position)
              (*buffer-string* string))
          (compile-from-temp-file string buffer position filename)))
    (reader-error () nil)))

;;;; Definition Finding

(defun buffer-or-file (file file-fun buffer-fun)
  (let* ((probe (gethash file *temp-file-map*)))
    (cond (probe
           (destructuring-bind (buffer start) probe
             (funcall buffer-fun buffer start)))
          (t (funcall file-fun (namestring (truename file)))))))

(defun buffer-or-file-location (file offset)
  (buffer-or-file file
                  (lambda (filename)
                    (make-location `(:file ,filename)
                                   `(:position ,(1+ offset))))
                  (lambda (buffer start)
                    (make-location `(:buffer ,buffer)
                                   `(:offset ,start ,offset)))))

(defun fspec-primary-name (fspec)
  (etypecase fspec
    (symbol fspec)
    (list (fspec-primary-name (second fspec)))))

(defun find-definition-in-file (fspec type file top-level)
  (let* ((part
          (or (scm::find-definition-in-definition-group
               fspec type (scm:section-file :file file)
               :top-level top-level)
              (scm::find-definition-in-definition-group
               (fspec-primary-name fspec)
               type (scm:section-file :file file)
               :top-level top-level)))
         (start (and part
                     (scm::source-part-start part)))
         (pos (if start
                  (list :offset 1 start)
                  (list :function-name (string (fspec-primary-name fspec))))))
    (make-location (list :file (namestring (truename file)))
                   pos)))

(defun find-fspec-location (fspec type file top-level)
  (handler-case
      (etypecase file
        (pathname
         (let ((probe (gethash file *temp-file-map*)))
           (cond (probe
                  (destructuring-bind (buffer offset) probe
                    (make-location `(:buffer ,buffer)
                                   `(:offset ,offset 0))))
                 (t
                  (find-definition-in-file fspec type file top-level)))))
        ((member :top-level)
         (make-error-location "Defined at toplevel: ~A"
                              (fspec->string fspec))))
    (error (e)
      (make-error-location "Error: ~A" e))))

(defun fspec->string (fspec)
  (typecase fspec
    (symbol (let ((*package* (find-package :keyword)))
              (prin1-to-string fspec)))
    (list (format nil "(~A ~A)"
                  (prin1-to-string (first fspec))
                  (let ((*package* (find-package :keyword)))
                    (prin1-to-string (second fspec)))))
    (t (princ-to-string fspec))))

(defun fspec-definition-locations (fspec)
  (cond
    ((and (listp fspec) (eq (car fspec) :internal))
     (destructuring-bind (_internal next _n) fspec
       (declare (ignore _internal _n))
       (fspec-definition-locations next)))
    (t
     (let ((defs (excl::find-source-file fspec)))
       (when (and (null defs)
                  (listp fspec)
                  (string= (car fspec) '#:method))
         ;; If methods are defined in a defgeneric form, the source location is
         ;; recorded for the gf but not for the methods. Therefore fall back to
         ;; the gf as the likely place of definition.
         (setq defs (excl::find-source-file (second fspec))))
       (if (null defs)
           (list
            (list fspec
                  (make-error-location "Unknown source location for ~A"
                                       (fspec->string fspec))))
           (loop for (fspec type file top-level) in defs collect
                 (list (list type fspec)
                       (find-fspec-location fspec type file top-level))))))))

(defimplementation find-definitions (symbol)
  (fspec-definition-locations symbol))

(defimplementation find-source-location (obj)
  (first (rest (first (fspec-definition-locations obj)))))

;;;; XREF

(defmacro defxref (name relation name1 name2)
  `(defimplementation ,name (x)
    (xref-result (xref:get-relation ,relation ,name1 ,name2))))

(defxref who-calls        :calls       :wild x)
(defxref calls-who        :calls       x :wild)
(defxref who-references   :uses        :wild x)
(defxref who-binds        :binds       :wild x)
(defxref who-macroexpands :macro-calls :wild x)
(defxref who-sets         :sets        :wild x)

(defun xref-result (fspecs)
  (loop for fspec in fspecs
        append (fspec-definition-locations fspec)))

;; list-callers implemented by groveling through all fbound symbols.
;; Only symbols are considered.  Functions in the constant pool are
;; searched recursively.  Closure environments are ignored at the
;; moment (constants in methods are therefore not found).

(defun map-function-constants (function fn depth)
  "Call FN with the elements of FUNCTION's constant pool."
  (do ((i 0 (1+ i))
       (max (excl::function-constant-count function)))
      ((= i max))
    (let ((c (excl::function-constant function i)))
      (cond ((and (functionp c)
                  (not (eq c function))
                  (plusp depth))
             (map-function-constants c fn (1- depth)))
            (t
             (funcall fn c))))))

(defun in-constants-p (fun symbol)
  (map-function-constants fun
                          (lambda (c)
                            (when (eq c symbol)
                              (return-from in-constants-p t)))
                          3))

(defun function-callers (name)
  (let ((callers '()))
    (do-all-symbols (sym)
      (when (fboundp sym)
        (let ((fn (fdefinition sym)))
          (when (in-constants-p fn name)
            (push sym callers)))))
    callers))

(defimplementation list-callers (name)
  (xref-result (function-callers name)))

(defimplementation list-callees (name)
  (let ((result '()))
    (map-function-constants (fdefinition name)
                            (lambda (c)
                              (when (fboundp c)
                                (push c result)))
                            2)
    (xref-result result)))

;;;; Profiling

;; Per-function profiling based on description in
;;  http://www.franz.com/support/documentation/8.0/\
;;  doc/runtime-analyzer.htm#data-collection-control-2

(defvar *profiled-functions* ())
(defvar *profile-depth* 0)

(defmacro with-redirected-y-or-n-p (&body body)
  ;; If the profiler is restarted when the data from the previous
  ;; session is not reported yet, the user is warned via Y-OR-N-P.
  ;; As the CL:Y-OR-N-P question is (for some reason) not directly
  ;; sent to the Slime user, the function CL:Y-OR-N-P is temporarily
  ;; overruled.
  `(let* ((pkg       (find-package :common-lisp))
          (saved-pdl (excl::package-definition-lock pkg))
          (saved-ynp (symbol-function 'cl:y-or-n-p)))
     (setf (excl::package-definition-lock pkg) nil
           (symbol-function 'cl:y-or-n-p)
           (symbol-function (read-from-string "swank:y-or-n-p-in-emacs")))
     (unwind-protect
          (progn ,@body)
       (setf (symbol-function 'cl:y-or-n-p)      saved-ynp
             (excl::package-definition-lock pkg) saved-pdl))))

(defun start-acl-profiler ()
  (with-redirected-y-or-n-p
      (prof:start-profiler :type :time :count t
                           :start-sampling-p nil :verbose nil)))
(defun acl-profiler-active-p ()
  (not (eq (prof:profiler-status :verbose nil) :inactive)))

(defun stop-acl-profiler ()
  (prof:stop-profiler :verbose nil))

(excl:def-fwrapper profile-fwrapper (&rest args)
  ;; Ensures sampling is done during the execution of the function,
  ;; taking into account recursion.
  (declare (ignore args))
  (cond ((zerop *profile-depth*)
         (let ((*profile-depth* (1+ *profile-depth*)))
           (prof:start-sampling)
           (unwind-protect (excl:call-next-fwrapper)
             (prof:stop-sampling))))
        (t
         (excl:call-next-fwrapper))))

(defimplementation profile (fname)
  (unless (acl-profiler-active-p)
    (start-acl-profiler))
  (excl:fwrap fname 'profile-fwrapper 'profile-fwrapper)
  (push fname *profiled-functions*))

(defimplementation profiled-functions ()
  *profiled-functions*)

(defimplementation unprofile (fname)
  (excl:funwrap fname 'profile-fwrapper)
  (setq *profiled-functions* (remove fname *profiled-functions*)))

(defimplementation profile-report ()
  (prof:show-flat-profile :verbose nil)
  (when *profiled-functions*
    (start-acl-profiler)))

(defimplementation profile-reset ()
  (when (acl-profiler-active-p)
    (stop-acl-profiler)
    (start-acl-profiler))
  "Reset profiling counters.")

;;;; Inspecting

(excl:without-redefinition-warnings
(defmethod emacs-inspect ((o t))
  (allegro-inspect o)))

(defmethod emacs-inspect ((o function))
  (allegro-inspect o))

(defmethod emacs-inspect ((o standard-object))
  (allegro-inspect o))

(defun allegro-inspect (o)
  (loop for (d dd) on (inspect::inspect-ctl o)
        append (frob-allegro-field-def o d)
        until (eq d dd)))

(defun frob-allegro-field-def (object def)
  (with-struct (inspect::field-def- name type access) def
    (ecase type
      ((:unsigned-word :unsigned-byte :unsigned-natural
                       :unsigned-long :unsigned-half-long
                       :unsigned-3byte :unsigned-long32)
       (label-value-line name (inspect::component-ref-v object access type)))
      ((:lisp :value :func)
       (label-value-line name (inspect::component-ref object access)))
      (:indirect
       (destructuring-bind (prefix count ref set) access
         (declare (ignore set prefix))
         (loop for i below (funcall count object)
               append (label-value-line (format nil "~A-~D" name i)
                                        (funcall ref object i))))))))

;;;; Multithreading

(defimplementation initialize-multiprocessing (continuation)
  (mp:start-scheduler)
  (funcall continuation))

(defimplementation spawn (fn &key name)
  (mp:process-run-function name fn))

(defvar *id-lock* (mp:make-process-lock :name "id lock"))
(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (mp:with-process-lock (*id-lock*)
    (or (getf (mp:process-property-list thread) 'id)
        (setf (getf (mp:process-property-list thread) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id mp:*all-processes*
        :key (lambda (p) (getf (mp:process-property-list p) 'id))))

(defimplementation thread-name (thread)
  (mp:process-name thread))

(defimplementation thread-status (thread)
  (princ-to-string (mp:process-whostate thread)))

(defimplementation thread-attributes (thread)
  (list :priority (mp:process-priority thread)
        :times-resumed (mp:process-times-resumed thread)))

(defimplementation make-lock (&key name)
  (mp:make-process-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (mp:with-process-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mp:*current-process*)

(defimplementation all-threads ()
  (copy-list mp:*all-processes*))

(defimplementation interrupt-thread (thread fn)
  (mp:process-interrupt thread fn))

(defimplementation kill-thread (thread)
  (mp:process-kill thread))

(defvar *mailbox-lock* (mp:make-process-lock :name "mailbox lock"))

(defstruct (mailbox (:conc-name mailbox.))
  (lock (mp:make-process-lock :name "process mailbox"))
  (queue '() :type list)
  (gate (mp:make-gate nil)))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (mp:with-process-lock (*mailbox-lock*)
    (or (getf (mp:process-property-list thread) 'mailbox)
        (setf (getf (mp:process-property-list thread) 'mailbox)
              (make-mailbox)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread)))
    (mp:with-process-lock ((mailbox.lock mbox))
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      (mp:open-gate (mailbox.gate mbox)))))

(defimplementation wake-thread (thread)
  (let* ((mbox (mailbox thread)))
    (mp:open-gate (mailbox.gate mbox))))

(defimplementation receive-if (test &optional timeout)
  (let ((mbox (mailbox mp:*current-process*)))
    (flet ((open-mailbox ()
             ;; this opens the mailbox and returns if has the message
             ;; we are expecting.  But first, check for interrupts.
             (check-slime-interrupts)
             (mp:with-process-lock ((mailbox.lock mbox))
               (let* ((q (mailbox.queue mbox))
                      (tail (member-if test q)))
                 (when tail
                   (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
                   (return-from receive-if (car tail)))
                 ;; ...if it doesn't, we close the gate (even if it
                 ;; was already closed)
                 (mp:close-gate (mailbox.gate mbox))))))
      (cond (timeout
             ;; open the mailbox and return asap
             (open-mailbox)
             (return-from receive-if (values nil t)))
            (t
             ;; wait until gate open, then open mailbox.  If there's
             ;; no message there, repeat forever.
             (loop
               (mp:process-wait
                "receive-if (waiting on gate)"
                #'mp:gate-open-p (mailbox.gate mbox))
               (open-mailbox)))))))

(let ((alist '())
      (lock (mp:make-process-lock :name "register-thread")))

  (defimplementation register-thread (name thread)
    (declare (type symbol name))
    (mp:with-process-lock (lock)
      (etypecase thread
        (null
         (setf alist (delete name alist :key #'car)))
        (mp:process
         (let ((probe (assoc name alist)))
           (cond (probe (setf (cdr probe) thread))
                 (t (setf alist (acons name thread alist))))))))
    nil)

  (defimplementation find-registered (name)
    (mp:with-process-lock (lock)
      (cdr (assoc name alist)))))

(defimplementation set-default-initial-binding (var form)
  (push (cons var form)
        #+(version>= 9 0)
        excl:*required-thread-bindings*
        #-(version>= 9 0)
        excl::required-thread-bindings))

(defimplementation quit-lisp ()
  (excl:exit 0 :quiet t))


;;Trace implementations
;;In Allegro 7.0, we have:
;; (trace <name>)
;; (trace ((method <name> <qualifier>? (<specializer>+))))
;; (trace ((labels <name> <label-name>)))
;; (trace ((labels (method <name> (<specializer>+)) <label-name>)))
;; <name> can be a normal name or a (setf name)

(defimplementation toggle-trace (spec)
  (ecase (car spec)
    ((setf)
     (toggle-trace-aux spec))
    (:defgeneric (toggle-trace-generic-function-methods (second spec)))
    ((setf :defmethod :labels :flet)
     (toggle-trace-aux (process-fspec-for-allegro spec)))
    (:call
     (destructuring-bind (caller callee) (cdr spec)
       (toggle-trace-aux callee
                         :inside (list (process-fspec-for-allegro caller)))))))

(defun tracedp (fspec)
  (member fspec (eval '(trace)) :test #'equal))

(defun toggle-trace-aux (fspec &rest args)
  (cond ((tracedp fspec)
         (eval `(untrace ,fspec))
         (format nil "~S is now untraced." fspec))
        (t
         (eval `(trace (,fspec ,@args)))
         (format nil "~S is now traced." fspec))))

(defun toggle-trace-generic-function-methods (name)
  (let ((methods (mop:generic-function-methods (fdefinition name))))
    (cond ((tracedp name)
           (eval `(untrace ,name))
           (dolist (method methods (format nil "~S is now untraced." name))
             (excl:funtrace (mop:method-function method))))
          (t
           (eval `(trace (,name)))
           (dolist (method methods (format nil "~S is now traced." name))
             (excl:ftrace (mop:method-function method)))))))

(defun process-fspec-for-allegro (fspec)
  (cond ((consp fspec)
         (ecase (first fspec)
           ((setf) fspec)
           ((:defun :defgeneric) (second fspec))
           ((:defmethod) `(method ,@(rest fspec)))
           ((:labels) `(labels ,(process-fspec-for-allegro (second fspec))
                         ,(third fspec)))
           ((:flet) `(flet ,(process-fspec-for-allegro (second fspec))
                       ,(third fspec)))))
        (t
         fspec)))


;;;; Weak hashtables

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak-keys t args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :values :weak args))

(defimplementation hash-table-weakness (hashtable)
  (cond ((excl:hash-table-weak-keys hashtable) :key)
        ((eq (excl:hash-table-values hashtable) :weak) :value)))



;;;; Character names

(defimplementation character-completion-set (prefix matchp)
  (loop for name being the hash-keys of excl::*name-to-char-table*
       when (funcall matchp prefix name)
       collect (string-capitalize name)))


;;;; wrap interface implementation

(defimplementation wrap (spec indicator &key before after replace)
  (let ((allegro-spec (process-fspec-for-allegro spec)))
    (excl:fwrap allegro-spec
                indicator
                (excl:def-fwrapper allegro-wrapper (&rest args)
                  (let (retlist completed)
                    (unwind-protect
                         (progn
                           (when before
                             (funcall before args))
                           (setq retlist (multiple-value-list
                                          (if replace
                                              (funcall replace args)
                                              (excl:call-next-fwrapper))))
                           (setq completed t)
                           (values-list retlist))
                      (when after
                        (funcall after (if completed
                                           retlist
                                           :exited-non-locally)))))))
    allegro-spec))

(defimplementation unwrap (spec indicator)
  (let ((allegro-spec (process-fspec-for-allegro spec)))
    (excl:funwrap allegro-spec indicator)
    allegro-spec))

(defimplementation wrapped-p (spec indicator)
  (getf (excl:fwrap-order (process-fspec-for-allegro spec)) indicator))
