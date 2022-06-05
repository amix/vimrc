;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ccl.lisp --- SLIME backend for Clozure CL.
;;;
;;; Copyright (C) 2003, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; This program is licensed under the terms of the Lisp Lesser GNU
;;; Public License, known as the LLGPL, and distributed with Clozure CL
;;; as the file "LICENSE".  The LLGPL consists of a preamble and the
;;; LGPL, which is distributed with Clozure CL as the file "LGPL".  Where
;;; these conflict, the preamble takes precedence.
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(defpackage swank/ccl
  (:use cl swank/backend))

(in-package swank/ccl)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (assert (and (= ccl::*openmcl-major-version* 1)
               (>= ccl::*openmcl-minor-version* 4))
          () "This file needs CCL version 1.4 or newer"))

(defimplementation gray-package-name ()
  "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (multiple-value-bind (ok err) (ignore-errors (require 'xref))
    (unless ok
      (warn "~a~%" err))))

;;; swank-mop

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   ccl:standard-slot-definition
   cl:method
   cl:standard-class
   ccl:eql-specializer
   openmcl-mop:finalize-inheritance
   openmcl-mop:compute-applicable-methods-using-classes
   ;; standard-class readers
   openmcl-mop:class-default-initargs
   openmcl-mop:class-direct-default-initargs
   openmcl-mop:class-direct-slots
   openmcl-mop:class-direct-subclasses
   openmcl-mop:class-direct-superclasses
   openmcl-mop:class-finalized-p
   cl:class-name
   openmcl-mop:class-precedence-list
   openmcl-mop:class-prototype
   openmcl-mop:class-slots
   openmcl-mop:specializer-direct-methods
   ;; eql-specializer accessors
   openmcl-mop:eql-specializer-object
   ;; generic function readers
   openmcl-mop:generic-function-argument-precedence-order
   openmcl-mop:generic-function-declarations
   openmcl-mop:generic-function-lambda-list
   openmcl-mop:generic-function-methods
   openmcl-mop:generic-function-method-class
   openmcl-mop:generic-function-method-combination
   openmcl-mop:generic-function-name
   ;; method readers
   openmcl-mop:method-generic-function
   openmcl-mop:method-function
   openmcl-mop:method-lambda-list
   openmcl-mop:method-specializers
   openmcl-mop:method-qualifiers
   ;; slot readers
   openmcl-mop:slot-definition-allocation
   openmcl-mop:slot-definition-documentation
   openmcl-mop:slot-value-using-class
   openmcl-mop:slot-definition-initargs
   openmcl-mop:slot-definition-initform
   openmcl-mop:slot-definition-initfunction
   openmcl-mop:slot-definition-name
   openmcl-mop:slot-definition-type
   openmcl-mop:slot-definition-readers
   openmcl-mop:slot-definition-writers
   openmcl-mop:slot-boundp-using-class
   openmcl-mop:slot-makunbound-using-class))

;;; UTF8

(defimplementation string-to-utf8 (string)
  (ccl:encode-string-to-octets string :external-format :utf-8))

(defimplementation utf8-to-string (octets)
  (ccl:decode-string-from-octets octets :external-format :utf-8))

;;; TCP Server

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port &key backlog)
  (ccl:make-socket :connect :passive :local-port port
                   :local-host host :reuse-address t
                   :backlog (or backlog 5)))

(defimplementation local-port (socket)
  (ccl:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket &key external-format
                                      buffering timeout)
  (declare (ignore buffering timeout))
  (let ((stream-args (and external-format
                          `(:external-format ,external-format))))
    (ccl:accept-connection socket :wait t :stream-args stream-args)))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix"
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

(defimplementation socket-fd (stream)
  (ccl::ioblock-device (ccl::stream-ioblock stream t)))

;;; Unix signals

(defimplementation getpid ()
  (ccl::getpid))

(defimplementation lisp-implementation-type-name ()
  "ccl")

;;; Arglist

(defimplementation arglist (fname)
  (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                           (ccl:arglist fname))
    (if binding
      arglist
      :not-available)))

(defimplementation function-name (function)
  (ccl:function-name function))

(defmethod declaration-arglist ((decl-identifier (eql 'optimize)))
  (let ((flags (ccl:declaration-information decl-identifier)))
    (if flags
        `(&any ,flags)
        (call-next-method))))

;;; Compilation

(defun handle-compiler-warning (condition)
  "Resignal a ccl:compiler-warning as swank/backend:compiler-warning."
  (signal 'compiler-condition
          :original-condition condition
          :message (compiler-warning-short-message condition)
          :source-context nil
          :severity (compiler-warning-severity condition)
          :location (source-note-to-source-location
                     (ccl:compiler-warning-source-note condition)
                     (lambda () "Unknown source")
                     (ccl:compiler-warning-function-name condition))))

(defgeneric compiler-warning-severity (condition))
(defmethod compiler-warning-severity ((c ccl:compiler-warning)) :warning)
(defmethod compiler-warning-severity ((c ccl:style-warning)) :style-warning)

(defgeneric compiler-warning-short-message (condition))

;; Pretty much the same as ccl:report-compiler-warning but
;; without the source position and function name stuff.
(defmethod compiler-warning-short-message ((c ccl:compiler-warning))
  (with-output-to-string (stream)
    (ccl:report-compiler-warning c stream :short t)))

;; Needed because `ccl:report-compiler-warning' would return
;; "Nonspecific warning".
(defmethod compiler-warning-short-message ((c ccl::shadowed-typecase-clause))
  (princ-to-string c))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((ccl:compiler-warning 'handle-compiler-warning))
    (let ((ccl:*merge-compiler-warnings* nil))
      (funcall function))))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (compile-file input-file
                  :output-file output-file
                  :load load-p
                  :external-format external-format)))

;; Use a temp file rather than in-core compilation in order to handle
;; eval-when's as compile-time.
(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore line column policy))
  (with-compilation-hooks ()
    (let ((temp-file-name (ccl:temp-pathname))
          (ccl:*save-source-locations* t))
      (unwind-protect
           (progn
             (with-open-file (s temp-file-name :direction :output
                                :if-exists :error :external-format :utf-8)
               (write-string string s))
             (let ((binary-filename (compile-temp-file
                                     temp-file-name filename buffer position)))
               (delete-file binary-filename)))
        (delete-file temp-file-name)))))

(defvar *temp-file-map* (make-hash-table :test #'equal)
  "A mapping from tempfile names to Emacs buffer names.")

(defun compile-temp-file (temp-file-name buffer-file-name buffer-name offset)
  (compile-file temp-file-name
                :load t
                :compile-file-original-truename
                (or buffer-file-name
                    (progn
                      (setf (gethash temp-file-name *temp-file-map*)
                            buffer-name)
                      temp-file-name))
                :compile-file-original-buffer-offset (1- offset)
                :external-format :utf-8))

(defimplementation save-image (filename &optional restart-function)
  (ccl:save-application filename :toplevel-function restart-function))

;;; Cross-referencing

(defun xref-locations (relation name &optional inverse)
  (delete-duplicates
   (mapcan #'find-definitions
           (if inverse
             (ccl::get-relation relation name :wild :exhaustive t)
             (ccl::get-relation relation :wild name :exhaustive t)))
   :test 'equal))

(defimplementation who-binds (name)
  (xref-locations :binds name))

(defimplementation who-macroexpands (name)
  (xref-locations :macro-calls name t))

(defimplementation who-references (name)
  (remove-duplicates
   (append (xref-locations :references name)
           (xref-locations :sets name)
           (xref-locations :binds name))
   :test 'equal))

(defimplementation who-sets (name)
  (xref-locations :sets name))

(defimplementation who-calls (name)
  (remove-duplicates
   (append
    (xref-locations :direct-calls name)
    (xref-locations :indirect-calls name)
    (xref-locations :macro-calls name t))
   :test 'equal))

(defimplementation who-specializes (class)
  (when (symbolp class)
    (setq class (find-class class nil)))
  (when class
    (delete-duplicates
     (mapcar (lambda (m)
               (car (find-definitions m)))
             (ccl:specializer-direct-methods class))
     :test 'equal)))

(defimplementation list-callees (name)
  (remove-duplicates
   (append
   (xref-locations :direct-calls name t)
   (xref-locations :macro-calls name nil))
   :test 'equal))

(defimplementation list-callers (symbol)
  (delete-duplicates
   (mapcan #'find-definitions (ccl:caller-functions symbol))
   :test #'equal))

;;; Profiling (alanr: lifted from swank-clisp)

(defimplementation profile (fname)
  (eval `(swank-monitor:monitor ,fname)))		;monitor is a macro

(defimplementation profiled-functions ()
  swank-monitor:*monitored-functions*)

(defimplementation unprofile (fname)
  (eval `(swank-monitor:unmonitor ,fname)))	;unmonitor is a macro

(defimplementation unprofile-all ()
  (swank-monitor:unmonitor))

(defimplementation profile-report ()
  (swank-monitor:report-monitoring))

(defimplementation profile-reset ()
  (swank-monitor:reset-all-monitoring))

(defimplementation profile-package (package callers-p methods)
  (declare (ignore callers-p methods))
  (swank-monitor:monitor-all package))

;;; Debugging

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* (;;(*debugger-hook* nil)
         ;; don't let error while printing error take us down
         (ccl:*signal-printing-errors* nil))
    (funcall debugger-loop-fn)))

;; This is called for an async interrupt and is running in a random
;; thread not selected by the user, so don't use thread-local vars
;; such as *emacs-connection*.
(defun find-repl-thread ()
  (let* ((*break-on-signals* nil)
         (conn (swank::default-connection)))
    (and (swank::multithreaded-connection-p conn)
         (swank::mconn.repl-thread conn))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (ccl:*break-hook* hook)
        (ccl:*select-interactive-process-hook* 'find-repl-thread))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq ccl:*break-hook* function)
  (setq ccl:*select-interactive-process-hook* 'find-repl-thread)
  )

(defun map-backtrace (function &optional
                      (start-frame-number 0)
                      end-frame-number)
  "Call FUNCTION passing information about each stack frame
 from frames START-FRAME-NUMBER to END-FRAME-NUMBER."
  (let ((end-frame-number (or end-frame-number most-positive-fixnum)))
    (ccl:map-call-frames function
                         :origin ccl:*top-error-frame*
                         :start-frame-number start-frame-number
                         :count (- end-frame-number start-frame-number))))

(defimplementation compute-backtrace (start-frame-number end-frame-number)
  (let (result)
    (map-backtrace (lambda (p context)
                     (push (list :frame p context) result))
                   start-frame-number end-frame-number)
    (nreverse result)))

(defimplementation print-frame (frame stream)
  (assert (eq (first frame) :frame))
  (destructuring-bind (p context) (rest frame)
    (let ((lfun (ccl:frame-function p context)))
      (format stream "(~S" (or (ccl:function-name lfun) lfun))
      (let* ((unavailable (cons nil nil))
             (args (ccl:frame-supplied-arguments p context
                                                 :unknown-marker unavailable)))
        (declare (dynamic-extent unavailable))
        (if (eq args unavailable)
            (format stream " #<Unknown Arguments>")
            (dolist (arg args)
              (if (eq arg unavailable)
                  (format stream " #<Unavailable>")
                  (format stream " ~s" arg)))))
      (format stream ")"))))

(defmacro with-frame ((p context) frame-number &body body)
  `(call/frame ,frame-number (lambda (,p ,context) . ,body)))

(defun call/frame (frame-number if-found)
  (map-backtrace
   (lambda (p context)
     (return-from call/frame
       (funcall if-found p context)))
   frame-number))

(defimplementation frame-call (frame-number)
  (with-frame (p context) frame-number
    (with-output-to-string (stream)
      (print-frame (list :frame p context) stream))))

(defimplementation frame-var-value (frame var)
  (with-frame (p context) frame
    (cdr (nth var (ccl:frame-named-variables p context)))))

(defimplementation frame-locals (index)
  (with-frame (p context) index
    (loop for (name . value) in (ccl:frame-named-variables p context)
          collect (list :name name :value value :id 0))))

(defimplementation frame-source-location (index)
  (with-frame (p context) index
    (multiple-value-bind (lfun pc) (ccl:frame-function p context)
      (if pc
        (pc-source-location lfun pc)
        (function-source-location lfun)))))

(defun function-name-package (name)
  (etypecase name
    (null nil)
    (symbol (symbol-package name))
    ((cons (eql ccl::traced)) (function-name-package (second name)))
    ((cons (eql setf)) (symbol-package (second name)))
    ((cons (eql :internal)) (function-name-package (car (last name))))
    ((cons (and symbol (not keyword)) (or (cons list null)
                                          (cons keyword (cons list null))))
     (symbol-package (car name)))
    (standard-method (function-name-package (ccl:method-name name)))))

(defimplementation frame-package (frame-number)
  (with-frame (p context) frame-number
    (let* ((lfun (ccl:frame-function p context))
           (name (ccl:function-name lfun)))
      (function-name-package name))))

(defimplementation eval-in-frame (form index)
  (with-frame (p context) index
    (let ((vars (ccl:frame-named-variables p context)))
      (eval `(let ,(loop for (var . val) in vars collect `(,var ',val))
               (declare (ignorable ,@(mapcar #'car vars)))
               ,form)))))

(defimplementation return-from-frame (index form)
  (let ((values (multiple-value-list (eval-in-frame form index))))
    (with-frame (p context) index
       (declare (ignore context))
       (ccl:apply-in-frame p #'values values))))

(defimplementation restart-frame (index)
  (with-frame (p context) index
    (ccl:apply-in-frame p
                        (ccl:frame-function p context)
                        (ccl:frame-supplied-arguments p context))))

(defimplementation disassemble-frame (the-frame-number)
  (with-frame (p context) the-frame-number
    (multiple-value-bind (lfun pc) (ccl:frame-function p context)
      (format t "LFUN: ~a~%PC: ~a  FP: #x~x  CONTEXT: ~a~%" lfun pc p context)
      (disassemble lfun))))

;; CCL commit r11373 | gz | 2008-11-16 16:35:28 +0100 (Sun, 16 Nov 2008)
;; contains some interesting details:
;;
;; Source location are recorded in CCL:SOURCE-NOTE's, which are objects
;; with accessors CCL:SOURCE-NOTE-FILENAME, CCL:SOURCE-NOTE-START-POS,
;; CCL:SOURCE-NOTE-END-POS and CCL:SOURCE-NOTE-TEXT.  The start and end
;; positions are file positions (not character positions).  The text will
;; be NIL unless text recording was on at read-time.  If the original
;; file is still available, you can force missing source text to be read
;; from the file at runtime via CCL:ENSURE-SOURCE-NOTE-TEXT.
;;
;; Source-note's are associated with definitions (via record-source-file)
;; and also stored in function objects (including anonymous and nested
;; functions).  The former can be retrieved via
;; CCL:FIND-DEFINITION-SOURCES, the latter via CCL:FUNCTION-SOURCE-NOTE.
;;
;; The recording behavior is controlled by the new variable
;; CCL:*SAVE-SOURCE-LOCATIONS*:
;;
;;   If NIL, don't store source-notes in function objects, and store only
;;   the filename for definitions (the latter only if
;;   *record-source-file* is true).
;;
;;   If T, store source-notes, including a copy of the original source
;;   text, for function objects and definitions (the latter only if
;;   *record-source-file* is true).
;;
;;   If :NO-TEXT, store source-notes, but without saved text, for
;;   function objects and defintions (the latter only if
;;   *record-source-file* is true).  This is the default.
;;
;; PC to source mapping is controlled by the new variable
;; CCL:*RECORD-PC-MAPPING*.  If true (the default), functions store a
;; compressed table mapping pc offsets to corresponding source locations.
;; This can be retrieved by (CCL:FIND-SOURCE-NOTE-AT-PC function pc)
;; which returns a source-note for the source at offset pc in the
;; function.

(defun function-source-location (function)
  (source-note-to-source-location
   (or (ccl:function-source-note function)
       (function-name-source-note function))
   (lambda ()
     (format nil "Function has no source note: ~A" function))
   (ccl:function-name function)))

(defun pc-source-location (function pc)
  (source-note-to-source-location
   (or (ccl:find-source-note-at-pc function pc)
       (ccl:function-source-note function)
       (function-name-source-note function))
   (lambda ()
     (format nil "No source note at PC: ~a[~d]" function pc))
   (ccl:function-name function)))

(defun function-name-source-note (fun)
  (let ((defs (ccl:find-definition-sources (ccl:function-name fun) 'function)))
    (and defs
         (destructuring-bind ((type . name) srcloc . srclocs) (car defs)
           (declare (ignore type name srclocs))
           srcloc))))

(defun source-note-to-source-location (source if-nil-thunk &optional name)
  (labels ((filename-to-buffer (filename)
             (cond ((gethash filename *temp-file-map*)
                    (list :buffer (gethash filename *temp-file-map*)))
                   ((probe-file filename)
                    (list :file (ccl:native-translated-namestring
                                 (truename filename))))
                   (t (error "File ~s doesn't exist" filename)))))
    (handler-case
        (cond ((ccl:source-note-p source)
               (let* ((full-text (ccl:source-note-text source))
                      (file-name (ccl:source-note-filename source))
                      (start-pos (ccl:source-note-start-pos source)))
                 (make-location
                  (when file-name (filename-to-buffer (pathname file-name)))
                  (when start-pos (list :position (1+ start-pos)))
                  (when full-text
                    (list :snippet (subseq full-text 0
                                           (min 40 (length full-text))))))))
              ((and source name)
               ;; This branch is probably never used
               (make-location
                (filename-to-buffer source)
                (list :function-name (princ-to-string
                                      (if (functionp name)
                                          (ccl:function-name name)
                                          name)))))
              (t `(:error ,(funcall if-nil-thunk))))
      (error (c) `(:error ,(princ-to-string c))))))

(defun alphatizer-definitions (name)
  (let ((alpha (gethash name ccl::*nx1-alphatizers*)))
    (and alpha (ccl:find-definition-sources alpha))))

(defun p2-definitions (name)
  (let ((nx1-op (gethash name ccl::*nx1-operators*)))
    (and nx1-op
         (let ((dispatch (ccl::backend-p2-dispatch ccl::*target-backend*)) )
           (and (array-in-bounds-p dispatch nx1-op)
                (let ((p2 (aref dispatch nx1-op)))
                  (and p2
                       (ccl:find-definition-sources p2))))))))

(defimplementation find-definitions (name)
  (let ((defs (append (or (ccl:find-definition-sources name)
                          (and (symbolp name)
                               (fboundp name)
                               (ccl:find-definition-sources
                                (symbol-function name))))
                      (alphatizer-definitions name)
                      (p2-definitions name))))
    (loop for ((type . name) . sources) in defs
          collect (list (definition-name type name)
                        (source-note-to-source-location
                         (find-if-not #'null sources)
                         (lambda () "No source-note available")
                         name)))))

(defimplementation find-source-location (obj)
  (let* ((defs (ccl:find-definition-sources obj))
         (best-def (or (find (ccl:name-of obj) defs :key #'cdar :test #'equal)
                       (car defs)))
         (note (find-if-not #'null (cdr best-def))))
    (when note
      (source-note-to-source-location
       note
       (lambda () "No source note available")))))

(defun definition-name (type object)
  (case (ccl:definition-type-name type)
    (method (ccl:name-of object))
    (t (list (ccl:definition-type-name type) (ccl:name-of object)))))

;;; Utilities

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
       :setf (let ((setf-function-name (ccl:setf-function-spec-name
                                        `(setf ,symbol))))
               (when (fboundp setf-function-name)
                 (doc 'function setf-function-name))))
      (maybe-push
       :type (when (ccl:type-specifier-p symbol)
               (doc 'type)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:setf
     (describe (ccl:setf-function-spec-name `(setf ,symbol))))
    (:class
     (describe (find-class symbol)))
    (:type
     (describe (or (find-class symbol nil) symbol)))))

;; spec ::= (:defmethod <name> {<qualifier>}* ({<specializer>}*))
(defun parse-defmethod-spec (spec)
  (values (second spec)
          (subseq spec 2 (position-if #'consp spec))
          (find-if #'consp (cddr spec))))

(defimplementation toggle-trace (spec)
  "We currently ignore just about everything."
  (let ((what (ecase (first spec)
                ((setf)
                 spec)
                ((:defgeneric)
                 (second spec))
                ((:defmethod)
                 (multiple-value-bind (name qualifiers specializers)
                     (parse-defmethod-spec spec)
                   (find-method (fdefinition name)
                                qualifiers
                                specializers))))))
    (cond ((member what (trace) :test #'equal)
           (ccl::%untrace what)
           (format nil "~S is now untraced." what))
          (t
           (ccl:trace-function what)
           (format nil "~S is now traced." what)))))

;;; Macroexpansion

(defimplementation macroexpand-all (form &optional env)
  (ccl:macroexpand-all form env))

;;;; Inspection

(defun comment-type-p (type)
  (or (eq type :comment)
      (and (consp type) (eq (car type) :comment))))

(defmethod emacs-inspect ((o t))
  (let* ((inspector:*inspector-disassembly* t)
         (i (inspector:make-inspector o))
         (count (inspector:compute-line-count i)))
    (loop for l from 0 below count append
          (multiple-value-bind (value label type) (inspector:line-n i l)
            (etypecase type
              ((member nil :normal)
               `(,(or label "") (:value ,value) (:newline)))
              ((member :colon)
               (label-value-line label value))
              ((member :static)
               (list (princ-to-string label) " " `(:value ,value) '(:newline)))
              ((satisfies comment-type-p)
               (list (princ-to-string label) '(:newline))))))))

(defmethod emacs-inspect :around ((o t))
  (if (or (uvector-inspector-p o)
          (not (ccl:uvectorp o)))
      (call-next-method)
      (let ((value (call-next-method)))
        (cond ((listp value)
               (append value
                       `((:newline)
                         (:value ,(make-instance 'uvector-inspector :object o)
                                 "Underlying UVECTOR"))))
              (t value)))))

(defmethod emacs-inspect ((f function))
  (append
   (label-value-line "Name" (function-name f))
   `("Its argument list is: "
     ,(princ-to-string (arglist f)) (:newline))
   (label-value-line "Documentation" (documentation  f t))
   (when (function-lambda-expression f)
     (label-value-line "Lambda Expression"
                       (function-lambda-expression f)))
   (when (ccl:function-source-note f)
     (label-value-line "Source note"
                       (ccl:function-source-note f)))
   (when (typep f 'ccl:compiled-lexical-closure)
     (append
      (label-value-line "Inner function" (ccl::closure-function f))
      '("Closed over values:" (:newline))
      (loop for (name value) in (ccl::closure-closed-over-values f)
            append (label-value-line (format nil " ~a" name)
                                     value))))))

(defclass uvector-inspector ()
  ((object :initarg :object)))

(defgeneric uvector-inspector-p (object)
  (:method ((object t)) nil)
  (:method ((object uvector-inspector)) t))

(defmethod emacs-inspect ((uv uvector-inspector))
  (with-slots (object) uv
    (loop for i below (ccl:uvsize object) append
          (label-value-line (princ-to-string i) (ccl:uvref object i)))))

(defimplementation type-specifier-p (symbol)
  (or (ccl:type-specifier-p symbol)
      (not (eq (type-specifier-arglist symbol) :not-available))))

;;; Multiprocessing

(defvar *known-processes*
  (make-hash-table :size 20 :weak :key :test #'eq)
  "A map from threads to mailboxes.")

(defvar *known-processes-lock* (ccl:make-lock "*known-processes-lock*"))

(defstruct (mailbox (:conc-name mailbox.))
  (mutex (ccl:make-lock "thread mailbox"))
  (semaphore (ccl:make-semaphore))
  (queue '() :type list))

(defimplementation spawn (fun &key name)
  (ccl:process-run-function (or name "Anonymous (Swank)")
                            fun))

(defimplementation thread-id (thread)
  (ccl:process-serial-number thread))

(defimplementation find-thread (id)
  (find id (ccl:all-processes) :key #'ccl:process-serial-number))

(defimplementation thread-name (thread)
  (ccl:process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A" (ccl:process-whostate thread)))

(defimplementation thread-attributes (thread)
   (list :priority (ccl:process-priority thread)))

(defimplementation make-lock (&key name)
  (ccl:make-lock name))

(defimplementation call-with-lock-held (lock function)
  (ccl:with-lock-grabbed (lock)
    (funcall function)))

(defimplementation current-thread ()
  ccl:*current-process*)

(defimplementation all-threads ()
  (ccl:all-processes))

(defimplementation kill-thread (thread)
  ;;(ccl:process-kill thread) ; doesn't cut it
  (ccl::process-initial-form-exited thread :kill))

(defimplementation thread-alive-p (thread)
  (not (ccl:process-exhausted-p thread)))

(defimplementation interrupt-thread (thread function)
  (ccl:process-interrupt
   thread
   (lambda ()
     (let ((ccl:*top-error-frame* (ccl::%current-exception-frame)))
       (funcall function)))))

(defun mailbox (thread)
  (ccl:with-lock-grabbed (*known-processes-lock*)
    (or (gethash thread *known-processes*)
        (setf (gethash thread *known-processes*) (make-mailbox)))))

(defimplementation send (thread message)
  (assert message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (ccl:with-lock-grabbed (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      (ccl:signal-semaphore (mailbox.semaphore mbox)))))

(defimplementation wake-thread (thread)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (ccl:with-lock-grabbed (mutex)
      (ccl:signal-semaphore (mailbox.semaphore mbox)))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox ccl:*current-process*))
         (mutex (mailbox.mutex mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
     (check-slime-interrupts)
     (ccl:with-lock-grabbed (mutex)
       (let* ((q (mailbox.queue mbox))
              (tail (member-if test q)))
         (when tail
           (setf (mailbox.queue mbox)
                 (nconc (ldiff q tail) (cdr tail)))
           (return (car tail)))))
     (when (eq timeout t) (return (values nil t)))
     (ccl:wait-on-semaphore (mailbox.semaphore mbox)))))

(let ((alist '())
      (lock (ccl:make-lock "register-thread")))

  (defimplementation register-thread (name thread)
    (declare (type symbol name))
    (ccl:with-lock-grabbed (lock)
      (etypecase thread
        (null
         (setf alist (delete name alist :key #'car)))
        (ccl:process
         (let ((probe (assoc name alist)))
           (cond (probe (setf (cdr probe) thread))
                 (t (setf alist (acons name thread alist))))))))
    nil)

  (defimplementation find-registered (name)
    (ccl:with-lock-grabbed (lock)
      (cdr (assoc name alist)))))

(defimplementation set-default-initial-binding (var form)
  (eval `(ccl::def-standard-initial-binding ,var ,form)))

(defimplementation quit-lisp ()
  (ccl:quit))

(defimplementation set-default-directory (directory)
  (let ((dir (truename (merge-pathnames directory))))
    (setf *default-pathname-defaults* (truename (merge-pathnames directory)))
    (ccl:cwd dir)
    (default-directory)))

;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak :value args))

(defimplementation hash-table-weakness (hashtable)
  (ccl:hash-table-weak-p hashtable))

(pushnew 'deinit-log-output ccl:*save-exit-functions*)
