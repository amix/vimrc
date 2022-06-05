;;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-mezzano.lisp --- SLIME backend for Mezzano
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;

;;; Administrivia

(defpackage swank/mezzano
  (:use cl swank/backend))

(in-package swank/mezzano)

;;; swank-mop

(import-swank-mop-symbols :mezzano.clos '(:class-default-initargs
                                          :class-direct-default-initargs
                                          :specializer-direct-methods
                                          :generic-function-declarations))

(defun swank-mop:specializer-direct-methods (obj)
  (declare (ignore obj))
  '())

(defun swank-mop:generic-function-declarations (gf)
  (declare (ignore gf))
  '())

(defimplementation gray-package-name ()
  "MEZZANO.GRAY")

;;;; TCP server

(defclass listen-socket ()
  ((%listener :initarg :listener)))

(defimplementation create-socket (host port &key backlog)
  (make-instance 'listen-socket
                 :listener (mezzano.network.tcp:tcp-listen
                            host
                            port
                            :backlog (or backlog 10))))

(defimplementation local-port (socket)
  (mezzano.network.tcp:tcp-listener-local-port (slot-value socket '%listener)))

(defimplementation close-socket (socket)
  (mezzano.network.tcp:close-tcp-listener (slot-value socket '%listener)))

(defimplementation accept-connection (socket &key external-format
                                             buffering timeout)
  (declare (ignore external-format buffering timeout))
  (loop
    (let ((value (mezzano.network.tcp:tcp-accept (slot-value socket '%listener)
                                                 :wait-p nil)))
      (if value
          (return value)
          ;; Poke standard-input every now and then to keep the console alive.
          (progn (listen)
                 (sleep 0.05))))))

(defimplementation preferred-communication-style ()
  :spawn)

;;;; Unix signals
;;;; ????

(defimplementation getpid ()
  0)

;;;; Compilation

(defun signal-compiler-condition (condition severity)
  (signal 'compiler-condition
          :original-condition condition
          :severity severity
          :message (format nil "~A" condition)
          :location nil))

(defimplementation call-with-compilation-hooks (func)
  (handler-bind
      ((error
        (lambda (c)
          (signal-compiler-condition c :error)))
       (warning
        (lambda (c)
          (signal-compiler-condition c :warning)))
       (style-warning
        (lambda (c)
          (signal-compiler-condition c :style-warning))))
    (funcall func)))

(defimplementation swank-compile-string (string &key buffer position filename
                                                line column policy)
  (declare (ignore buffer line column policy))
  (let* ((*load-pathname* (ignore-errors (pathname filename)))
         (*load-truename* (when *load-pathname*
                            (ignore-errors (truename *load-pathname*))))
         (sys.int::*top-level-form-number* `(:position ,position)))
    (with-compilation-hooks ()
      (eval (read-from-string (concatenate 'string "(progn " string " )")))))
  t)

(defimplementation swank-compile-file (input-file output-file load-p
                                                  external-format
                                                  &key policy)
  (with-compilation-hooks ()
    (multiple-value-prog1
        (compile-file input-file
                      :output-file output-file
                      :external-format external-format)
      (when load-p
        (load output-file)))))

(defimplementation find-external-format (coding-system)
  (if (or (equal coding-system "utf-8")
          (equal coding-system "utf-8-unix"))
      :default
      nil))

;;;; Debugging

;; Definitely don't allow this.
(defimplementation install-debugger-globally (function)
  (declare (ignore function))
  nil)

(defvar *current-backtrace*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*current-backtrace* '()))
    (let ((prev-fp nil))
      (sys.int::map-backtrace
       (lambda (i fp)
         (push (list (1- i) fp prev-fp) *current-backtrace*)
         (setf prev-fp fp))))
    (setf *current-backtrace* (reverse *current-backtrace*))
    ;; Drop the topmost frame, which is finished call to MAP-BACKTRACE.
    (pop *current-backtrace*)
    ;; And the next one for good measure.
    (pop *current-backtrace*)
    (funcall debugger-loop-fn)))

(defimplementation compute-backtrace (start end)
  (subseq *current-backtrace* start end))

(defimplementation print-frame (frame stream)
  (format stream "~S" (sys.int::function-from-frame frame)))

(defimplementation frame-source-location (frame-number)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame)))
    (function-location fn)))

(defimplementation frame-locals (frame-number)
  (loop
     with frame = (nth frame-number *current-backtrace*)
     for (name id location repr) in (sys.int::frame-locals frame)
     collect (list :name name
                   :id id
                   :value (sys.int::read-frame-slot frame location repr))))

(defimplementation frame-var-value (frame-number var-id)
  (let* ((frame (nth frame-number *current-backtrace*))
         (locals (sys.int::frame-locals frame))
         (info (nth var-id locals)))
    (if info
        (destructuring-bind (name id location repr)
            info
          (declare (ignore id))
          (values (sys.int::read-frame-slot frame location repr) name))
        (error "Invalid variable id ~D for frame number ~D."
               var-id frame-number))))

;;;; Definition finding

(defun top-level-form-position (pathname tlf)
  (ignore-errors
    (with-open-file (s pathname)
      (loop
         repeat tlf
         do (with-standard-io-syntax
              (let ((*read-suppress* t)
                    (*read-eval* nil))
                (read s nil))))
      (let ((default (make-pathname :host (pathname-host s))))
        (make-location `(:file ,(enough-namestring s default))
                       `(:position ,(1+ (file-position s))))))))

(defun function-location (function)
  "Return a location object for FUNCTION."
  (let* ((info (sys.int::function-debug-info function))
         (pathname (sys.int::debug-info-source-pathname info))
         (tlf (sys.int::debug-info-source-top-level-form-number info)))
    (cond ((and (consp tlf)
                (eql (first tlf) :position))
           (let ((default (make-pathname :host (pathname-host pathname))))
             (make-location `(:file ,(enough-namestring pathname default))
                            `(:position ,(second tlf)))))
          (t
           (top-level-form-position pathname tlf)))))

(defun method-definition-name (name method)
  `(defmethod ,name
       ,@(mezzano.clos:method-qualifiers method)
     ,(mapcar (lambda (x)
                (typecase x
                  (mezzano.clos:class
                   (mezzano.clos:class-name x))
                  (mezzano.clos:eql-specializer
                   `(eql ,(mezzano.clos:eql-specializer-object x)))
                  (t x)))
              (mezzano.clos:method-specializers method))))

(defimplementation find-definitions (name)
  (let ((result '()))
    (labels
        ((frob-fn (dspec fn)
           (let ((loc (function-location fn)))
             (when loc
               (push (list dspec loc) result))))
         (try-fn (name)
           (when (valid-function-name-p name)
             (when (and (fboundp name)
                        (not (and (symbolp name)
                                  (or (special-operator-p name)
                                      (macro-function name)))))
               (let ((fn (fdefinition name)))
                 (cond ((typep fn 'mezzano.clos:standard-generic-function)
                        (dolist (m (mezzano.clos:generic-function-methods fn))
                          (frob-fn (method-definition-name name m)
                                   (mezzano.clos:method-function m))))
                       (t
                        (frob-fn `(defun ,name) fn)))))
             (when (compiler-macro-function name)
               (frob-fn `(define-compiler-macro ,name)
                        (compiler-macro-function name))))))
      (try-fn name)
      (try-fn `(setf name))
      (try-fn `(sys.int::cas name))
      (when (and (symbolp name)
                 (get name 'sys.int::setf-expander))
        (frob-fn `(define-setf-expander ,name)
                 (get name 'sys.int::setf-expander)))
      (when (and (symbolp name)
                 (macro-function name))
        (frob-fn `(defmacro ,name)
                 (macro-function name))))
    result))

;;;; XREF
;;; Simpler variants.

(defun find-all-frefs ()
  (let ((frefs (make-array 500 :adjustable t :fill-pointer 0))
        (keep-going t))
    (loop
       (when (not keep-going)
         (return))
       (adjust-array frefs (* (array-dimension frefs 0) 2))
       (setf keep-going nil
             (fill-pointer frefs) 0)
       ;; Walk the wired area looking for FREFs.
       (sys.int::walk-area
        :wired
        (lambda (object address size)
          (when (sys.int::function-reference-p object)
            (when (not (vector-push object frefs))
              (setf keep-going t))))))
    (remove-duplicates (coerce frefs 'list))))

(defimplementation list-callers (function-name)
  (let ((fref-for-fn (sys.int::function-reference function-name))
        (callers '()))
    (loop
       for fref in (find-all-frefs)
       for fn = (sys.int::function-reference-function fref)
       for name = (sys.int::function-reference-name fref)
       when fn
       do
         (cond ((typep fn 'standard-generic-function)
                (dolist (m (mezzano.clos:generic-function-methods fn))
                  (let* ((mf (mezzano.clos:method-function m))
                         (mf-frefs (get-all-frefs-in-function mf)))
                    (when (member fref-for-fn mf-frefs)
                      (push `((defmethod ,name
                                  ,@(mezzano.clos:method-qualifiers m)
                                ,(mapcar #'specializer-name
                                         (mezzano.clos:method-specializers m)))
                              ,(function-location mf))
                            callers)))))
               ((member fref-for-fn
                        (get-all-frefs-in-function fn))
                (push `((defun ,name) ,(function-location fn)) callers))))
    callers))

(defun specializer-name (specializer)
  (if (typep specializer 'standard-class)
      (mezzano.clos:class-name specializer)
      specializer))

(defun get-all-frefs-in-function (function)
  (when (sys.int::funcallable-std-instance-p function)
    (setf function (sys.int::funcallable-std-instance-function function)))
  (when (sys.int::closure-p function)
    (setf function (sys.int::%closure-function function)))
  (loop
     for i below (sys.int::function-pool-size function)
     for entry = (sys.int::function-pool-object function i)
     when (sys.int::function-reference-p entry)
     collect entry
     when (compiled-function-p entry) ; closures
     append (get-all-frefs-in-function entry)))

(defimplementation list-callees (function-name)
  (let* ((fn (fdefinition function-name))
         ;; Grovel around in the function's constant pool looking for
         ;; function-references.  These may be for #', but they're
         ;; probably going to be for normal calls.
         ;; TODO: This doesn't work well on interpreted functions or
         ;; funcallable instances.
         (callees (remove-duplicates (get-all-frefs-in-function fn))))
    (loop
       for fref in callees
       for name = (sys.int::function-reference-name fref)
       for fn = (sys.int::function-reference-function fref)
       when fn
       collect `((defun ,name) ,(function-location fn)))))

;;;; Documentation

(defimplementation arglist (name)
  (let ((macro (when (symbolp name)
                 (macro-function name)))
        (fn (if (functionp name)
                name
                (ignore-errors (fdefinition name)))))
    (cond
      (macro
       (get name 'sys.int::macro-lambda-list))
      (fn
       (cond
         ((typep fn 'mezzano.clos:standard-generic-function)
          (mezzano.clos:generic-function-lambda-list fn))
         (t
          (function-lambda-list fn))))
      (t :not-available))))

(defun function-lambda-list (function)
  (sys.int::debug-info-lambda-list
   (sys.int::function-debug-info function)))

(defimplementation type-specifier-p (symbol)
  (cond
    ((or (get symbol 'sys.int::type-expander)
         (get symbol 'sys.int::compound-type)
         (get symbol 'sys.int::type-symbol))
     t)
    (t :not-available)))

(defimplementation function-name (function)
  (sys.int::function-name function))

(defimplementation valid-function-name-p (form)
  "Is FORM syntactically valid to name a function?
   If true, FBOUNDP should not signal a type-error for FORM."
  (flet ((length=2 (list)
           (and (not (null (cdr list))) (null (cddr list)))))
    (or (symbolp form)
        (and (consp form) (length=2 form)
             (or (eq (first form) 'setf)
                 (eq (first form) 'sys.int::cas))
             (symbolp (second form))))))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (when (boundp symbol)
      (setf (getf result :variable) nil))
    (when (and (fboundp symbol)
               (not (macro-function symbol)))
      (setf (getf result :function)
            (function-docstring symbol)))
    (when (fboundp `(setf ,symbol))
      (setf (getf result :setf)
            (function-docstring `(setf ,symbol))))
    (when (get symbol 'sys.int::setf-expander)
      (setf (getf result :setf) nil))
    (when (special-operator-p symbol)
      (setf (getf result :special-operator) nil))
    (when (macro-function symbol)
      (setf (getf result :macro) nil))
    (when (compiler-macro-function symbol)
      (setf (getf result :compiler-macro) nil))
    (when (type-specifier-p symbol)
      (setf (getf result :type) nil))
    (when (find-class symbol nil)
      (setf (getf result :class) nil))
    result))

(defun function-docstring (function-name)
  (let* ((definition (fdefinition function-name))
         (debug-info (sys.int::function-debug-info definition)))
    (sys.int::debug-info-docstring debug-info)))

;;;; Multithreading

;; FIXME: This should be a weak table.
(defvar *thread-ids-for-emacs* (make-hash-table))
(defvar *next-thread-id-for-emacs* 0)
(defvar *thread-id-for-emacs-lock* (mezzano.supervisor:make-mutex
                                    "SWANK thread ID table"))

(defimplementation spawn (fn &key name)
  (mezzano.supervisor:make-thread fn :name name))

(defimplementation thread-id (thread)
  (mezzano.supervisor:with-mutex (*thread-id-for-emacs-lock*)
    (let ((id (gethash thread *thread-ids-for-emacs*)))
      (when (null id)
        (setf id (incf *next-thread-id-for-emacs*)
              (gethash thread *thread-ids-for-emacs*) id
              (gethash id *thread-ids-for-emacs*) thread))
      id)))

(defimplementation find-thread (id)
  (mezzano.supervisor:with-mutex (*thread-id-for-emacs-lock*)
    (gethash id *thread-ids-for-emacs*)))

(defimplementation thread-name (thread)
  (mezzano.supervisor:thread-name thread))

(defimplementation thread-status (thread)
  (format nil "~:(~A~)" (mezzano.supervisor:thread-state thread)))

(defimplementation current-thread ()
  (mezzano.supervisor:current-thread))

(defimplementation all-threads ()
  (mezzano.supervisor:all-threads))

(defimplementation thread-alive-p (thread)
  (not (eql (mezzano.supervisor:thread-state thread) :dead)))

(defimplementation interrupt-thread (thread fn)
  (mezzano.supervisor:establish-thread-foothold thread fn))

(defimplementation kill-thread (thread)
  ;; Documentation says not to execute unwind-protected sections, but there's
  ;; no way to do that.
  ;; And killing threads at arbitrary points without unwinding them is a good
  ;; way to hose the system.
  (mezzano.supervisor:terminate-thread thread))

(defvar *mailbox-lock* (mezzano.supervisor:make-mutex "mailbox lock"))
(defvar *mailboxes* (list))

(defstruct (mailbox (:conc-name mailbox.))
  thread
  (mutex (mezzano.supervisor:make-mutex))
  (queue '() :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  ;; Use weak pointers to avoid holding on to dead threads forever.
  (mezzano.supervisor:with-mutex (*mailbox-lock*)
    ;; Flush forgotten threads.
    (setf *mailboxes*
          (remove-if-not #'sys.int::weak-pointer-value *mailboxes*))
    (loop
       for entry in *mailboxes*
       do
         (multiple-value-bind (key value livep)
             (sys.int::weak-pointer-pair entry)
           (when (eql key thread)
             (return value)))
       finally
         (let ((mb (make-mailbox :thread thread)))
           (push (sys.int::make-weak-pointer thread mb) *mailboxes*)
           (return mb)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (mezzano.supervisor:with-mutex (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defvar *receive-if-sleep-time* 0.02)

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread)))
         (mutex (mailbox.mutex mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
       (check-slime-interrupts)
       (mezzano.supervisor:with-mutex (mutex)
         (let* ((q (mailbox.queue mbox))
                (tail (member-if test q)))
           (when tail
             (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
             (return (car tail))))
         (when (eq timeout t) (return (values nil t))))
       (sleep *receive-if-sleep-time*))))

(defvar *registered-threads* (make-hash-table))
(defvar *registered-threads-lock*
  (mezzano.supervisor:make-mutex "registered threads lock"))

(defimplementation register-thread (name thread)
  (declare (type symbol name))
  (mezzano.supervisor:with-mutex (*registered-threads-lock*)
    (etypecase thread
      (null
       (remhash name *registered-threads*))
      (mezzano.supervisor:thread
       (setf (gethash name *registered-threads*) thread))))
  nil)

(defimplementation find-registered (name)
  (mezzano.supervisor:with-mutex (*registered-threads-lock*)
    (values (gethash name *registered-threads*))))

(defimplementation wait-for-input (streams &optional timeout)
  (loop
       (let ((ready '()))
         (dolist (s streams)
           (when (or (listen s)
                     (and (typep s 'mezzano.network.tcp::tcp-stream)
                          (mezzano.network.tcp::tcp-connection-closed-p s)))
             (push s ready)))
         (when ready
           (return ready))
         (when (check-slime-interrupts)
           (return :interrupt))
         (when timeout
           (return '()))
         (sleep 1)
         (when (numberp timeout)
           (decf timeout 1)
           (when (not (plusp timeout))
             (return '()))))))

;;;;  Locks

(defstruct recursive-lock
  mutex
  (depth 0))

(defimplementation make-lock (&key name)
  (make-recursive-lock
   :mutex (mezzano.supervisor:make-mutex name)))

(defimplementation call-with-lock-held (lock function)
  (cond ((mezzano.supervisor:mutex-held-p
          (recursive-lock-mutex lock))
         (unwind-protect
              (progn (incf (recursive-lock-depth lock))
                     (funcall function))
           (decf (recursive-lock-depth lock))))
        (t
         (mezzano.supervisor:with-mutex ((recursive-lock-mutex lock))
           (multiple-value-prog1
               (funcall function)
             (assert (eql (recursive-lock-depth lock) 0)))))))

;;;; Character names

(defimplementation character-completion-set (prefix matchp)
  ;; TODO: Unicode characters too.
  (loop
     for names in sys.int::*char-name-alist*
     append
       (loop
          for name in (rest names)
          when (funcall matchp prefix name)
          collect name)))

;;;; Inspector

(defmethod emacs-inspect ((o function))
  (case (sys.int::%object-tag o)
    (#.sys.int::+object-tag-function+
     (label-value-line*
      (:name (sys.int::function-name o))
      (:arglist (arglist o))
      (:debug-info (sys.int::function-debug-info o))))
    (#.sys.int::+object-tag-closure+
     (append
      (label-value-line :function (sys.int::%closure-function o))
      `("Closed over values:" (:newline))
      (loop
         for i below (sys.int::%closure-length o)
         append (label-value-line i (sys.int::%closure-value o i)))))
    (t
     (call-next-method))))

(defmethod emacs-inspect ((o sys.int::weak-pointer))
  (label-value-line*
   (:key (sys.int::weak-pointer-key o))
   (:value (sys.int::weak-pointer-value o))))

(defmethod emacs-inspect ((o sys.int::function-reference))
  (label-value-line*
   (:name (sys.int::function-reference-name o))
   (:function (sys.int::function-reference-function o))))

(defmethod emacs-inspect ((object structure-object))
  (let ((class (class-of object)))
    `("Class: " (:value ,class) (:newline)
                ,@(swank::all-slots-for-inspector object))))

(in-package :swank)

(defmethod all-slots-for-inspector ((object structure-object))
  (let* ((class           (class-of object))
         (direct-slots    (swank-mop:class-direct-slots class))
         (effective-slots (swank-mop:class-slots class))
         (longest-slot-name-length
          (loop for slot :in effective-slots
                maximize (length (symbol-name
                                  (swank-mop:slot-definition-name slot)))))
         (checklist
          (reinitialize-checklist
           (ensure-istate-metadata object :checklist
                                   (make-checklist (length effective-slots)))))
         (grouping-kind
          ;; We box the value so we can re-set it.
          (ensure-istate-metadata object :grouping-kind
                                  (box *inspector-slots-default-grouping*)))
         (sort-order
          (ensure-istate-metadata object :sort-order
                                  (box *inspector-slots-default-order*)))
         (sort-predicate (ecase (ref sort-order)
                           (:alphabetically #'string<)
                           (:unsorted (constantly nil))))
         (sorted-slots (sort (copy-seq effective-slots)
                             sort-predicate
                             :key #'swank-mop:slot-definition-name))
         (effective-slots
          (ecase (ref grouping-kind)
            (:all sorted-slots)
            (:inheritance (stable-sort-by-inheritance sorted-slots
                                                      class sort-predicate)))))
    `("--------------------"
      (:newline)
      " Group slots by inheritance "
      (:action ,(ecase (ref grouping-kind)
                       (:all "[ ]")
                       (:inheritance "[X]"))
               ,(lambda ()
                        ;; We have to do this as the order of slots will
                        ;; be sorted differently.
                        (fill (checklist.buttons checklist) nil)
                        (setf (ref grouping-kind)
                              (ecase (ref grouping-kind)
                                (:all :inheritance)
                                (:inheritance :all))))
               :refreshp t)
      (:newline)
      " Sort slots alphabetically  "
      (:action ,(ecase (ref sort-order)
                       (:unsorted "[ ]")
                       (:alphabetically "[X]"))
               ,(lambda ()
                        (fill (checklist.buttons checklist) nil)
                        (setf (ref sort-order)
                              (ecase (ref sort-order)
                                (:unsorted :alphabetically)
                                (:alphabetically :unsorted))))
               :refreshp t)
      (:newline)
      ,@ (case (ref grouping-kind)
           (:all
            `((:newline)
              "All Slots:"
              (:newline)
              ,@(make-slot-listing checklist object class
                                   effective-slots direct-slots
                                   longest-slot-name-length)))
           (:inheritance
            (list-all-slots-by-inheritance checklist object class
                                           effective-slots direct-slots
                                           longest-slot-name-length)))
      (:newline)
      (:action "[set value]"
               ,(lambda ()
                        (do-checklist (idx checklist)
                          (query-and-set-slot class object
                                              (nth idx effective-slots))))
               :refreshp t)
      "  "
      (:action "[make unbound]"
               ,(lambda ()
                        (do-checklist (idx checklist)
                          (swank-mop:slot-makunbound-using-class
                           class object (nth idx effective-slots))))
               :refreshp t)
      (:newline))))
