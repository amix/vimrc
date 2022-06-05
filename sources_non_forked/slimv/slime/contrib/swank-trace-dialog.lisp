(defpackage :swank-trace-dialog
  (:use :cl)
  (:import-from :swank :defslimefun :from-string :to-string)
  (:export #:clear-trace-tree
           #:dialog-toggle-trace
           #:dialog-trace
           #:dialog-traced-p
           #:dialog-untrace
           #:dialog-untrace-all
           #:inspect-trace-part
           #:report-partial-tree
           #:report-specs
           #:report-total
           #:report-trace-detail
           #:report-specs
           #:trace-format
           #:still-inside
           #:exited-non-locally
           #:*record-backtrace*
           #:*traces-per-report*
           #:*dialog-trace-follows-trace*
           #:find-trace-part
           #:find-trace))

(in-package :swank-trace-dialog)

(defparameter *record-backtrace* nil
  "Record a backtrace of the last 20 calls for each trace.

Beware that this may have a drastic performance impact on your
program.")

(defparameter *traces-per-report* 150
  "Number of traces to report to emacs in each batch.")


;;;; `trace-entry' model
;;;;
(defvar *traces* (make-array 1000 :fill-pointer 0
                                  :adjustable t))

(defvar *trace-lock* (swank/backend:make-lock :name "swank-trace-dialog lock"))

(defvar *current-trace-by-thread* (make-hash-table))

(defclass trace-entry ()
  ((id         :reader   id-of)
   (children   :accessor children-of :initform nil)
   (backtrace  :accessor backtrace-of :initform (when *record-backtrace*
                                                  (useful-backtrace)))

   (spec       :initarg  :spec      :accessor spec-of
               :initform (error "must provide a spec"))
   (args       :initarg  :args      :accessor args-of
               :initform (error "must provide args"))
   (parent     :initarg  :parent    :reader   parent-of
               :initform (error "must provide a parent, even if nil"))
   (retlist    :initarg  :retlist   :accessor retlist-of
               :initform 'still-inside)))

(defmethod initialize-instance :after ((entry trace-entry) &rest initargs)
  (declare (ignore initargs))
  (if (parent-of entry)
      (nconc (children-of (parent-of entry)) (list entry)))
  (swank/backend:call-with-lock-held
   *trace-lock*
   #'(lambda ()
       (setf (slot-value entry 'id) (fill-pointer *traces*))
       (vector-push-extend entry *traces*))))

(defmethod print-object ((entry trace-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "~a: ~a" (id-of entry) (spec-of entry))))

(defun completed-p (trace) (not (eq (retlist-of trace) 'still-inside)))

(defun find-trace (id)
  (when (<= 0 id (1- (length *traces*)))
    (aref *traces* id)))

(defun find-trace-part (id part-id type)
  (let* ((trace (find-trace id))
         (l (and trace
                 (ecase type
                   (:arg (args-of trace))
                   (:retval (swank::ensure-list (retlist-of trace)))))))
    (values (nth part-id l)
            (< part-id (length l)))))

(defun useful-backtrace ()
  (swank/backend:call-with-debugging-environment
   #'(lambda ()
       (loop for i from 0
             for frame in (swank/backend:compute-backtrace 0 20)
             collect (list i (swank::frame-to-string frame))))))

(defun current-trace ()
  (gethash (swank/backend:current-thread) *current-trace-by-thread*))

(defun (setf current-trace) (trace)
  (setf (gethash (swank/backend:current-thread) *current-trace-by-thread*)
        trace))


;;;; Control of traced specs
;;;
(defvar *traced-specs* '())

(defslimefun dialog-trace (spec)
  (flet ((before-hook (args)
           (setf (current-trace) (make-instance 'trace-entry
                                                :spec      spec
                                                :args      args
                                                :parent    (current-trace))))
         (after-hook (retlist)
           (let ((trace (current-trace)))
             (when trace
               ;; the current trace might have been wiped away if the
               ;; user cleared the tree in the meantime. no biggie,
               ;; don't do anything.
               ;;
               (setf (retlist-of trace) retlist
                     (current-trace) (parent-of trace))))))
    (when (dialog-traced-p spec)
      (warn "~a is apparently already traced! Untracing and retracing." spec)
      (dialog-untrace spec))
    (swank/backend:wrap spec 'trace-dialog
                        :before #'before-hook
                        :after #'after-hook)
    (pushnew spec *traced-specs*)
    (format nil "~a is now traced for trace dialog" spec)))

(defslimefun dialog-untrace (spec)
  (swank/backend:unwrap spec 'trace-dialog)
  (setq *traced-specs* (remove spec *traced-specs* :test #'equal))
  (format nil "~a is now untraced for trace dialog" spec))

(defslimefun dialog-toggle-trace (spec)
  (if (dialog-traced-p spec)
      (dialog-untrace spec)
      (dialog-trace spec)))

(defslimefun dialog-traced-p (spec)
  (find spec *traced-specs* :test #'equal))

(defslimefun dialog-untrace-all ()
  (untrace)
  (mapcar #'dialog-untrace *traced-specs*))

(defparameter *dialog-trace-follows-trace* nil)

(setq swank:*after-toggle-trace-hook*
      #'(lambda (spec traced-p)
          (when *dialog-trace-follows-trace*
            (cond (traced-p
                   (dialog-trace spec)
                   "traced for trace dialog as well")
                  (t
                   (dialog-untrace spec)
                   "untraced for the trace dialog as well")))))


;;;; A special kind of trace call
;;;
(defun trace-format (format-spec &rest format-args)
  "Make a string from FORMAT-SPEC and FORMAT-ARGS and as a trace."
  (let* ((line (apply #'format nil format-spec format-args)))
    (make-instance 'trace-entry :spec line
                                :args format-args
                                :parent (current-trace)
                                :retlist nil)))


;;;; Reporting to emacs
;;;
(defparameter *visitor-idx* 0)

(defparameter *visitor-key* nil)

(defvar *unfinished-traces* '())

(defun describe-trace-for-emacs (trace)
  `(,(id-of trace)
    ,(and (parent-of trace) (id-of (parent-of trace)))
    ,(spec-of trace)
    ,(loop for arg in (args-of trace)
           for i from 0
           collect (list i (swank::to-line arg)))
    ,(loop for retval in (swank::ensure-list (retlist-of trace))
           for i from 0
           collect (list i (swank::to-line retval)))))

(defslimefun report-partial-tree (key)
  (unless (equal key *visitor-key*)
    (setq *visitor-idx* 0
          *visitor-key* key))
  (let* ((recently-finished
           (loop with i = 0
                 for trace in *unfinished-traces*
                 while (< i *traces-per-report*)
                 when (completed-p trace)
                   collect trace
                   and do
                     (incf i)
                     (setq *unfinished-traces*
                           (remove trace *unfinished-traces*))))
         (new (loop for i
                      from (length recently-finished)
                        below *traces-per-report*
                    while (< *visitor-idx* (length *traces*))
                    for trace = (aref *traces* *visitor-idx*)
                    collect trace
                    unless (completed-p trace)
                      do (push trace *unfinished-traces*)
                    do (incf *visitor-idx*))))
    (list
     (mapcar #'describe-trace-for-emacs
             (append recently-finished new))
     (- (length *traces*) *visitor-idx*)
    key)))

(defslimefun report-trace-detail (trace-id)
  (swank::call-with-bindings
   swank::*inspector-printer-bindings*
   #'(lambda ()
       (let ((trace (find-trace trace-id)))
         (when trace
           (append
            (describe-trace-for-emacs trace)
            (list (backtrace-of trace)
                  (swank::to-line trace))))))))

(defslimefun report-specs ()
  (sort (copy-list *traced-specs*)
        #'string<
        :key #'princ-to-string))

(defslimefun report-total ()
  (length *traces*))

(defslimefun clear-trace-tree ()
  (setf *current-trace-by-thread* (clrhash *current-trace-by-thread*)
        *visitor-key* nil
        *unfinished-traces* nil)
  (swank/backend:call-with-lock-held
   *trace-lock*
   #'(lambda () (setf (fill-pointer *traces*) 0)))
  nil)

;; HACK: `swank::*inspector-history*' is unbound by default and needs
;; a reset in that case so that it won't error `swank::inspect-object'
;; before any other object is inspected in the slime session.
;;
(unless (boundp 'swank::*inspector-history*)
  (swank::reset-inspector))

(defslimefun inspect-trace-part (trace-id part-id type)
  (multiple-value-bind (obj found)
      (find-trace-part trace-id part-id type)
    (if found
        (swank::inspect-object obj)
        (error "No object found with ~a, ~a and ~a" trace-id part-id type))))

(provide :swank-trace-dialog)
