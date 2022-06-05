;;;; Source-paths

;;; CMUCL/SBCL use a data structure called "source-path" to locate
;;; subforms.  The compiler assigns a source-path to each form in a
;;; compilation unit.  Compiler notes usually contain the source-path
;;; of the error location.
;;;
;;; Compiled code objects don't contain source paths, only the
;;; "toplevel-form-number" and the (sub-) "form-number".  To get from
;;; the form-number to the source-path we need the entire toplevel-form
;;; (i.e. we have to read the source code).  CMUCL has already some
;;; utilities to do this translation, but we use some extended
;;; versions, because we need more exact position info.  Apparently
;;; Hemlock is happy with the position of the toplevel-form; we also
;;; need the position of subforms.
;;;
;;; We use a special readtable to get the positions of the subforms.
;;; The readtable stores the start and end position for each subform in
;;; hashtable for later retrieval.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.

;;; Taken from swank-cmucl.lisp, by Helmut Eller

(defpackage swank/source-path-parser
  (:use cl)
  (:export
   read-source-form
   source-path-string-position
   source-path-file-position
   source-path-source-position

   sexp-in-bounds-p
   sexp-ref)
  (:shadow ignore-errors))

(in-package swank/source-path-parser)

;; Some test to ensure the required conformance
(let ((rt (copy-readtable nil)))
  (assert (or (not (get-macro-character #\space rt))
	      (nth-value 1 (get-macro-character #\space rt))))
  (assert (not (get-macro-character #\\ rt))))

(eval-when (:compile-toplevel)
  (defmacro ignore-errors (&rest forms)
    ;;`(progn . ,forms) ; for debugging
    `(cl:ignore-errors . ,forms)))

(defun make-sharpdot-reader (orig-sharpdot-reader)
  (lambda (s c n)
    ;; We want things like M-. to work regardless of any #.-fu in
    ;; the source file that is to be visited. (For instance, when a
    ;; file contains #. forms referencing constants that do not
    ;; currently exist in the image.)
    (ignore-errors (funcall orig-sharpdot-reader s c n))))

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (lambda (stream char)
    (let ((start (1- (file-position stream)))
	  (values (multiple-value-list (funcall fn stream char)))
	  (end (file-position stream)))
      #+(or)
      (format t "[~D \"~{~A~^, ~}\" ~D ~D ~S]~%"
	      start values end (char-code char) char)
      (when values
        (destructuring-bind (&optional existing-start &rest existing-end)
            (car (gethash (car values) source-map))
          ;; Some macros may return what a sub-call to another macro
          ;; produced, e.g. "#+(and) (a)" may end up saving (a) twice,
          ;; once from #\# and once from #\(. If the saved form
          ;; is a subform, don't save it again.
          (unless (and existing-start existing-end
                       (<= start existing-start end)
                       (<= start existing-end end))
            (push (cons start end) (gethash (car values) source-map)))))
      (values-list values))))

(defun make-source-recording-readtable (readtable source-map)
  (declare (type readtable readtable) (type hash-table source-map))
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (flet ((install-special-sharpdot-reader (rt)
	   (let ((fun (ignore-errors
			(get-dispatch-macro-character #\# #\. rt))))
	     (when fun
	       (let ((wrapper (make-sharpdot-reader fun)))
		 (set-dispatch-macro-character #\# #\. wrapper rt)))))
	 (install-wrappers (rt)
	   (dotimes (code 128)
	     (let ((char (code-char code)))
	       (multiple-value-bind (fun nt) (get-macro-character char rt)
		 (when fun
		   (let ((wrapper (make-source-recorder fun source-map)))
		     (set-macro-character char wrapper nt rt))))))))
    (let ((rt (copy-readtable readtable)))
      (install-special-sharpdot-reader rt)
      (install-wrappers rt)
      rt)))

;; FIXME: try to do this with *READ-SUPPRESS* = t to avoid interning.
;; Should be possible as we only need the right "list structure" and
;; not the right atoms.
(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         (*readtable* (make-source-recording-readtable *readtable* source-map))
	 (*read-suppress* nil)
	 (start (file-position stream))
	 (form (ignore-errors (read stream)))
	 (end (file-position stream)))
    ;; ensure that at least FORM is in the source-map
    (unless (gethash form source-map)
      (push (cons start end) (gethash form source-map)))
    (values form source-map)))

(defun starts-with-p (string prefix)
  (declare (type string string prefix))
  (not (mismatch string prefix
		 :end1 (min (length string) (length prefix))
		 :test #'char-equal)))

(defun extract-package (line)
  (declare (type string line))
  (let ((name (cadr (read-from-string line))))
    (find-package name)))

#+(or)
(progn
  (assert (extract-package "(in-package cl)"))
  (assert (extract-package "(cl:in-package cl)"))
  (assert (extract-package "(in-package \"CL\")"))
  (assert (extract-package "(in-package #:cl)")))

;; FIXME: do something cleaner than this.
(defun readtable-for-package (package)
  ;; KLUDGE: due to the load order we can't reference the swank
  ;; package.
  (funcall (read-from-string "swank::guess-buffer-readtable")
           (string-upcase (package-name package))))

;; Search STREAM for a "(in-package ...)" form.  Use that to derive
;; the values for *PACKAGE* and *READTABLE*.
;;
;; IDEA: move GUESS-READER-STATE to swank.lisp so that all backends
;; use the same heuristic and to avoid the need to access
;; swank::guess-buffer-readtable from here.
(defun guess-reader-state (stream)
  (let* ((point (file-position stream))
	 (pkg *package*))
    (file-position stream 0)
    (loop for line = (read-line stream nil nil) do
	  (when (not line) (return))
	  (when (or (starts-with-p line "(in-package ")
		    (starts-with-p line "(cl:in-package "))
	    (let ((p (extract-package line)))
	      (when p (setf pkg p)))
	    (return)))
    (file-position stream point)
    (values (readtable-for-package pkg) pkg)))

(defun skip-whitespace (stream)
  (peek-char t stream nil nil))

;; Skip over N toplevel forms.
(defun skip-toplevel-forms (n stream)
  (let ((*read-suppress* t))
    (dotimes (i n)
      (read stream))
    (skip-whitespace stream)))

(defun read-source-form (n stream)
  "Read the Nth toplevel form number with source location recording.
Return the form and the source-map."
  (multiple-value-bind (*readtable* *package*) (guess-reader-state stream)
    (let (#+sbcl
          (*features* (append *features*
                              (symbol-value (find-symbol "+INTERNAL-FEATURES+" 'sb-impl)))))
      (skip-toplevel-forms n stream)
      (read-and-record-source-map stream))))

(defun source-path-stream-position (path stream)
  "Search the source-path PATH in STREAM and return its position."
  (check-source-path path)
  (destructuring-bind (tlf-number . path) path
    (multiple-value-bind (form source-map) (read-source-form tlf-number stream)
      (source-path-source-position (cons 0 path) form source-map))))

(defun check-source-path (path)
  (unless (and (consp path)
               (every #'integerp path))
    (error "The source-path ~S is not valid." path)))

(defun source-path-string-position (path string)
  (with-input-from-string (s string)
    (source-path-stream-position path s)))

(defun source-path-file-position (path filename)
  ;; We go this long way round, and don't directly operate on the file
  ;; stream because FILE-POSITION (used above) is not totally savy even
  ;; on file character streams; on SBCL, FILE-POSITION returns the binary
  ;; offset, and not the character offset---screwing up on Unicode.
  (let ((toplevel-number (first path))
	(buffer))
    (with-open-file (file filename)
      (skip-toplevel-forms (1+ toplevel-number) file)
      (let ((endpos (file-position file)))
	(setq buffer (make-array (list endpos) :element-type 'character
				 :initial-element #\Space))
	(assert (file-position file 0))
	(read-sequence buffer file :end endpos)))
    (source-path-string-position path buffer)))

(defgeneric sexp-in-bounds-p (sexp i)
  (:method ((list list) i)
    (< i (loop for e on list
               count t)))
  (:method ((sexp t) i) nil))

(defgeneric sexp-ref (sexp i)
  (:method ((s list) i) (elt s i)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of the deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for i in path
		     for f = form then (if (sexp-in-bounds-p f i)
					   (sexp-ref f i))
		     collect f)))
    ;; select the first subform present in source-map
    (loop for form in (nreverse forms)
	  for ((start . end) . rest) = (gethash form source-map)
	  when (and start end (not rest))
	  return (return (values start end)))))
