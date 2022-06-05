;;; swank-macrostep.lisp -- fancy macro-expansion via macrostep.el
;;
;; Authors: Luis Oliveira <luismbo@gmail.com>
;;          Jon Oddie <j.j.oddie@gmail.com>
;;
;; License: Public Domain

(defpackage swank-macrostep
  (:use cl swank)
  (:import-from swank
		#:*macroexpand-printer-bindings*
                #:with-buffer-syntax
		#:with-bindings
                #:to-string
                #:macroexpand-all
                #:compiler-macroexpand-1
                #:defslimefun
                #:collect-macro-forms)
  (:export #:macrostep-expand-1
           #:macro-form-p))

(in-package #:swank-macrostep)

(defslimefun macrostep-expand-1 (string compiler-macros? context)
  (with-buffer-syntax ()
    (let ((form (read-from-string string)))
      (multiple-value-bind (expansion error-message)
	  (expand-form-once form compiler-macros? context)
	(if error-message
            `(:error ,error-message)
	    (multiple-value-bind (macros compiler-macros)
		(collect-macro-forms-in-context expansion context)
	      (let* ((all-macros (append macros compiler-macros))
		     (pretty-expansion (pprint-to-string expansion))
		     (positions (collect-form-positions expansion
							pretty-expansion
							all-macros))
                     (subform-info
                      (loop
                         for form in all-macros
                         for (start end) in positions
                         when (and start end)
                         collect (let ((op-name (to-string (first form)))
                                       (op-type
                                        (if (member form macros)
                                            :macro
                                            :compiler-macro)))
                                   (list op-name
                                         op-type
                                         start)))))
		`(:ok ,pretty-expansion ,subform-info))))))))

(defun expand-form-once (form compiler-macros? context)
  (multiple-value-bind (expansion expanded?)
      (macroexpand-1-in-context form context)
    (if expanded?
	(values expansion nil)
	(if (not compiler-macros?)
	    (values nil "Not a macro form")
	    (multiple-value-bind (expansion expanded?)
		(compiler-macroexpand-1 form)
	      (if expanded?
		  (values expansion nil)
		  (values nil "Not a macro or compiler-macro form")))))))

(defslimefun macro-form-p (string compiler-macros? context)
  (with-buffer-syntax ()
    (let ((form
           (handler-case
               (read-from-string string)
             (error (condition)
               (unless (debug-on-swank-error)
                 (return-from macro-form-p
                   `(:error ,(format nil "Read error: ~A" condition))))))))
      `(:ok ,(macro-form-type form compiler-macros? context)))))

(defun macro-form-type (form compiler-macros? context)
  (cond
    ((or (not (consp form))
         (not (symbolp (car form))))
     nil)
    ((multiple-value-bind (expansion expanded?)
         (macroexpand-1-in-context form context)
       (declare (ignore expansion))
       expanded?)
     :macro)
    ((and compiler-macros?
          (multiple-value-bind (expansion expanded?)
              (compiler-macroexpand-1 form)
            (declare (ignore expansion))
            expanded?))
     :compiler-macro)
    (t
     nil)))


;;;; Hacks to support macro-expansion within local context

(defparameter *macrostep-tag* (gensym))

(defparameter *macrostep-placeholder* '*macrostep-placeholder*)

(define-condition expansion-in-context-failed (simple-error)
  ())

(defmacro throw-expansion (form &environment env)
  (throw *macrostep-tag* (macroexpand-1 form env)))

(defmacro throw-collected-macro-forms (form &environment env)
  (throw *macrostep-tag* (collect-macro-forms form env)))

(defun macroexpand-1-in-context (form context)
  (handler-case
      (macroexpand-and-catch
       `(throw-expansion ,form) context)
    (error ()
      (macroexpand-1 form))))

(defun collect-macro-forms-in-context (form context)
  (handler-case
      (macroexpand-and-catch
       `(throw-collected-macro-forms ,form) context)
    (error ()
      (collect-macro-forms form))))

(defun macroexpand-and-catch (form context)
  (catch *macrostep-tag*
    (macroexpand-all (enclose-form-in-context form context))
    (error 'expansion-in-context-failed)))

(defun enclose-form-in-context (form context)
  (with-buffer-syntax ()
    (destructuring-bind (prefix suffix) context
      (let* ((placeholder-form
              (read-from-string
               (concatenate
                'string
                prefix (prin1-to-string *macrostep-placeholder*) suffix)))
             (substituted-form (subst form *macrostep-placeholder*
                                      placeholder-form)))
        (if (not (equal placeholder-form substituted-form))
            substituted-form
            (error 'expansion-in-context-failed))))))


;;;; Tracking Pretty Printer

(defun marker-char-p (char)
  (<= #xe000 (char-code char) #xe8ff))

(defun make-marker-char (id)
  ;; using the private-use characters U+E000..U+F8FF as markers, so
  ;; that's our upper limit for how many we can use.
  (assert (<= 0 id #x8ff))
  (code-char (+ #xe000 id)))

(defun marker-char-id (char)
  (assert (marker-char-p char))
  (- (char-code char) #xe000))

(defparameter +whitespace+ (mapcar #'code-char '(9 13 10 32)))

(defun whitespacep (char)
  (member char +whitespace+))

(defun pprint-to-string (object &optional pprint-dispatch)
  (let ((*print-pprint-dispatch* (or pprint-dispatch *print-pprint-dispatch*)))
    (with-bindings *macroexpand-printer-bindings*
      (to-string object))))

#-clisp
(defun collect-form-positions (expansion printed-expansion forms)
  (loop for (start end)
     in (collect-marker-positions
         (pprint-to-string expansion (make-tracking-pprint-dispatch forms))
         (length forms))
     collect (when (and start end)
               (list (find-non-whitespace-position printed-expansion start)
                     (find-non-whitespace-position printed-expansion end)))))

;; The pprint-dispatch table constructed by
;; MAKE-TRACKING-PPRINT-DISPATCH causes an infinite loop and stack
;; overflow under CLISP version 2.49.  Make the COLLECT-FORM-POSITIONS
;; entry point a no-op in thi case, so that basic macro-expansion will
;; still work (without detection of inner macro forms)
#+clisp
(defun collect-form-positions (expansion printed-expansion forms)
  nil)

(defun make-tracking-pprint-dispatch (forms)
  (let ((original-table *print-pprint-dispatch*)
        (table (copy-pprint-dispatch)))
    (flet ((maybe-write-marker (position stream)
             (when position
               (write-char (make-marker-char position) stream))))
      (set-pprint-dispatch 'cons
                           (lambda (stream cons)
                             (let ((pos (position cons forms)))
                               (maybe-write-marker pos stream)
                               ;; delegate printing to the original table.
                               (funcall (pprint-dispatch cons original-table)
                                        stream
                                        cons)
                               (maybe-write-marker pos stream)))
                           most-positive-fixnum
                           table))
    table))

(defun collect-marker-positions (string position-count)
  (let ((positions (make-array position-count :initial-element nil)))
    (loop with p = 0
          for char across string
          unless (whitespacep char)
            do (if (marker-char-p char)
                   (push p (aref positions (marker-char-id char)))
                   (incf p)))
    (map 'list #'reverse positions)))

(defun find-non-whitespace-position (string position)
  (loop with non-whitespace-position = -1
        for i from 0 and char across string
        unless (whitespacep char)
          do (incf non-whitespace-position)
        until (eql non-whitespace-position position)
        finally (return i)))

(provide :swank-macrostep)
