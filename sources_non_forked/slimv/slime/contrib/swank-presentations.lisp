;;; swank-presentations.lisp --- imitate LispM's presentations
;;
;; Authors: Alan Ruttenberg  <alanr-l@mumble.net>
;;          Luke Gorrie  <luke@synap.se>
;;          Helmut Eller  <heller@common-lisp.net>
;;          Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; License: This code has been placed in the Public Domain.  All warranties
;;          are disclaimed.
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-repl))

;;;; Recording and accessing results of computations

(defvar *record-repl-results* t
  "Non-nil means that REPL results are saved for later lookup.")

(defvar *object-to-presentation-id*
  (make-weak-key-hash-table :test 'eq)
  "Store the mapping of objects to numeric identifiers")

(defvar *presentation-id-to-object*
  (make-weak-value-hash-table :test 'eql)
  "Store the mapping of numeric identifiers to objects")

(defun clear-presentation-tables ()
  (clrhash *object-to-presentation-id*)
  (clrhash *presentation-id-to-object*))

(defvar *presentation-counter* 0 "identifier counter")

(defvar *nil-surrogate* (make-symbol "nil-surrogate"))

;; XXX thread safety? [2006-09-13] mb: not in the slightest (fwiw the
;; rest of slime isn't thread safe either), do we really care?
(defun save-presented-object (object)
  "Save OBJECT and return the assigned id.
If OBJECT was saved previously return the old id."
  (let ((object (if (null object) *nil-surrogate* object)))
    ;; We store *nil-surrogate* instead of nil, to distinguish it from
    ;; an object that was garbage collected.
    (or (gethash object *object-to-presentation-id*)
        (let ((id (incf *presentation-counter*)))
          (setf (gethash id *presentation-id-to-object*) object)
          (setf (gethash object *object-to-presentation-id*) id)
          id))))

(defslimefun lookup-presented-object (id)
  "Retrieve the object corresponding to ID.
The secondary value indicates the absence of an entry."
  (etypecase id
    (integer
     ;;
     (multiple-value-bind (object foundp)
         (gethash id *presentation-id-to-object*)
       (cond
         ((eql object *nil-surrogate*)
          ;; A stored nil object
          (values nil t))
         ((null object)
          ;; Object that was replaced by nil in the weak hash table
          ;; when the object was garbage collected.
          (values nil nil))
         (t
          (values object foundp)))))
    (cons
     (dcase id
       ((:frame-var thread-id frame index)
        (declare (ignore thread-id)) ; later
        (handler-case
            (frame-var-value frame index)
          (t (condition)
            (declare (ignore condition))
            (values nil nil))
          (:no-error (value)
            (values value t))))
       ((:inspected-part part-index)
        (inspector-nth-part part-index))))))

(defslimefun lookup-presented-object-or-lose (id)
  "Get the result of the previous REPL evaluation with ID."
  (multiple-value-bind (object foundp) (lookup-presented-object id)
    (cond (foundp object)
          (t (error "Attempt to access unrecorded object (id ~D)." id)))))

(defslimefun lookup-and-save-presented-object-or-lose (id)
  "Get the object associated with ID and save it in the presentation tables."
  (let ((obj (lookup-presented-object-or-lose id)))
    (save-presented-object obj)))

(defslimefun clear-repl-results ()
  "Forget the results of all previous REPL evaluations."
  (clear-presentation-tables)
  t)

(defun present-repl-results (values)
  ;; Override a function in swank.lisp, so that
  ;; presentations are associated with every REPL result.
  (flet ((send (value)
           (let ((id (and *record-repl-results*
                          (save-presented-object value))))
	     (send-to-emacs `(:presentation-start ,id :repl-result))
	     (send-to-emacs `(:write-string ,(prin1-to-string value)
					    :repl-result))
	     (send-to-emacs `(:presentation-end ,id :repl-result))
	     (send-to-emacs `(:write-string ,(string #\Newline)
					    :repl-result)))))
    (fresh-line)
    (finish-output)
    (if (null values)
        (send-to-emacs `(:write-string "; No value" :repl-result))
        (mapc #'send values))))


;;;; Presentation menu protocol
;;
;; To define a menu for a type of object, define a method
;; menu-choices-for-presentation on that object type.  This function
;; should return a list of two element lists where the first element is
;; the name of the menu action and the second is a function that will be
;; called if the menu is chosen. The function will be called with 3
;; arguments:
;;
;; choice: The string naming the action from above
;;
;; object: The object
;;
;; id: The presentation id of the object
;;
;; You might want append (when (next-method-p) (call-next-method)) to
;; pick up the Menu actions of superclasses.
;;

(defvar *presentation-active-menu* nil)

(defun menu-choices-for-presentation-id (id)
  (multiple-value-bind (ob presentp) (lookup-presented-object id)
    (cond ((not presentp) 'not-present)
	  (t
	   (let ((menu-and-actions (menu-choices-for-presentation ob)))
	     (setq *presentation-active-menu* (cons id menu-and-actions))
	     (mapcar 'car menu-and-actions))))))

(defun swank-ioify (thing)
  (cond ((keywordp thing) thing)
	((and (symbolp thing)(not (find #\: (symbol-name thing))))
	 (intern (symbol-name thing) 'swank-io-package))
	((consp thing) (cons (swank-ioify (car thing))
			     (swank-ioify (cdr thing))))
	(t thing)))

(defun execute-menu-choice-for-presentation-id (id count item)
  (let ((ob (lookup-presented-object id)))
    (assert (equal id (car *presentation-active-menu*)) ()
	    "Bug: Execute menu call for id ~a  but menu has id ~a"
	    id (car *presentation-active-menu*))
    (let ((action (second (nth (1- count) (cdr *presentation-active-menu*)))))
      (swank-ioify (funcall action item ob id)))))


(defgeneric menu-choices-for-presentation (object)
  (:method (ob) (declare (ignore ob)) nil)) ; default method

;; Pathname
(defmethod menu-choices-for-presentation ((ob pathname))
  (let* ((file-exists (ignore-errors (probe-file ob)))
	 (lisp-type (make-pathname :type "lisp"))
	 (source-file (and (not (member (pathname-type ob) '("lisp" "cl")
					:test 'equal))
			   (let ((source (merge-pathnames lisp-type ob)))
			     (and (ignore-errors (probe-file source))
				  source))))
	 (fasl-file (and file-exists
			 (equal (ignore-errors
				  (namestring
				   (truename
				    (compile-file-pathname
				     (merge-pathnames lisp-type ob)))))
				(namestring (truename ob))))))
    (remove nil
	    (list*
	     (and (and file-exists (not fasl-file))
		  (list "Edit this file"
			(lambda(choice object id)
			  (declare (ignore choice id))
			  (ed-in-emacs (namestring (truename object)))
			  nil)))
	     (and file-exists
		  (list "Dired containing directory"
			(lambda (choice object id)
			  (declare (ignore choice id))
			  (ed-in-emacs (namestring
					(truename
					 (merge-pathnames
					  (make-pathname :name "" :type "")
					  object))))
			  nil)))
	     (and fasl-file
		  (list "Load this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object))
			  (load ob)
			  nil)))
	     (and fasl-file
		  (list "Delete this fasl file"
			(lambda (choice object id)
			  (declare (ignore choice id object))
			  (let ((nt (namestring (truename ob))))
			    (when (y-or-n-p-in-emacs "Delete ~a? " nt)
			      (delete-file nt)))
			  nil)))
	     (and source-file
		  (list "Edit lisp source file"
			(lambda (choice object id)
			  (declare (ignore choice id object))
			  (ed-in-emacs (namestring (truename source-file)))
			  nil)))
	     (and source-file
		  (list "Load lisp source file"
			(lambda(choice object id)
			  (declare (ignore choice id object))
			  (load source-file)
			  nil)))
	     (and (next-method-p) (call-next-method))))))

(defmethod menu-choices-for-presentation ((ob function))
  (list (list "Disassemble"
              (lambda (choice object id)
                (declare (ignore choice id))
                (disassemble object)))))

(defslimefun inspect-presentation (id reset-p)
  (let ((what (lookup-presented-object-or-lose id)))
    (when reset-p
      (reset-inspector))
    (inspect-object what)))

(defslimefun init-presentations ()
  ;; FIXME: import/use swank-repl to avoid package qualifier.
  (setq swank-repl:*send-repl-results-function* 'present-repl-results))

(provide :swank-presentations)
