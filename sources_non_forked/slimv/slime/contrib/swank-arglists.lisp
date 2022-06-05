;;; swank-arglists.lisp --- arglist related code ??
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-c-p-c))

;;;; Utilities

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions :initial-value x :from-end t)))

(defun length= (seq n)
  "Test for whether SEQ contains N number of elements. I.e. it's equivalent
 to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
 efficiently implemented."
  (etypecase seq
    (list (do ((i n (1- i))
               (list seq (cdr list)))
              ((or (<= i 0) (null list))
               (and (zerop i) (null list)))))
    (sequence (= (length seq) n))))

(declaim (inline memq))
(defun memq (item list)
  (member item list :test #'eq))

(defun exactly-one-p (&rest values)
  "If exactly one value in VALUES is non-NIL, this value is returned.
Otherwise NIL is returned."
  (let ((found nil))
    (dolist (v values)
      (when v (if found
                  (return-from exactly-one-p nil)
                  (setq found v))))
    found))

(defun valid-operator-symbol-p (symbol)
  "Is SYMBOL the name of a function, a macro, or a special-operator?"
  (or (fboundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)
      (member symbol '(declare declaim))))

(defun function-exists-p (form)
  (and (valid-function-name-p form)
       (fboundp form)
       t))

(defmacro multiple-value-or (&rest forms)
  (if (null forms)
      nil
      (let ((first (first forms))
            (rest (rest forms)))
        `(let* ((values (multiple-value-list ,first))
                (primary-value (first values)))
          (if primary-value
              (values-list values)
              (multiple-value-or ,@rest))))))

(defun arglist-available-p (arglist)
  (not (eql arglist :not-available)))

(defmacro with-available-arglist ((var &rest more-vars) form &body body)
  `(multiple-value-bind (,var ,@more-vars) ,form
     (if (eql ,var :not-available)
         :not-available
         (progn ,@body))))


;;;; Arglist Definition

(defstruct (arglist (:conc-name arglist.) (:predicate arglist-p))
  provided-args         ; list of the provided actual arguments
  required-args         ; list of the required arguments
  optional-args         ; list of the optional arguments
  key-p                 ; whether &key appeared
  keyword-args          ; list of the keywords
  rest                  ; name of the &rest or &body argument (if any)
  body-p                ; whether the rest argument is a &body
  allow-other-keys-p    ; whether &allow-other-keys appeared
  aux-args              ; list of &aux variables
  any-p                 ; whether &any appeared
  any-args              ; list of &any arguments  [*]
  known-junk            ; &whole, &environment
  unknown-junk)         ; unparsed stuff

;;;
;;; [*] The &ANY lambda keyword is an extension to ANSI Common Lisp,
;;;     and is only used to describe certain arglists that cannot be
;;;     described in another way.
;;;
;;;     &ANY is very similiar to &KEY but while &KEY is based upon
;;;     the idea of a plist (key1 value1 key2 value2), &ANY is a
;;;     cross between &OPTIONAL, &KEY and *FEATURES* lists:
;;;
;;;        a) (&ANY :A :B :C) means that you can provide any (non-null)
;;;              set consisting of the keywords `:A', `:B', or `:C' in
;;;              the arglist. E.g. (:A) or (:C :B :A).
;;;
;;;        (This is not restricted to keywords only, but any self-evaluating
;;;         expression is allowed.)
;;;
;;;        b) (&ANY (key1 v1) (key2 v2) (key3 v3)) means that you can
;;;              provide any (non-null) set consisting of lists where
;;;              the CAR of the list is one of `key1', `key2', or `key3'.
;;;              E.g. ((key1 100) (key3 42)), or ((key3 66) (key2 23))
;;;
;;;
;;;     For example, a) let us describe the situations of EVAL-WHEN as
;;;
;;;     (EVAL-WHEN (&ANY :compile-toplevel :load-toplevel :execute) &BODY body)
;;;
;;;     and b) let us describe the optimization qualifiers that are valid
;;;     in the declaration specifier `OPTIMIZE':
;;;
;;;       (DECLARE (OPTIMIZE &ANY (compilation-speed 1) (safety 1) ...))
;;;

;; This is a wrapper object around anything that came from Slime and
;; could not reliably be read.
(defstruct (arglist-dummy
	     (:conc-name #:arglist-dummy.)
             (:constructor make-arglist-dummy (string-representation)))
  string-representation)

(defun empty-arg-p (dummy)
  (and (arglist-dummy-p dummy)
       (zerop (length (arglist-dummy.string-representation dummy)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +lambda-list-keywords+
    '(&provided &required &optional &rest &key &any)))

(defmacro do-decoded-arglist (decoded-arglist &body clauses)
  (assert (loop for clause in clauses
		thereis (member (car clause) +lambda-list-keywords+)))
  (flet ((parse-clauses (clauses)
	   (let* ((size    (length +lambda-list-keywords+))
		  (initial (make-hash-table :test #'eq :size size))
		  (main    (make-hash-table :test #'eq :size size))
		  (final   (make-hash-table :test #'eq :size size)))
	     (loop for clause in clauses
		   for lambda-list-keyword = (first clause)
		   for clause-parameter    = (second clause)
		   do
                   (case clause-parameter
                     (:initially
                      (setf (gethash lambda-list-keyword initial) clause))
                     (:finally
                      (setf (gethash lambda-list-keyword final) clause))
                     (t
                      (setf (gethash lambda-list-keyword main) clause)))
		   finally
                   (return (values initial main final)))))
	 (generate-main-clause (clause arglist)
	   (dcase clause
             ((&provided (&optional arg) . body)
              (let ((gensym (gensym "PROVIDED-ARG+")))
		`(dolist (,gensym (arglist.provided-args ,arglist))
		   (declare (ignorable ,gensym))
		   (let (,@(when arg `((,arg ,gensym))))
		     ,@body))))
	     ((&required (&optional arg) . body)
	      (let ((gensym (gensym "REQUIRED-ARG+")))
		`(dolist (,gensym (arglist.required-args ,arglist))
		   (declare (ignorable ,gensym))
		   (let (,@(when arg `((,arg ,gensym))))
		     ,@body))))
	     ((&optional (&optional arg init) . body)
	      (let ((optarg (gensym "OPTIONAL-ARG+")))
		`(dolist (,optarg (arglist.optional-args ,arglist))
		   (declare (ignorable ,optarg))
		   (let (,@(when arg
                             `((,arg (optional-arg.arg-name ,optarg))))
			 ,@(when init
                             `((,init (optional-arg.default-arg ,optarg)))))
		     ,@body))))
	     ((&key (&optional keyword arg init) . body)
	      (let ((keyarg (gensym "KEY-ARG+")))
		`(dolist (,keyarg (arglist.keyword-args ,arglist))
		   (declare (ignorable ,keyarg))
		   (let (,@(when keyword
                             `((,keyword (keyword-arg.keyword ,keyarg))))
			 ,@(when arg
                             `((,arg (keyword-arg.arg-name ,keyarg))))
			 ,@(when init
                             `((,init (keyword-arg.default-arg ,keyarg)))))
		     ,@body))))
	     ((&rest (&optional arg body-p) . body)
	      `(when (arglist.rest ,arglist)
		 (let (,@(when arg    `((,arg (arglist.rest ,arglist))))
		       ,@(when body-p `((,body-p (arglist.body-p ,arglist)))))
		   ,@body)))
	     ((&any (&optional arg) . body)
              (let ((gensym (gensym "REQUIRED-ARG+")))
                `(dolist (,gensym (arglist.any-args ,arglist))
                    (declare (ignorable ,gensym))
                    (let (,@(when arg `((,arg ,gensym))))
                      ,@body)))))))
    (let ((arglist (gensym "DECODED-ARGLIST+")))
      (multiple-value-bind (initially-clauses main-clauses finally-clauses)
	  (parse-clauses clauses)
	`(let ((,arglist ,decoded-arglist))
	   (block do-decoded-arglist
	     ,@(loop for keyword in '(&provided &required
                                      &optional &rest &key &any)
		     append (cddr (gethash keyword initially-clauses))
		     collect (let ((clause (gethash keyword main-clauses)))
			       (when clause
                                 (generate-main-clause clause arglist)))
		     append (cddr (gethash keyword finally-clauses)))))))))

;;;; Arglist Printing

(defun undummy (x)
  (if (typep x 'arglist-dummy)
      (arglist-dummy.string-representation x)
      (prin1-to-string x)))

(defun print-decoded-arglist (arglist &key operator provided-args highlight)
  (let ((first-space-after-operator (and operator t)))
    (macrolet ((space ()
                 ;; Kludge: When OPERATOR is not given, we don't want to
                 ;; print a space for the first argument.
                 `(if (not operator)
                      (setq operator t)
                      (progn (write-char #\space)
                             (if first-space-after-operator
                                 (setq first-space-after-operator nil)
                                 (pprint-newline :fill)))))
               (with-highlighting ((&key index) &body body)
                 `(if (eql ,index (car highlight))
                      (progn (princ "===> ") ,@body (princ " <==="))
                      (progn ,@body)))
               (print-arglist-recursively (argl &key index)
                 `(if (eql ,index (car highlight))
                      (print-decoded-arglist ,argl :highlight (cdr highlight))
                      (print-decoded-arglist ,argl))))
      (let ((index 0))
        (pprint-logical-block (nil nil :prefix "(" :suffix ")")
          (when operator
            (print-arg operator)
            (pprint-indent :current 1)) ; 1 due to possibly added space
          (do-decoded-arglist (remove-given-args arglist provided-args)
            (&provided (arg)
                       (space)
                       (print-arg arg :literal-strings t)
                       (incf index))
            (&required (arg)
                       (space)
                       (if (arglist-p arg)
                           (print-arglist-recursively arg :index index)
                           (with-highlighting (:index index)
                             (print-arg arg)))
                       (incf index))
            (&optional :initially
                       (when (arglist.optional-args arglist)
                         (space)
                         (princ '&optional)))
            (&optional (arg init-value)
                       (space)
                       (if (arglist-p arg)
                           (print-arglist-recursively arg :index index)
                           (with-highlighting (:index index)
                             (if (null init-value)
                                 (print-arg arg)
                                 (format t "~:@<~A ~A~@:>"
                                         (undummy arg) (undummy init-value)))))
                       (incf index))
            (&key :initially
                  (when (arglist.key-p arglist)
                    (space)
                    (princ '&key)))
            (&key (keyword arg init)
                  (space)
                  (if (arglist-p arg)
                      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                        (prin1 keyword) (space)
                        (print-arglist-recursively arg :index keyword))
                      (with-highlighting (:index keyword)
                        (cond ((and init (keywordp keyword))
                               (format t "~:@<~A ~A~@:>" keyword (undummy init)))
                              (init
                               (format t "~:@<(~A ..) ~A~@:>"
                                       (undummy keyword) (undummy init)))
                              ((not (keywordp keyword))
                               (format t "~:@<(~S ..)~@:>" keyword))
                              (t
                               (princ keyword))))))
            (&key :finally
                  (when (arglist.allow-other-keys-p arglist)
                    (space)
                    (princ '&allow-other-keys)))
            (&any :initially
                  (when (arglist.any-p arglist)
                    (space)
                    (princ '&any)))
            (&any (arg)
                  (space)
                  (print-arg arg))
            (&rest (args bodyp)
                   (space)
                   (princ (if bodyp '&body '&rest))
                   (space)
                   (if (arglist-p args)
                       (print-arglist-recursively args :index index)
                       (with-highlighting (:index index)
                         (print-arg args))))
            ;; FIXME: add &UNKNOWN-JUNK?
            ))))))

(defun print-arg (arg &key literal-strings)
  (let ((arg (if (arglist-dummy-p arg)
                 (arglist-dummy.string-representation arg)
                 arg)))
    (if (or
         (and literal-strings
              (stringp arg))
         (keywordp arg))
        (prin1 arg)
        (princ arg))))

(defun print-decoded-arglist-as-template (decoded-arglist &key
                                          (prefix "(") (suffix ")"))
  (let ((first-p t))
    (flet ((space ()
             (unless first-p
               (write-char #\space))
             (setq first-p nil))
           (print-arg-or-pattern (arg)
             (etypecase arg
               (symbol        (if (keywordp arg) (prin1 arg) (princ arg)))
               (string        (princ arg))
               (list          (princ arg))
               (arglist-dummy (princ
                               (arglist-dummy.string-representation arg)))
               (arglist       (print-decoded-arglist-as-template arg)))
             (pprint-newline :fill)))
      (pprint-logical-block (nil nil :prefix prefix :suffix suffix)
        (do-decoded-arglist decoded-arglist
          (&provided ()) ; do nothing; provided args are in the buffer already.
          (&required (arg)
            (space) (print-arg-or-pattern arg))
          (&optional (arg)
            (space) (princ "[") (print-arg-or-pattern arg) (princ "]"))
          (&key (keyword arg)
            (space)
            (prin1 (if (keywordp keyword) keyword `',keyword))
            (space)
            (print-arg-or-pattern arg)
            (pprint-newline :linear))
          (&any (arg)
            (space) (print-arg-or-pattern arg))
          (&rest (args)
            (when (or (not (arglist.keyword-args decoded-arglist))
                      (arglist.allow-other-keys-p decoded-arglist))
              (space)
              (format t "~A..." args))))))))

(defvar *arglist-pprint-bindings*
  '((*print-case*     . :downcase)
    (*print-pretty*   . t)
    (*print-circle*   . nil)
    (*print-readably* . nil)
    (*print-level*    . 10)
    (*print-length*   . 20)
    (*print-escape*   . nil)))

(defvar *arglist-show-packages* t)

(defmacro with-arglist-io-syntax (&body body)
  (let ((package (gensym)))
    `(let ((,package *package*))
       (with-standard-io-syntax
         (let ((*package* (if *arglist-show-packages*
                              *package*
                              ,package)))
           (with-bindings *arglist-pprint-bindings*
             ,@body))))))

(defun decoded-arglist-to-string (decoded-arglist
                                  &key operator highlight
                                  print-right-margin)
  (with-output-to-string (*standard-output*)
    (with-arglist-io-syntax
      (let ((*print-right-margin* print-right-margin))
        (print-decoded-arglist decoded-arglist
                               :operator operator
                               :highlight highlight)))))

(defun decoded-arglist-to-template-string (decoded-arglist
                                           &key (prefix "(") (suffix ")"))
  (with-output-to-string (*standard-output*)
    (with-arglist-io-syntax
      (print-decoded-arglist-as-template decoded-arglist
                                         :prefix prefix
                                         :suffix suffix))))

;;;; Arglist Decoding / Encoding

(defun decode-required-arg (arg)
  "ARG can be a symbol or a destructuring pattern."
  (etypecase arg
    (symbol        arg)
    (arglist-dummy arg)
    (list          (decode-arglist arg))))

(defun encode-required-arg (arg)
  (etypecase arg
    (symbol arg)
    (arglist (encode-arglist arg))))

(defstruct (keyword-arg
            (:conc-name keyword-arg.)
            (:constructor %make-keyword-arg))
  keyword
  arg-name
  default-arg)

(defun canonicalize-default-arg (form)
  (if (equalp ''nil form)
      nil
      form))

(defun make-keyword-arg (keyword arg-name default-arg)
  (%make-keyword-arg :keyword keyword
                     :arg-name arg-name
                     :default-arg (canonicalize-default-arg default-arg)))

(defun decode-keyword-arg (arg)
  "Decode a keyword item of formal argument list.
Return three values: keyword, argument name, default arg."
  (flet ((intern-as-keyword (arg)
           (intern (etypecase arg
                     (symbol (symbol-name arg))
                     (arglist-dummy (arglist-dummy.string-representation arg)))
                   keyword-package)))
    (cond ((or (symbolp arg) (arglist-dummy-p arg))
           (make-keyword-arg (intern-as-keyword arg) arg nil))
          ((and (consp arg)
                (consp (car arg)))
           (make-keyword-arg (caar arg)
                             (decode-required-arg (cadar arg))
                             (cadr arg)))
          ((consp arg)
           (make-keyword-arg (intern-as-keyword (car arg))
                             (car arg) (cadr arg)))
          (t
           (error "Bad keyword item of formal argument list")))))

(defun encode-keyword-arg (arg)
  (cond
    ((arglist-p (keyword-arg.arg-name arg))
     ;; Destructuring pattern
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (encode-required-arg
                                (keyword-arg.arg-name arg)))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))
    ((eql (intern (symbol-name (keyword-arg.arg-name arg))
                  keyword-package)
          (keyword-arg.keyword arg))
     (if (keyword-arg.default-arg arg)
         (list (keyword-arg.arg-name arg)
               (keyword-arg.default-arg arg))
         (keyword-arg.arg-name arg)))
    (t
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (keyword-arg.arg-name arg))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))))

(progn
  (assert (equalp (decode-keyword-arg 'x)
                  (make-keyword-arg :x 'x nil)))
  (assert (equalp (decode-keyword-arg '(x t))
                  (make-keyword-arg :x 'x t)))
  (assert (equalp (decode-keyword-arg '((:x y)))
                  (make-keyword-arg :x 'y nil)))
  (assert (equalp (decode-keyword-arg '((:x y) t))
                  (make-keyword-arg :x 'y t))))

;;; FIXME suppliedp?
(defstruct (optional-arg
            (:conc-name optional-arg.)
            (:constructor %make-optional-arg))
  arg-name
  default-arg)

(defun make-optional-arg (arg-name default-arg)
  (%make-optional-arg :arg-name arg-name
                      :default-arg (canonicalize-default-arg default-arg)))

(defun decode-optional-arg (arg)
  "Decode an optional item of a formal argument list.
Return an OPTIONAL-ARG structure."
  (etypecase arg
    (symbol        (make-optional-arg arg nil))
    (arglist-dummy (make-optional-arg arg nil))
    (list          (make-optional-arg (decode-required-arg (car arg))
                                      (cadr arg)))))

(defun encode-optional-arg (optional-arg)
  (if (or (optional-arg.default-arg optional-arg)
          (arglist-p (optional-arg.arg-name optional-arg)))
      (list (encode-required-arg
             (optional-arg.arg-name optional-arg))
            (optional-arg.default-arg optional-arg))
      (optional-arg.arg-name optional-arg)))

(progn
  (assert (equalp (decode-optional-arg 'x)
                  (make-optional-arg 'x nil)))
  (assert (equalp (decode-optional-arg '(x t))
                  (make-optional-arg 'x t))))

(define-modify-macro nreversef () nreverse "Reverse the list in PLACE.")

(defun decode-arglist (arglist)
  "Parse the list ARGLIST and return an ARGLIST structure."
  (if (eq arglist :not-available)
      :not-available
      (loop
        with mode = nil
        with result = (make-arglist)
        for arg = (if (consp arglist)
                      (pop arglist)
                      (progn
                        (prog1 arglist
                          (setf mode '&rest
                                arglist nil))))
        do (cond
             ((eql mode '&unknown-junk)
              ;; don't leave this mode -- we don't know how the arglist
              ;; after unknown lambda-list keywords is interpreted
              (push arg (arglist.unknown-junk result)))
             ((eql arg '&allow-other-keys)
              (setf (arglist.allow-other-keys-p result) t))
             ((eql arg '&key)
              (setf (arglist.key-p result) t
                    mode arg))
             ((memq arg '(&optional &rest &body &aux))
              (setq mode arg))
             ((memq arg '(&whole &environment))
              (setq mode arg)
              (push arg (arglist.known-junk result)))
             ((and (symbolp arg)
                   (string= (symbol-name arg) (string '#:&any))) ; may be interned
              (setf (arglist.any-p result) t) ;  in any *package*.
              (setq mode '&any))
             ((memq arg lambda-list-keywords)
              (setq mode '&unknown-junk)
              (push arg (arglist.unknown-junk result)))
             (t
              (ecase mode
                (&key
                 (push (decode-keyword-arg arg)
                       (arglist.keyword-args result)))
                (&optional
                 (push (decode-optional-arg arg)
                       (arglist.optional-args result)))
                (&body
                 (setf (arglist.body-p result) t
                       (arglist.rest result) arg))
                (&rest
                 (setf (arglist.rest result) arg))
                (&aux
                 (push (decode-optional-arg arg)
                       (arglist.aux-args result)))
                ((nil)
                 (push (decode-required-arg arg)
                       (arglist.required-args result)))
                ((&whole &environment)
                 (setf mode nil)
                 (push arg (arglist.known-junk result)))
                (&any
                 (push arg (arglist.any-args result))))))
        until (null arglist)
        finally (nreversef (arglist.required-args result))
        finally (nreversef (arglist.optional-args result))
        finally (nreversef (arglist.keyword-args result))
        finally (nreversef (arglist.aux-args result))
        finally (nreversef (arglist.any-args result))
        finally (nreversef (arglist.known-junk result))
        finally (nreversef (arglist.unknown-junk result))
        finally (assert (or (and (not (arglist.key-p result))
                                 (not (arglist.any-p result)))
                            (exactly-one-p (arglist.key-p result)
                                           (arglist.any-p result))))
        finally (return result))))

(defun encode-arglist (decoded-arglist)
  (append (mapcar #'encode-required-arg
                  (arglist.required-args decoded-arglist))
          (when (arglist.optional-args decoded-arglist)
            '(&optional))
          (mapcar #'encode-optional-arg
                  (arglist.optional-args decoded-arglist))
          (when (arglist.key-p decoded-arglist)
            '(&key))
          (mapcar #'encode-keyword-arg
                  (arglist.keyword-args decoded-arglist))
          (when (arglist.allow-other-keys-p decoded-arglist)
            '(&allow-other-keys))
          (when (arglist.any-args decoded-arglist)
            `(&any ,@(arglist.any-args decoded-arglist)))
          (cond ((not (arglist.rest decoded-arglist))
                 '())
                ((arglist.body-p decoded-arglist)
                 `(&body ,(arglist.rest decoded-arglist)))
                (t
                 `(&rest ,(arglist.rest decoded-arglist))))
          (when (arglist.aux-args decoded-arglist)
            `(&aux ,(arglist.aux-args decoded-arglist)))
          (arglist.known-junk decoded-arglist)
          (arglist.unknown-junk decoded-arglist)))

;;;; Arglist Enrichment

(defun arglist-keywords (lambda-list)
  "Return the list of keywords in ARGLIST.
As a secondary value, return whether &allow-other-keys appears."
  (let ((decoded-arglist (decode-arglist lambda-list)))
    (values (arglist.keyword-args decoded-arglist)
	    (arglist.allow-other-keys-p decoded-arglist))))


(defun methods-keywords (methods)
  "Collect all keywords in the arglists of METHODS.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (let ((keywords '())
	(allow-other-keys nil))
    (dolist (method methods)
      (multiple-value-bind (kw aok)
	  (arglist-keywords
	   (swank-mop:method-lambda-list method))
	(setq keywords (remove-duplicates (append keywords kw)
                                          :key #'keyword-arg.keyword)
	      allow-other-keys (or allow-other-keys aok))))
    (values keywords allow-other-keys)))

(defun generic-function-keywords (generic-function)
  "Collect all keywords in the methods of GENERIC-FUNCTION.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (methods-keywords
   (swank-mop:generic-function-methods generic-function)))

(defun applicable-methods-keywords (generic-function arguments)
  "Collect all keywords in the methods of GENERIC-FUNCTION that are
applicable for argument of CLASSES.  As a secondary value, return
whether &allow-other-keys appears somewhere."
  (methods-keywords
   (multiple-value-bind (amuc okp)
       (swank-mop:compute-applicable-methods-using-classes
        generic-function (mapcar #'class-of arguments))
     (if okp
         amuc
         (compute-applicable-methods generic-function arguments)))))

(defgeneric extra-keywords (operator args)
   (:documentation "Return a list of extra keywords of OPERATOR (a
symbol) when applied to the (unevaluated) ARGS.
As a secondary value, return whether other keys are allowed.
As a tertiary value, return the initial sublist of ARGS that was needed
to determine the extra keywords."))

;;; We make sure that symbol-from-KEYWORD-using keywords come before
;;; symbol-from-arbitrary-package-using keywords. And we sort the
;;; latter according to how their home-packages relate to *PACKAGE*.
;;;
;;; Rationale is to show those key parameters first which make most
;;; sense in the current context. And in particular: to put
;;; implementation-internal stuff last.
;;;
;;; This matters tremendeously on Allegro in combination with
;;; AllegroCache as that does some evil tinkering with initargs,
;;; obfuscating the arglist of MAKE-INSTANCE.
;;;

(defmethod extra-keywords :around (op args)
  (declare (ignorable op args))
  (multiple-value-bind (keywords aok enrichments) (call-next-method)
    (values (sort-extra-keywords keywords) aok enrichments)))

(defun make-package-comparator (reference-packages)
  "Returns a two-argument test function which compares packages
according to their used-by relation with REFERENCE-PACKAGES. Packages
will be sorted first which appear first in the PACKAGE-USE-LIST of the
reference packages."
  (let ((package-use-table (make-hash-table :test 'eq)))
    ;; Walk the package dependency graph breadth-fist, and fill
    ;; PACKAGE-USE-TABLE accordingly.
    (loop with queue = (copy-list reference-packages)
	  with bfn   = 0		; Breadth-First Number
	  for p      = (pop queue)
	  unless (gethash p package-use-table)
	    do      (setf (gethash p package-use-table) (shiftf bfn (1+ bfn)))
	    and do  (setf queue (nconc queue (copy-list (package-use-list p))))
	  while queue)
    #'(lambda (p1 p2)
	(let ((bfn1 (gethash p1 package-use-table))
	      (bfn2 (gethash p2 package-use-table)))
	  (cond ((and bfn1 bfn2) (<= bfn1 bfn2))
		(bfn1            bfn1)
		(bfn2            nil)	; p2 is used, p1 not
		(t (string<= (package-name p1) (package-name p2))))))))

(defun sort-extra-keywords (kwds)
  (stable-sort kwds (make-package-comparator (list keyword-package *package*))
               :key (compose #'symbol-package #'keyword-arg.keyword)))

(defun keywords-of-operator (operator)
  "Return a list of KEYWORD-ARGs that OPERATOR accepts.
This function is useful for writing EXTRA-KEYWORDS methods for
user-defined functions which are declared &ALLOW-OTHER-KEYS and which
forward keywords to OPERATOR."
  (with-available-arglist (arglist) (arglist-from-form (ensure-list operator))
    (values (arglist.keyword-args arglist)
            (arglist.allow-other-keys-p arglist))))

(defmethod extra-keywords (operator args)
  ;; default method
  (declare (ignore args))
  (let ((symbol-function (symbol-function operator)))
    (if (typep symbol-function 'generic-function)
        (generic-function-keywords symbol-function)
        nil)))

(defun class-from-class-name-form (class-name-form)
  (when (and (listp class-name-form)
             (= (length class-name-form) 2)
             (eq (car class-name-form) 'quote))
    (let* ((class-name (cadr class-name-form))
           (class (find-class class-name nil)))
      (when (and class
                 (not (swank-mop:class-finalized-p class)))
        ;; Try to finalize the class, which can fail if
        ;; superclasses are not defined yet
        (ignore-errors (swank-mop:finalize-inheritance class)))
      class)))

(defun extra-keywords/slots (class)
  (multiple-value-bind (slots allow-other-keys-p)
      (if (swank-mop:class-finalized-p class)
          (values (swank-mop:class-slots class) nil)
          (values (swank-mop:class-direct-slots class) t))
    (let ((slot-init-keywords
            (loop for slot in slots append
                  (mapcar (lambda (initarg)
                            (make-keyword-arg
                             initarg
                             (swank-mop:slot-definition-name slot)
                             (and (swank-mop:slot-definition-initfunction slot)
                                  (swank-mop:slot-definition-initform slot))))
                          (swank-mop:slot-definition-initargs slot)))))
      (values slot-init-keywords allow-other-keys-p))))

(defun extra-keywords/make-instance (operator args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (multiple-value-bind (allocate-instance-keywords ai-aokp)
              (applicable-methods-keywords
               #'allocate-instance (list class))
            (multiple-value-bind (initialize-instance-keywords ii-aokp)
                (ignore-errors
                 (applicable-methods-keywords
                  #'initialize-instance
                  (list (swank-mop:class-prototype class))))
              (multiple-value-bind (shared-initialize-keywords si-aokp)
                  (ignore-errors
                   (applicable-methods-keywords
                    #'shared-initialize
                    (list (swank-mop:class-prototype class) t)))
                (values (append slot-init-keywords
                                allocate-instance-keywords
                                initialize-instance-keywords
                                shared-initialize-keywords)
                        (or class-aokp ai-aokp ii-aokp si-aokp)
                        (list class-name-form))))))))))

(defun extra-keywords/change-class (operator args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (declare (ignore class-aokp))
          (multiple-value-bind (shared-initialize-keywords si-aokp)
              (ignore-errors
                (applicable-methods-keywords
                 #'shared-initialize
                 (list (swank-mop:class-prototype class) t)))
            ;; FIXME: much as it would be nice to include the
            ;; applicable keywords from
            ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, I don't really see
            ;; how to do it: so we punt, always declaring
            ;; &ALLOW-OTHER-KEYS.
            (declare (ignore si-aokp))
            (values (append slot-init-keywords shared-initialize-keywords)
                    t
                    (list class-name-form))))))))

(defmethod extra-keywords ((operator (eql 'make-instance))
                           args)
  (multiple-value-or (extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'make-condition))
                           args)
  (multiple-value-or (extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'error))
                           args)
  (multiple-value-or (extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'signal))
                           args)
  (multiple-value-or (extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'warn))
                           args)
  (multiple-value-or (extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'cerror))
                           args)
  (multiple-value-bind (keywords aok determiners)
      (extra-keywords/make-instance operator (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defmethod extra-keywords ((operator (eql 'change-class))
                           args)
  (multiple-value-bind (keywords aok determiners)
      (extra-keywords/change-class operator (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defun enrich-decoded-arglist-with-keywords (decoded-arglist keywords
                                             allow-other-keys-p)
  "Modify DECODED-ARGLIST using KEYWORDS and ALLOW-OTHER-KEYS-P."
  (when keywords
    (setf (arglist.key-p decoded-arglist) t)
    (setf (arglist.keyword-args decoded-arglist)
          (remove-duplicates
           (append (arglist.keyword-args decoded-arglist)
                   keywords)
           :key #'keyword-arg.keyword)))
  (setf (arglist.allow-other-keys-p decoded-arglist)
        (or (arglist.allow-other-keys-p decoded-arglist)
            allow-other-keys-p)))

(defun enrich-decoded-arglist-with-extra-keywords (decoded-arglist form)
  "Determine extra keywords from the function call FORM, and modify
DECODED-ARGLIST to include them.  As a secondary return value, return
the initial sublist of ARGS that was needed to determine the extra
keywords.  As a tertiary return value, return whether any enrichment
was done."
  (multiple-value-bind (extra-keywords extra-aok determining-args)
      (extra-keywords (car form) (cdr form))
    ;; enrich the list of keywords with the extra keywords
    (enrich-decoded-arglist-with-keywords decoded-arglist
                                          extra-keywords extra-aok)
    (values decoded-arglist
            determining-args
            (or extra-keywords extra-aok))))

(defgeneric compute-enriched-decoded-arglist (operator-form argument-forms)
  (:documentation
   "Return three values: DECODED-ARGLIST, DETERMINING-ARGS, and
ANY-ENRICHMENT, just like enrich-decoded-arglist-with-extra-keywords.
If the arglist is not available, return :NOT-AVAILABLE."))

(defmethod compute-enriched-decoded-arglist (operator-form argument-forms)
  (with-available-arglist (decoded-arglist)
      (decode-arglist (arglist operator-form))
    (enrich-decoded-arglist-with-extra-keywords decoded-arglist
                                                (cons operator-form
                                                      argument-forms))))

(defmethod compute-enriched-decoded-arglist
    ((operator-form (eql 'with-open-file)) argument-forms)
  (declare (ignore argument-forms))
  (multiple-value-bind (decoded-arglist determining-args)
      (call-next-method)
    (let ((first-arg (first (arglist.required-args decoded-arglist)))
          (open-arglist (compute-enriched-decoded-arglist 'open nil)))
      (when (and (arglist-p first-arg) (arglist-p open-arglist))
        (enrich-decoded-arglist-with-keywords
         first-arg
         (arglist.keyword-args open-arglist)
         nil)))
    (values decoded-arglist determining-args t)))

(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'apply))
                                             argument-forms)
  (let ((function-name-form (car argument-forms)))
    (when (and (listp function-name-form)
               (length= function-name-form 2)
               (memq (car function-name-form) '(quote function)))
      (let ((function-name (cadr function-name-form)))
        (when (valid-operator-symbol-p function-name)
          (let ((function-arglist
                 (compute-enriched-decoded-arglist function-name
                                                   (cdr argument-forms))))
            (return-from compute-enriched-decoded-arglist
              (values
               (make-arglist :required-args
                             (list 'function)
                             :optional-args
                             (append
                              (mapcar #'(lambda (arg)
                                          (make-optional-arg arg nil))
                                      (arglist.required-args function-arglist))
                              (arglist.optional-args function-arglist))
                             :key-p
                             (arglist.key-p function-arglist)
                             :keyword-args
                             (arglist.keyword-args function-arglist)
                             :rest
                             'args
                             :allow-other-keys-p
                             (arglist.allow-other-keys-p function-arglist))
               (list function-name-form)
               t)))))))
  (call-next-method))

(defmethod compute-enriched-decoded-arglist
    ((operator-form (eql 'multiple-value-call)) argument-forms)
  (compute-enriched-decoded-arglist 'apply argument-forms))

(defun delete-given-args (decoded-arglist args)
  "Delete given ARGS from DECODED-ARGLIST."
  (macrolet ((pop-or-return (list)
	       `(if (null ,list)
		    (return-from do-decoded-arglist)
		    (pop ,list))))
    (do-decoded-arglist decoded-arglist
      (&provided ()
       (assert (eq (pop-or-return args)
                   (pop (arglist.provided-args decoded-arglist)))))
      (&required ()
       (pop-or-return args)
       (pop (arglist.required-args decoded-arglist)))
      (&optional ()
       (pop-or-return args)
       (pop (arglist.optional-args decoded-arglist)))
      (&key (keyword)
       ;; N.b. we consider a keyword to be given only when the keyword
       ;; _and_ a value has been given for it.
       (loop for (key value) on args by #'cddr
	     when (and (eq keyword key) value)
	       do (setf (arglist.keyword-args decoded-arglist)
			(remove keyword (arglist.keyword-args decoded-arglist)
				:key #'keyword-arg.keyword))))))
  decoded-arglist)

(defun remove-given-args (decoded-arglist args)
  ;; FIXME: We actually needa deep copy here.
  (delete-given-args (copy-arglist decoded-arglist) args))

;;;; Arglist Retrieval

(defun arglist-from-form (form)
  (if (null form)
      :not-available
      (arglist-dispatch (car form) (cdr form))))

(export 'arglist-dispatch)
(defgeneric arglist-dispatch (operator arguments)
  ;; Default method
  (:method (operator arguments)
    (unless (and (symbolp operator) (valid-operator-symbol-p operator))
      (return-from arglist-dispatch :not-available))
    (when (equalp (package-name (symbol-package operator)) "closer-mop")
      (let ((standard-symbol (or (find-symbol (symbol-name operator) :cl)
                                 (find-symbol (symbol-name operator) :swank-mop))))
        (when standard-symbol
          (return-from arglist-dispatch
            (arglist-dispatch standard-symbol arguments)))))

    (multiple-value-bind (decoded-arglist determining-args)
        (compute-enriched-decoded-arglist operator arguments)
      (with-available-arglist (arglist) decoded-arglist
        ;; replace some formal args by determining actual args
        (setf arglist (delete-given-args arglist determining-args))
        (setf (arglist.provided-args arglist) determining-args)
        arglist))))

(defmethod arglist-dispatch ((operator (eql 'defmethod)) arguments)
  (match (cons operator arguments)
    (('defmethod (#'function-exists-p gf-name) . rest)
     (let ((gf (fdefinition gf-name)))
       (when (typep gf 'generic-function)
         (let ((lambda-list (swank-mop:generic-function-lambda-list gf)))
           (with-available-arglist (arglist) (decode-arglist lambda-list)
             (let ((qualifiers (loop for x in rest
                                     until (or (listp x) (empty-arg-p x))
                                     collect x)))
               (return-from arglist-dispatch
                 (make-arglist :provided-args (cons gf-name qualifiers)
                               :required-args (list arglist)
                               :rest "body" :body-p t))))))))
    (_)) ; Fall through
  (call-next-method))

(defmethod arglist-dispatch ((operator (eql 'define-compiler-macro)) arguments)
  (match (cons operator arguments)
    (('define-compiler-macro (#'function-exists-p gf-name) . _)
     (let ((gf (fdefinition gf-name)))
       (with-available-arglist (arglist) (decode-arglist (arglist gf))
         (return-from arglist-dispatch
           (make-arglist :provided-args (list gf-name)
                         :required-args (list arglist)
                         :rest "body" :body-p t)))))
    (_)) ; Fall through
  (call-next-method))


(defmethod arglist-dispatch ((operator (eql 'eval-when)) arguments)
  (declare (ignore arguments))
    (let ((eval-when-args '(:compile-toplevel :load-toplevel :execute)))
    (make-arglist
     :required-args (list (make-arglist :any-p t :any-args eval-when-args))
     :rest '#:body :body-p t)))


(defmethod arglist-dispatch ((operator (eql 'declare)) arguments)
  (let* ((declaration      (cons operator (last arguments)))
         (typedecl-arglist (arglist-for-type-declaration declaration)))
    (if (arglist-available-p typedecl-arglist)
        typedecl-arglist
        (match declaration
          (('declare ((#'consp typespec) . decl-args))
           (with-available-arglist (typespec-arglist)
               (decoded-arglist-for-type-specifier typespec)
             (make-arglist
              :required-args (list (make-arglist
                                    :required-args (list typespec-arglist)
                                    :rest '#:variables)))))
          (('declare (decl-identifier . decl-args))
           (decoded-arglist-for-declaration decl-identifier decl-args))
          (_ (make-arglist :rest '#:declaration-specifiers))))))

(defmethod arglist-dispatch ((operator (eql 'declaim)) arguments)
  (arglist-dispatch 'declare arguments))


(defun arglist-for-type-declaration (declaration)
  (flet ((%arglist-for-type-declaration (identifier typespec rest-var-name)
           (with-available-arglist (typespec-arglist)
               (decoded-arglist-for-type-specifier typespec)
             (make-arglist
              :required-args (list (make-arglist
                                    :provided-args (list identifier)
                                    :required-args (list typespec-arglist)
                                    :rest rest-var-name))))))
    (match declaration
      (('declare ('type (#'consp typespec) . decl-args))
       (%arglist-for-type-declaration 'type typespec '#:variables))
      (('declare ('ftype (#'consp typespec) . decl-args))
       (%arglist-for-type-declaration 'ftype typespec '#:function-names))
      (('declare ((#'consp typespec) . decl-args))
       (with-available-arglist (typespec-arglist)
           (decoded-arglist-for-type-specifier typespec)
         (make-arglist
          :required-args (list (make-arglist
                                :required-args (list typespec-arglist)
                                :rest '#:variables)))))
      (_ :not-available))))

(defun decoded-arglist-for-declaration (decl-identifier decl-args)
  (declare (ignore decl-args))
    (with-available-arglist (arglist)
      (decode-arglist (declaration-arglist decl-identifier))
    (setf (arglist.provided-args arglist) (list decl-identifier))
    (make-arglist :required-args (list arglist))))

(defun decoded-arglist-for-type-specifier (type-specifier)
  (etypecase type-specifier
    (arglist-dummy :not-available)
    (cons (decoded-arglist-for-type-specifier (car type-specifier)))
    (symbol
     (with-available-arglist (arglist)
         (decode-arglist (type-specifier-arglist type-specifier))
       (setf (arglist.provided-args arglist) (list type-specifier))
       arglist))))

;;; Slimefuns

;;; We work on a RAW-FORM, or BUFFER-FORM, which represent the form at
;;; user's point in Emacs. A RAW-FORM looks like
;;;
;;;       ("FOO" ("BAR" ...) "QUUX" ("ZURP" SWANK::%CURSOR-MARKER%))
;;;
;;; The expression before the cursor marker is the expression where
;;; user's cursor points at. An explicit marker is necessary to
;;; disambiguate between
;;;
;;;       ("IF" ("PRED")
;;;             ("F" "X" "Y" %CURSOR-MARKER%))
;;;
;;; and
;;;       ("IF" ("PRED")
;;;             ("F" "X" "Y") %CURSOR-MARKER%)

;;; Notice that for a form like (FOO (BAR |) QUUX), where | denotes
;;; user's point, the following should be sent ("FOO" ("BAR" ""
;;; %CURSOR-MARKER%)). Only the forms up to point should be
;;; considered.

(defslimefun autodoc (raw-form &key print-right-margin)
  "Return a list of two elements.
First, a string representing the arglist for the deepest subform in
RAW-FORM that does have an arglist. The highlighted parameter is
wrapped in ===> X <===.

Second, a boolean value telling whether the returned string can be cached."
  (handler-bind ((serious-condition
                  #'(lambda (c)
                      (unless (debug-on-swank-error)
                        (let ((*print-right-margin* print-right-margin))
                          (return-from autodoc
                            (format nil "Arglist Error: \"~A\"" c)))))))
    (with-buffer-syntax ()
      (multiple-value-bind (form arglist obj-at-cursor form-path)
          (find-subform-with-arglist (parse-raw-form raw-form))
        (cond ((boundp-and-interesting obj-at-cursor)
               (list (print-variable-to-string obj-at-cursor) nil))
              (t
               (list
                (with-available-arglist (arglist) arglist
                  (decoded-arglist-to-string
                   arglist
                   :print-right-margin print-right-margin
                   :operator (car form)
                   :highlight (form-path-to-arglist-path form-path
                                                         form
                                                         arglist)))
                t)))))))

(defun boundp-and-interesting (symbol)
  (and symbol
       (symbolp symbol)
       (boundp symbol)
       (not (memq symbol '(cl:t cl:nil)))
       (not (keywordp symbol))))

(defun print-variable-to-string (symbol)
  "Return a short description of VARIABLE-NAME, or NIL."
  (let ((*print-pretty* t) (*print-level* 4)
        (*print-length* 10) (*print-lines* 1)
        (*print-readably* nil)
        (value (symbol-value symbol)))
    (call/truncated-output-to-string
     75 (lambda (s)
          (without-printing-errors (:object value :stream s)
            (format s "~A ~A~S" symbol *echo-area-prefix* value))))))


(defslimefun complete-form (raw-form)
  "Read FORM-STRING in the current buffer package, then complete it
  by adding a template for the missing arguments."
  ;; We do not catch errors here because COMPLETE-FORM is an
  ;; interactive command, not automatically run in the background like
  ;; ARGLIST-FOR-ECHO-AREA.
  (with-buffer-syntax ()
    (multiple-value-bind (arglist provided-args)
        (find-immediately-containing-arglist (parse-raw-form raw-form))
      (with-available-arglist (arglist) arglist
        (decoded-arglist-to-template-string
         (delete-given-args arglist
                            (remove-if #'empty-arg-p provided-args
                                       :from-end t :count 1))
         :prefix "" :suffix "")))))

(defslimefun completions-for-keyword (keyword-string raw-form)
  "Return a list of possible completions for KEYWORD-STRING relative
to the context provided by RAW-FORM."
  (with-buffer-syntax ()
    (let ((arglist (find-immediately-containing-arglist
                    (parse-raw-form raw-form))))
      (when (arglist-available-p arglist)
        ;; It would be possible to complete keywords only if we are in
        ;; a keyword position, but it is not clear if we want that.
        (let* ((keywords
                (append (mapcar #'keyword-arg.keyword
                                (arglist.keyword-args arglist))
                        (remove-if-not #'keywordp (arglist.any-args arglist))))
               (keyword-name
                (tokenize-symbol keyword-string))
               (matching-keywords
                (find-matching-symbols-in-list
                 keyword-name keywords (make-compound-prefix-matcher #\-)))
               (converter (completion-output-symbol-converter keyword-string))
               (strings
                (mapcar converter
                        (mapcar #'symbol-name matching-keywords)))
               (completion-set
                (format-completion-set strings nil "")))
          (list completion-set
                (longest-compound-prefix completion-set)))))))

(defparameter +cursor-marker+ '%cursor-marker%)

(defun find-subform-with-arglist (form)
  "Returns four values:

     The appropriate subform of `form' which is closest to the
     +CURSOR-MARKER+ and whose operator is valid and has an
     arglist. The +CURSOR-MARKER+ is removed from that subform.

     Second value is the arglist. Local function and macro definitions
     appearing in `form' into account.

     Third value is the object in front of +CURSOR-MARKER+.

     Fourth value is a form path to that object."
  (labels
      ((yield-success (form local-ops)
         (multiple-value-bind (form obj-at-cursor form-path)
             (extract-cursor-marker form)
           (values form
                   (let ((entry (assoc (car form) local-ops :test #'op=)))
                     (if entry
                         (decode-arglist (cdr entry))
                         (arglist-from-form form)))
                   obj-at-cursor
                   form-path)))
       (yield-failure ()
         (values nil :not-available))
       (operator-p (operator local-ops)
         (or (and (symbolp operator) (valid-operator-symbol-p operator))
             (assoc operator local-ops :test #'op=)))
       (op= (op1 op2)
         (cond ((and (symbolp op1) (symbolp op2))
                (eq op1 op2))
               ((and (arglist-dummy-p op1) (arglist-dummy-p op2))
                (string= (arglist-dummy.string-representation op1)
                         (arglist-dummy.string-representation op2)))))
       (grovel-form (form local-ops)
         "Descend FORM top-down, always taking the rightest branch,
          until +CURSOR-MARKER+."
         (assert (listp form))
         (destructuring-bind (operator . args) form
           ;; N.b. the user's cursor is at the rightmost, deepest
           ;; subform right before +CURSOR-MARKER+.
           (let ((last-subform (car (last form)))
                 (new-ops))
             (cond
               ((eq last-subform +cursor-marker+)
                (if (operator-p operator local-ops)
                    (yield-success form local-ops)
                    (yield-failure)))
               ((not (operator-p operator local-ops))
                (grovel-form last-subform local-ops))
               ;; Make sure to pick up the arglists of local
               ;; function/macro definitions.
               ((setq new-ops (extract-local-op-arglists operator args))
                (multiple-value-or (grovel-form last-subform
                                                (nconc new-ops local-ops))
                                   (yield-success form local-ops)))
               ;; Some typespecs clash with function names, so we make
               ;; sure to bail out early.
               ((member operator '(cl:declare cl:declaim))
                (yield-success form local-ops))
               ;; Mostly uninteresting, hence skip.
               ((memq operator '(cl:quote cl:function))
                (yield-failure))
               (t
                (multiple-value-or (grovel-form last-subform local-ops)
                                   (yield-success form local-ops))))))))
    (if (null form)
        (yield-failure)
        (grovel-form form '()))))

(defun extract-cursor-marker (form)
  "Returns three values: normalized `form' without +CURSOR-MARKER+,
the object in front of +CURSOR-MARKER+, and a form path to that
object."
  (labels ((grovel (form last path)
             (let ((result-form))
               (loop for (car . cdr) on form do
                     (cond ((eql car +cursor-marker+)
                            (decf (first path))
                            (return-from grovel
                              (values (nreconc result-form cdr)
                                      last
                                      (nreverse path))))
                           ((consp car)
                            (multiple-value-bind (new-car new-last new-path)
                                (grovel car last (cons 0 path))
                              (when new-path ; CAR contained cursor-marker?
                                (return-from grovel
                                  (values (nreconc
                                           (cons new-car result-form) cdr)
                                          new-last
                                          new-path))))))
                     (push car result-form)
                     (setq last car)
                     (incf (first path))
                     finally
                       (return-from grovel
                         (values (nreverse result-form) nil nil))))))
    (grovel form nil (list 0))))

(defgeneric extract-local-op-arglists (operator args)
  (:documentation
   "If the form `(OPERATOR ,@ARGS) is a local operator binding form,
     return a list of pairs (OP . ARGLIST) for each locally bound op.")
  (:method (operator args)
    (declare (ignore operator args))
    nil)
  ;; FLET
  (:method ((operator (eql 'cl:flet)) args)
    (let ((defs (first args))
          (body (rest args)))
      (cond ((null body) nil)            ; `(flet ((foo (x) |'
            ((atom defs) nil)            ; `(flet ,foo (|'
            (t (%collect-op/argl-alist defs)))))
  ;; LABELS
  (:method ((operator (eql 'cl:labels)) args)
    ;; Notice that we only have information to "look backward" and
    ;; show arglists of previously occuring local functions.
    (destructuring-bind (defs . body) args
      (unless (or (atom defs) (null body))   ; `(labels ,foo (|'
        (let ((current-def (car (last defs))))
          (cond ((atom current-def) nil) ; `(labels ((foo (x) ...)|'
                ((not (null body))
                 (extract-local-op-arglists 'cl:flet args))
                (t
                 (let ((def.body (cddr current-def)))
                   (when def.body
                     (%collect-op/argl-alist defs)))))))))
  ;; MACROLET
  (:method ((operator (eql 'cl:macrolet)) args)
    (extract-local-op-arglists 'cl:labels args)))

(defun %collect-op/argl-alist (defs)
  (setq defs (remove-if-not #'(lambda (x)
                                ;; Well-formed FLET/LABELS def?
                                (and (consp x) (second x)))
                            defs))
  (loop for (name arglist . nil) in defs
        collect (cons name arglist)))

(defun find-immediately-containing-arglist (form)
  "Returns the arglist of the subform _immediately_ containing
+CURSOR-MARKER+ in `form'. Notice, however, that +CURSOR-MARKER+ may
be in a nested arglist \(e.g. `(WITH-OPEN-FILE (<here>'\), and the
arglist of the appropriate parent form \(WITH-OPEN-FILE\) will be
returned in that case."
  (flet ((try (form-path form arglist)
           (let* ((arglist-path (form-path-to-arglist-path form-path
                                                           form
                                                           arglist))
                  (argl (apply #'arglist-ref
                               arglist
                               arglist-path))
                  (args (apply #'provided-arguments-ref
                               (cdr form)
                               arglist
                               arglist-path)))
             (when (and (arglist-p argl) (listp args))
               (values argl args)))))
    (multiple-value-bind (form arglist obj form-path)
        (find-subform-with-arglist form)
      (declare (ignore obj))
      (with-available-arglist (arglist) arglist
        ;; First try the form the cursor is in (in case of a normal
        ;; form), then try the surrounding form (in case of a nested
        ;; macro form).
        (multiple-value-or (try form-path form arglist)
                           (try (butlast form-path) form arglist)
                           :not-available)))))

(defun form-path-to-arglist-path (form-path form arglist)
  "Convert a form path to an arglist path consisting of arglist
indices."
  (labels ((convert (path args arglist)
             (if (null path)
                 nil
                 (let* ((idx      (car path))
                        (idx*     (arglist-index idx args arglist))
                        (arglist* (and idx* (arglist-ref arglist idx*)))
                        (args*    (and idx* (provided-arguments-ref args
                                                                    arglist
                                                                    idx*))))
                   ;; The FORM-PATH may be more detailed than ARGLIST;
                   ;; consider (defun foo (x y) ...), a form path may
                   ;; point into the function's lambda-list, but the
                   ;; arglist of DEFUN won't contain as much information.
                   ;; So we only recurse if possible.
                   (cond ((null idx*)
                          nil)
                         ((arglist-p arglist*)
                          (cons idx* (convert (cdr path) args* arglist*)))
                         (t
                          (list idx*)))))))
    (convert
     ;; FORM contains irrelevant operator. Adjust FORM-PATH.
     (cond ((null form-path) nil)
           ((equal form-path '(0)) nil)
           (t
            (destructuring-bind (car . cdr) form-path
              (cons (1- car) cdr))))
     (cdr form)
     arglist)))

(defun arglist-index (provided-argument-index provided-arguments arglist)
  "Return the arglist index into `arglist' for the parameter belonging
to the argument (NTH `provided-argument-index' `provided-arguments')."
  (let ((positional-args# (positional-args-number arglist))
        (arg-index provided-argument-index))
    (with-struct (arglist. key-p rest) arglist
      (cond
        ((< arg-index positional-args#) ; required + optional
         arg-index)
        ((and (not key-p) (not rest))   ; more provided than allowed
         nil)
        ((not key-p)                    ; rest + body
         (assert (arglist.rest arglist))
         positional-args#)
        (t                              ; key
         ;; Find last provided &key parameter
         (let* ((argument      (nth arg-index provided-arguments))
                (provided-keys (subseq provided-arguments positional-args#)))
           (loop for (key value) on provided-keys by #'cddr
                 when (eq value argument)
                 return (match key
                            (('quote symbol) symbol)
                            (_ key)))))))))

(defun arglist-ref (arglist &rest indices)
  "Returns the parameter in ARGLIST along the INDICIES path. Numbers
represent positional parameters (required, optional), keywords
represent key parameters."
  (flet ((ref-positional-arg (arglist index)
           (check-type index (integer 0 *))
           (with-struct (arglist. provided-args required-args
                                  optional-args rest)
               arglist
             (loop for args in (list provided-args required-args
                                     (mapcar #'optional-arg.arg-name
                                             optional-args))
                   for args# = (length args)
                   if (< index args#)
                     return (nth index args)
                   else
                     do (decf index args#)
                   finally (return (or rest nil)))))
         (ref-keyword-arg (arglist keyword)
           ;; keyword argument may be any symbol,
           ;; not only from the KEYWORD package.
           (let ((keyword (match keyword
                            (('quote symbol) symbol)
                            (_ keyword))))
             (do-decoded-arglist arglist
               (&key (kw arg) (when (eq kw keyword)
                                (return-from ref-keyword-arg arg)))))
           nil))
    (dolist (index indices)
      (assert (arglist-p arglist))
      (setq arglist (if (numberp index)
                        (ref-positional-arg arglist index)
                        (ref-keyword-arg arglist index))))
    arglist))

(defun provided-arguments-ref (provided-args arglist &rest indices)
  "Returns the argument in PROVIDED-ARGUMENT along the INDICES path
relative to ARGLIST."
  (check-type arglist arglist)
  (flet ((ref (provided-args arglist index)
           (if (numberp index)
               (nth index provided-args)
               (let ((provided-keys (subseq provided-args
                                            (positional-args-number arglist))))
                 (loop for (key value) on provided-keys
                       when (eq key index)
                         return value)))))
    (dolist (idx indices)
      (setq provided-args (ref provided-args arglist idx))
      (setq arglist (arglist-ref arglist idx)))
    provided-args))

(defun positional-args-number (arglist)
  (+ (length (arglist.provided-args arglist))
     (length (arglist.required-args arglist))
     (length (arglist.optional-args arglist))))

(defun parse-raw-form (raw-form)
  "Parse a RAW-FORM into a Lisp form. I.e. substitute strings by
symbols if already interned. For strings not already interned, use
ARGLIST-DUMMY."
  (unless (null raw-form)
    (loop for element in raw-form
          collect (etypecase element
                    (string (read-conversatively element))
                    (list   (parse-raw-form element))
                    (symbol (prog1 element
                              ;; Comes after list, so ELEMENT can't be NIL.
                              (assert (eq element +cursor-marker+))))))))

(defun read-conversatively (string)
  "Tries to find the symbol that's represented by STRING.

If it can't, this either means that STRING does not represent a
symbol, or that the symbol behind STRING would have to be freshly
interned. Because this function is supposed to be called from the
automatic arglist display stuff from Slime, interning freshly
symbols is a big no-no.

In such a case (that no symbol could be found), an object of type
ARGLIST-DUMMY is returned instead, which works as a placeholder
datum for subsequent logics to rely on."
  (let* ((string  (string-left-trim '(#\Space #\Tab #\Newline) string))
         (length  (length string))
	 (type    (cond ((zerop length) nil)
                        ((eql (aref string 0) #\')
                         :quoted-symbol)
                        ((search "#'" string :end2 (min length 2))
                         :sharpquoted-symbol)
                        ((char= (char string 0) (char string (1- length))
                                #\")
                         :string)
                        (t
                         :symbol))))
    (multiple-value-bind (symbol found?)
	(case type
          (:symbol             (parse-symbol string))
          (:quoted-symbol      (parse-symbol (subseq string 1)))
          (:sharpquoted-symbol (parse-symbol (subseq string 2)))
          (:string             (values string t))
          (t                   (values string nil)))
      (if found?
          (ecase type
            (:symbol             symbol)
            (:quoted-symbol      `(quote ,symbol))
            (:sharpquoted-symbol `(function ,symbol))
            (:string             (if (> length 1)
                                     (subseq string 1 (1- length))
                                     string)))
	  (make-arglist-dummy string)))))

(defun test-print-arglist ()
  (flet ((test (arglist &rest strings)
           (let* ((*package* (find-package :swank))
                  (actual (decoded-arglist-to-string
                           (decode-arglist arglist)
                           :print-right-margin 1000)))
             (unless (loop for string in strings
                           thereis (string= actual string))
               (warn "Test failed: ~S => ~S~%  Expected: ~A"
                     arglist actual
                     (if (cdr strings)
                         (format nil "One of: ~{~S~^, ~}" strings)
                         (format nil "~S" (first strings))))))))
    (test '(function cons) "(function cons)")
    (test '(quote cons) "(quote cons)")
    (test '(&key (function #'+))
          "(&key (function #'+))" "(&key (function (function +)))")
    (test '(&whole x y z) "(y z)")
    (test '(x &aux y z) "(x)")
    (test '(x &environment env y) "(x y)")
    (test '(&key ((function f))) "(&key ((function ..)))")
    (test
     '(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)
     "(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)")
    (test '(declare (optimize &any (speed 1) (safety 1)))
	  "(declare (optimize &any (speed 1) (safety 1)))")))

(defun test-arglist-ref ()
  (macrolet ((soft-assert (form)
               `(unless ,form
                  (warn "Assertion failed: ~S~%" ',form))))
    (let ((sample (decode-arglist '(x &key ((:k (y z)))))))
      (soft-assert (eq (arglist-ref sample 0)    'x))
      (soft-assert (eq (arglist-ref sample :k 0) 'y))
      (soft-assert (eq (arglist-ref sample :k 1) 'z))

      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample 0)
                       'a))
      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample :k 0)
                       'b))
      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample :k 1)
                       'c)))))

(test-print-arglist)
(test-arglist-ref)

(provide :swank-arglists)
