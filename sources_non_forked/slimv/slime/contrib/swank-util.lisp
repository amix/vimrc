;;; swank-util.lisp --- stuff of questionable utility
;;
;; License: public domain

(in-package :swank)

(defmacro do-symbols* ((var &optional (package '*package*) result-form)
                       &body body)
  "Just like do-symbols, but makes sure a symbol is visited only once."
  (let ((seen-ht (gensym "SEEN-HT")))
    `(let ((,seen-ht (make-hash-table :test #'eq)))
       (do-symbols (,var ,package ,result-form)
         (unless (gethash ,var ,seen-ht)
           (setf (gethash ,var ,seen-ht) t)
           (tagbody ,@body))))))

(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :CONSTANT, :GENERIC-FUNCTION,
:TYPESPEC, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (flet ((type-specifier-p (s)
           (or (documentation s 'type)
               (not (eq (type-specifier-arglist s) :not-available)))))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol)
                                                  :constant :boundp) result))
      (when (fboundp symbol)            (push :fboundp result))
      (when (type-specifier-p symbol)   (push :typespec result))
      (when (find-class symbol nil)     (push :class result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (and (fboundp symbol)
                 (typep (ignore-errors (fdefinition symbol))
                        'generic-function))
        (push :generic-function result))
      result)))

(defun symbol-classification-string (symbol)
  "Return a string in the form -f-c---- where each letter stands for
boundp fboundp generic-function class macro special-operator package"
  (let ((letters "bfgctmsp")
        (result (copy-seq "--------")))
    (flet ((flip (letter)
             (setf (char result (position letter letters))
                   letter)))
      (when (boundp symbol) (flip #\b))
      (when (fboundp symbol)
        (flip #\f)
        (when (typep (ignore-errors (fdefinition symbol))
                     'generic-function)
          (flip #\g)))
      (when (type-specifier-p symbol) (flip #\t))
      (when (find-class symbol nil)   (flip #\c) )
      (when (macro-function symbol)   (flip #\m))
      (when (special-operator-p symbol) (flip #\s))
      (when (find-package symbol)       (flip #\p))
      result)))

(provide :swank-util)
