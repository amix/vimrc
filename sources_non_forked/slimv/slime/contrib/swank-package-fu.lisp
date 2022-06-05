
(in-package :swank)

(defslimefun package= (string1 string2)
  (let* ((pkg1 (guess-package string1))
	 (pkg2 (guess-package string2)))
    (and pkg1 pkg2 (eq pkg1 pkg2))))

(defslimefun export-symbol-for-emacs (symbol-str package-str)
  (let ((package (guess-package package-str)))
    (when package
      (let ((*buffer-package* package))
	(export `(,(from-string symbol-str)) package)))))

(defslimefun unexport-symbol-for-emacs (symbol-str package-str)
  (let ((package (guess-package package-str)))
    (when package
      (let ((*buffer-package* package))
	(unexport `(,(from-string symbol-str)) package)))))

#+sbcl
(defun list-structure-symbols (name)
  (let ((dd (sb-kernel:find-defstruct-description name )))
    (list* name
           (sb-kernel:dd-default-constructor dd)
           (sb-kernel:dd-predicate-name dd)
           (sb-kernel::dd-copier-name dd)
           (mapcar #'sb-kernel:dsd-accessor-name
                   (sb-kernel:dd-slots dd)))))

#+ccl
(defun list-structure-symbols (name)
  (let ((definition (gethash name ccl::%defstructs%)))
    (list* name
           (ccl::sd-constructor definition)
           (ccl::sd-refnames definition))))

(defun list-class-symbols (name)
  (let* ((class (find-class name))
         (slots (swank-mop:class-direct-slots class)))
    (labels ((extract-symbol (name)
               (if (and (consp name) (eql (car name) 'setf))
                   (cadr name)
                   name))
             (slot-accessors (slot)
               (nintersection (copy-list (swank-mop:slot-definition-readers slot))
                              (copy-list (swank-mop:slot-definition-readers slot))
                              :key #'extract-symbol)))
      (list* (class-name class)
             (mapcan #'slot-accessors slots)))))

(defslimefun export-structure (name package)
  (let ((*package* (guess-package package)))
    (when *package*
      (let* ((name (from-string name))
             (symbols (cond #+(or sbcl ccl)
			    ((or (not (find-class name nil))
                                 (subtypep name 'structure-object))
                             (list-structure-symbols name))
                            (t
                             (list-class-symbols name)))))
        (export symbols)
        symbols))))

(provide :swank-package-fu)
