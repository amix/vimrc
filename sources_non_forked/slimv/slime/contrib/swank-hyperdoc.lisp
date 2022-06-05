(in-package :swank)

(defslimefun hyperdoc (string)
  (let ((hyperdoc-package (find-package :hyperdoc)))
    (when hyperdoc-package
      (multiple-value-bind (symbol foundp symbol-name package)
          (parse-symbol string *buffer-package*)
        (declare (ignore symbol))
        (when foundp
          (funcall (find-symbol (string :lookup) hyperdoc-package)
                   (package-name (if (member package (cons *buffer-package*
                                                           (package-use-list
                                                            *buffer-package*)))
                                     *buffer-package*
                                     package))
                   symbol-name))))))

(provide :swank-hyperdoc)
