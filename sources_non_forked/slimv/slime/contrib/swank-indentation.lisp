(in-package :swank)

(defvar *application-hints-tables* '()
  "A list of hash tables mapping symbols to indentation hints (lists 
of symbols and numbers as per cl-indent.el). Applications can add hash 
tables to the list to change the auto indentation slime sends to 
emacs.")

(defun has-application-indentation-hint-p (symbol)
  (let ((default (load-time-value (gensym))))
    (dolist (table *application-hints-tables*)
      (let ((indentation (gethash symbol table default)))
        (unless (eq default indentation)
          (return-from has-application-indentation-hint-p
            (values indentation t))))))
  (values nil nil))

(defun application-indentation-hint (symbol)
  (let ((indentation (has-application-indentation-hint-p symbol)))
    (labels ((walk (indentation-spec)
               (etypecase indentation-spec
                 (null nil)
                 (number indentation-spec)
                 (symbol (string-downcase indentation-spec))
                 (cons (cons (walk (car indentation-spec))
                             (walk (cdr indentation-spec)))))))
      (walk indentation))))

;;; override swank version of this function
(defun symbol-indentation (symbol)
  "Return a form describing the indentation of SYMBOL. 

The form is to be used as the `common-lisp-indent-function' property 
in Emacs."
  (cond
    ((has-application-indentation-hint-p symbol)
     (application-indentation-hint symbol))
    ((and (macro-function symbol)
             (not (known-to-emacs-p symbol)))
     (let ((arglist (arglist symbol)))
       (etypecase arglist
         ((member :not-available)
          nil)
         (list
          (macro-indentation arglist)))))
    (t nil)))

;;; More complex version.
(defun macro-indentation (arglist)
  (labels ((frob (list &optional base)
             (if (every (lambda (x)
                          (member x '(nil "&rest") :test #'equal))
                        list)
                 ;; If there was nothing interesting, don't return anything.
                 nil
                 ;; Otherwise substitute leading NIL's with 4 or 1.
                 (let ((ok t))
                   (substitute-if (if base
                                      4
                                      1)
                                  (lambda (x)
                                    (if (and ok (not x))
                                        t
                                        (setf ok nil)))
                                  list))))
           (walk (list level &optional firstp)
             (when (consp list)
               (let ((head (car list)))
                 (if (consp head)
                     (let ((indent (frob (walk head (+ level 1) t))))
                       (cons (list* "&whole" (if (zerop level)
                                                 4
                                                 1)
                                    indent) (walk (cdr list) level)))
                     (case head
                       ;; &BODY is &BODY, this is clear.
                       (&body
                        '("&body"))
                       ;; &KEY is tricksy. If it's at the base level, we want
                       ;; to indent them normally:
                       ;;
                       ;;  (foo bar quux
                       ;;       :quux t
                       ;;       :zot nil)
                       ;;
                       ;; If it's at a destructuring level, we want indent of 1:
                       ;;
                       ;;  (with-foo (var arg
                       ;;             :foo t
                       ;;             :quux nil)
                       ;;     ...)
                       (&key
                        (if (zerop level)
                            '("&rest" nil)
                            '("&rest" 1)))
                       ;; &REST is tricksy. If it's at the front of
                       ;; destructuring, we want to indent by 1, otherwise
                       ;; normally:
                       ;;
                       ;;  (foo (bar quux
                       ;;        zot)
                       ;;    ...)
                       ;;
                       ;; but
                       ;;
                       ;;  (foo bar quux
                       ;;       zot)
                       (&rest
                        (if (and (plusp level) firstp)
                            '("&rest" 1)
                            '("&rest" nil)))
                       ;; &WHOLE and &ENVIRONMENT are skipped as if they weren't there
                       ;; at all.
                       ((&whole &environment)
                        (walk (cddr list) level firstp))
                       ;; &OPTIONAL is indented normally -- and the &OPTIONAL marker
                       ;; itself is not counted.
                       (&optional
                        (walk (cdr list) level))
                       ;; Indent normally, walk the tail -- but
                       ;; unknown lambda-list keywords terminate the walk.
                       (otherwise
                        (unless (member head lambda-list-keywords)
                          (cons nil (walk (cdr list) level))))))))))
    (frob (walk arglist 0 t) t)))

#+nil
(progn
  (assert (equal '(4 4 ("&whole" 4 "&rest" 1) "&body")
                 (macro-indentation '(bar quux (&rest slots) &body body))))
  (assert (equal nil
                 (macro-indentation '(a b c &rest more))))
  (assert (equal '(4 4 4 "&body")
                 (macro-indentation '(a b c &body more))))
  (assert (equal '(("&whole" 4 1 1 "&rest" 1) "&body")
                 (macro-indentation '((name zot &key foo bar) &body body))))
  (assert (equal nil
                 (macro-indentation '(x y &key z)))))

(provide :swank-indentation)
