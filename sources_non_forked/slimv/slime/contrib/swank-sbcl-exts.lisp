;;; swank-sbcl-exts.lisp --- Misc extensions for SBCL
;;
;; Authors: Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-arglists))

;; We need to do this so users can place `slime-sbcl-exts' into their
;; ~/.emacs, and still use any implementation they want.
#+sbcl
(progn
  
;;; Display arglist of instructions.
;;;
(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'sb-assem:inst))
                                             argument-forms)
  (flet ((decode-instruction-arglist (instr-name instr-arglist)
           (let ((decoded-arglist (decode-arglist instr-arglist)))
             ;; The arglist of INST is (instruction ...INSTR-ARGLIST...).
             (push 'sb-assem::instruction (arglist.required-args decoded-arglist))
             (values decoded-arglist
                     (list instr-name)
                     t))))
    (if (null argument-forms)
        (call-next-method)
        (destructuring-bind (instruction &rest args) argument-forms
          (declare (ignore args))
          (let* ((instr-name
                   (typecase instruction
                     (arglist-dummy
                      (string-upcase (arglist-dummy.string-representation instruction)))
                     (symbol
                      (string-downcase instruction))))
                 (instr-fn
                   #+#.(swank/backend:with-symbol 'op-encoder-name 'sb-assem)
                   (or (sb-assem::op-encoder-name instr-name)
                       (sb-assem::op-encoder-name (string-upcase instr-name)))
                   #+#.(swank/backend:with-symbol 'inst-emitter-symbol 'sb-assem)
                   (sb-assem::inst-emitter-symbol instr-name)
                   #+(and
                      (not #.(swank/backend:with-symbol 'inst-emitter-symbol 'sb-assem))
                      #.(swank/backend:with-symbol '*assem-instructions* 'sb-assem))
                   (gethash instr-name sb-assem:*assem-instructions*)))
            (cond ((functionp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     (decode-instruction-arglist instr-name arglist)))
                  ((fboundp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     ;; SB-ASSEM:INST invokes a symbolic INSTR-FN with
                     ;; current segment and current vop implicitly.
                     (decode-instruction-arglist instr-name
                                                 (if (or (get instr-fn :macro)
                                                         (macro-function instr-fn))
                                                     arglist
                                                     (cddr arglist)))))
                  (t
                   (call-next-method))))))))


) ; PROGN

(provide :swank-sbcl-exts)
