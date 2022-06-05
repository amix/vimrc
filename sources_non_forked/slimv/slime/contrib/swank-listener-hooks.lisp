;;; swank-listener-hooks.lisp --- listener with special hooks
;;
;; Author: Alan Ruttenberg  <alanr-l@mumble.net>

;; Provides *slime-repl-eval-hooks* special variable which
;; can be used for easy interception of SLIME REPL form evaluation
;; for purposes such as integration with application event loop.

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-repl))

(defvar *slime-repl-advance-history* nil
  "In the dynamic scope of a single form typed at the repl, is set to nil to
   prevent the repl from advancing the history - * ** *** etc.")

(defvar *slime-repl-suppress-output* nil
  "In the dynamic scope of a single form typed at the repl, is set to nil to
   prevent the repl from printing the result of the evalation.")

(defvar *slime-repl-eval-hook-pass* (gensym "PASS")
  "Token to indicate that a repl hook declines to evaluate the form")

(defvar *slime-repl-eval-hooks* nil
  "A list of functions. When the repl is about to eval a form, first try running each of
   these hooks. The first hook which returns a value which is not *slime-repl-eval-hook-pass*
   is considered a replacement for calling eval. If there are no hooks, or all
   pass, then eval is used.")

(export '*slime-repl-eval-hooks*)

(defslimefun repl-eval-hook-pass ()
  "call when repl hook declines to evaluate the form"
  (throw *slime-repl-eval-hook-pass* *slime-repl-eval-hook-pass*))

(defslimefun repl-suppress-output ()
  "In the dynamic scope of a single form typed at the repl, call to
   prevent the repl from printing the result of the evalation."
  (setq *slime-repl-suppress-output* t))

(defslimefun repl-suppress-advance-history ()
  "In the dynamic scope of a single form typed at the repl, call to
   prevent the repl from advancing the history - * ** *** etc."
  (setq *slime-repl-advance-history* nil))

(defun %eval-region (string)
  (with-input-from-string (stream string)
    (let (- values)
      (loop
       (let ((form (read stream nil stream)))
	 (when (eq form stream)
	   (fresh-line)
	   (finish-output)
	   (return (values values -)))
	 (setq - form)
	 (if *slime-repl-eval-hooks*
	     (setq values (run-repl-eval-hooks form))
	     (setq values (multiple-value-list (eval form))))
	 (finish-output))))))

(defun run-repl-eval-hooks (form)
  (loop for hook in *slime-repl-eval-hooks*
	for res =  (catch *slime-repl-eval-hook-pass*
		     (multiple-value-list (funcall hook form)))
	until (not (eq res *slime-repl-eval-hook-pass*))
	finally (return
		  (if (eq res *slime-repl-eval-hook-pass*)
		      (multiple-value-list (eval form))
		      res))))

(defun %listener-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (swank-repl::track-package
     (lambda ()
       (let ((*slime-repl-suppress-output* :unset)
	     (*slime-repl-advance-history* :unset))
	 (multiple-value-bind (values last-form) (%eval-region string)
	   (unless (or (and (eq values nil) (eq last-form nil))
		       (eq *slime-repl-advance-history* nil))
	     (setq *** **  ** *  * (car values)
		   /// //  // /  / values))
	   (setq +++ ++  ++ +  + last-form)
	   (unless (eq *slime-repl-suppress-output* t)
	     (funcall swank-repl::*send-repl-results-function* values)))))))
  nil)

(setq swank-repl::*listener-eval-function* '%listener-eval)

(provide :swank-listener-hooks)
