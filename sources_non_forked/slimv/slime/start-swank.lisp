;;; This file is intended to be loaded by an implementation to
;;; get a running swank server
;;; e.g. sbcl --load start-swank.lisp
;;;
;;; Default port is 4005

;;; For additional swank-side configurations see
;;; 6.2 section of the Slime user manual.
;;;
;;; Modified for Slimv:
;;; - don't close connection
;;; - pass swank port in environment variable

(load (merge-pathnames "swank-loader.lisp" *load-truename*))

(swank-loader:init
 :delete nil         ; delete any existing SWANK packages
 :reload nil         ; reload SWANK, even if the SWANK package already exists
 :load-contribs nil  ; load all contribs
 :from-emacs nil)    ; not started from emacs

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
    #+Allegro (sys:getenv name)
    #+CLISP (ext:getenv name)
    #+ECL (si:getenv name)
    #+SBCL (sb-unix::posix-getenv name)
    #+LISPWORKS (lispworks:environment-variable name)
    #+CCL (ccl::getenv name)
    default))

(swank:create-server :port (parse-integer (my-getenv "SWANK_PORT" "4005"))
                     ;; if non-nil the connection won't be closed
                     ;; after connecting
                     :dont-close t)
