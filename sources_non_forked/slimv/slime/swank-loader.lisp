;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-loader.lisp --- Compile and load the Slime backend.
;;;
;;; Created 2003, James Bielman <jamesjb@jamesjb.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;; If you want customize the source- or fasl-directory you can set
;; swank-loader:*source-directory* resp. swank-loader:*fasl-directory*
;; before loading this files.
;; E.g.:
;;
;;   (load ".../swank-loader.lisp")
;;   (setq swank-loader::*fasl-directory* "/tmp/fasl/")
;;   (swank-loader:init)

(cl:defpackage :swank-loader
  (:use :cl)
  (:export :init
           :dump-image
           :list-fasls
           :*source-directory*
           :*fasl-directory*
           :*started-from-emacs*))

(cl:in-package :swank-loader)

(defvar *started-from-emacs* nil)

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory where to look for the source.")

(defparameter *sysdep-files*
  #+cmu '((swank source-path-parser) (swank source-file-cache) (swank cmucl)
          (swank gray))
  #+scl '((swank source-path-parser) (swank source-file-cache) (swank scl)
          (swank gray))
  #+sbcl '((swank source-path-parser) (swank source-file-cache) (swank sbcl)
           (swank gray))
  #+clozure '(metering (swank ccl) (swank gray))
  #+lispworks '((swank lispworks) (swank gray))
  #+allegro '((swank allegro) (swank gray))
  #+clisp '(xref metering (swank clisp) (swank gray))
  #+armedbear '((swank abcl))
  #+cormanlisp '((swank corman) (swank gray))
  #+ecl '((swank ecl) (swank gray))
  #+clasp '(metering (swank clasp) (swank gray))
  #+mkcl '((swank mkcl) (swank gray))
  #+mezzano '((swank mezzano) (swank gray))
  )

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :clozure :cmu :clisp :ccl :corman :cormanlisp
    :armedbear :gcl :ecl :scl :mkcl :clasp :mezzano))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :hpux
    :unix :mezzano))

(defparameter *architecture-features*
  '(:powerpc :ppc :ppc64 :x86 :x86-64 :x86_64 :amd64 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa :arm :armv5l :armv6l :armv7l :arm64 :aarch64
    :pentium3 :pentium4
    :mips :mipsel
    :java-1.4 :java-1.5 :java-1.6 :java-1.7))

(defun q (s) (read-from-string s))

#+ecl
(defun ecl-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (when (find-symbol "LISP-IMPLEMENTATION-VCS-ID" :ext)
            (let ((vcs-id (funcall (q "ext:lisp-implementation-vcs-id"))))
              (when (>= (length vcs-id) 8)
                (subseq vcs-id 0 8))))))

#+clasp
(defun clasp-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (core:lisp-implementation-id)))

(defun lisp-version-string ()
  #+(or clozure cmu) (substitute-if #\_ (lambda (x) (find x " /"))
                                    (lisp-implementation-version))
  #+(or cormanlisp scl mkcl) (lisp-implementation-version)
  #+sbcl (format nil "~a~:[~;-no-threads~]"
                 (lisp-implementation-version)
                 #+sb-thread nil
                 #-sb-thread t)
  #+lispworks (lisp-implementation-version)
  #+allegro   (format nil "~@{~a~}"
                      excl::*common-lisp-version-number*
                      (if (string= 'lisp "LISP") "A" "M")     ; ANSI vs MoDeRn
                      (if (member :smp *features*) "s" "")
                      (if (member :64bit *features*) "-64bit" "")
                      (excl:ics-target-case
                       (:-ics "")
                       (:+ics "-ics")))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+ecl (ecl-version-string)
  #+clasp (clasp-version-string)
  #+mezzano (let ((s (lisp-implementation-version)))
              (subseq s 0 (position #\space s))))

(defun unique-dir-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
                 when (find f *features*) return it))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (apply #'warn fstring args)
                    "unknown"))))
    (let ((lisp (maybe-warn (first-of *implementation-features*)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun string-starts-with (string prefix)
  (string-equal string prefix :end1 (min (length string) (length prefix))))

(defun slime-version-string ()
  "Return a string identifying the SLIME version.
Return nil if nothing appropriate is available."
  (with-open-file (s (merge-pathnames "slime.el" *source-directory*)
                     :if-does-not-exist nil)
    (when s
      (loop with prefix = ";; Version: "
            for line = (read-line s nil :eof)
            until (eq line :eof)
            when (string-starts-with line prefix)
              return (subseq line (length prefix))))))

(defun default-fasl-dir ()
  (merge-pathnames
   (make-pathname
    :directory `(:relative ".slime" "fasl"
                 ,@(if (slime-version-string) (list (slime-version-string)))
                 ,(unique-dir-name)))
   (user-homedir-pathname)))

(defvar *fasl-directory* (default-fasl-dir)
  "The directory where fasl files should be placed.")

(defun binary-pathname (src-pathname binary-dir)
  "Return the pathname where SRC-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname src-pathname)))
    (merge-pathnames (make-pathname :name (pathname-name cfp)
                                    :type (pathname-type cfp))
                     binary-dir)))

(defun handle-swank-load-error (condition context pathname)
  (fresh-line *error-output*)
  (pprint-logical-block (*error-output* () :per-line-prefix ";; ")
    (format *error-output*
            "~%Error ~A ~A:~%  ~A~%"
            context pathname condition)))

(defun compile-files (files fasl-dir load quiet)
  "Compile each file in FILES if the source is newer than its
corresponding binary, or the file preceding it was recompiled.
If LOAD is true, load the fasl file."
  (let ((needs-recompile nil)
        (state :unknown))
    (dolist (src files)
      (let ((dest (binary-pathname src fasl-dir)))
        (handler-bind
            ((error (lambda (c)
                      (ecase state
                        (:compile (handle-swank-load-error c "compiling" src))
                        (:load    (handle-swank-load-error c "loading" dest))
                        (:unknown (handle-swank-load-error c "???ing" src))))))
          (when (or needs-recompile
                    (not (probe-file dest))
                    (file-newer-p src dest))
            (ensure-directories-exist dest)
            ;; need to recompile SRC, so we'll need to recompile
            ;; everything after this too.
            (setf needs-recompile t
                  state :compile)
            (or (compile-file src :output-file dest :print nil
                                  :verbose (not quiet))
                ;; An implementation may not necessarily signal a
                ;; condition itself when COMPILE-FILE fails (e.g. ECL)
                (error "COMPILE-FILE returned NIL.")))
          (when load
            (setf state :load)
            (load dest :verbose (not quiet))))))))

#+cormanlisp
(defun compile-files (files fasl-dir load quiet)
  "Corman Lisp has trouble with compiled files."
  (declare (ignore fasl-dir))
  (when load
    (dolist (file files)
      (load file :verbose (not quiet)
      (force-output)))))

(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (load (merge-pathnames (user-homedir-pathname)
                         (make-pathname :name ".swank" :type "lisp"))
        :if-does-not-exist nil))

(defun load-site-init-file (dir)
  (load (make-pathname :name "site-init" :type "lisp"
                       :defaults dir)
        :if-does-not-exist nil))

(defun src-files (names src-dir)
  (mapcar (lambda (name)
            (multiple-value-bind (dirs name)
                (etypecase name
                  (symbol (values '() name))
                  (cons (values (butlast name) (car (last name)))))
              (make-pathname
               :directory (append (or (pathname-directory src-dir)
                                      '(:relative))
                                  (mapcar #'string-downcase dirs))
               :name (string-downcase name)
               :type "lisp"
               :defaults src-dir)))
          names))

(defvar *swank-files*
  `(packages
    (swank backend) ,@*sysdep-files* (swank match) (swank rpc)
    swank))

(defvar *contribs*
  '(swank-util swank-repl
    swank-c-p-c swank-arglists swank-fuzzy
    swank-fancy-inspector
    swank-presentations swank-presentation-streams
    #+(or asdf2 asdf3 sbcl ecl) swank-asdf
    swank-package-fu
    swank-hyperdoc
    #+sbcl swank-sbcl-exts
    swank-mrepl
    swank-trace-dialog
    swank-macrostep
    swank-quicklisp)
  "List of names for contrib modules.")

(defun append-dir (absolute name)
  (merge-pathnames
   (make-pathname :directory `(:relative ,name) :defaults absolute)
   absolute))

(defun contrib-dir (base-dir)
  (append-dir base-dir "contrib"))

(defun load-swank (&key (src-dir *source-directory*)
                     (fasl-dir *fasl-directory*)
                        quiet)
  (with-compilation-unit ()
    (compile-files (src-files *swank-files* src-dir) fasl-dir t quiet))
  (funcall (q "swank::before-init")
           (slime-version-string)
           (list (contrib-dir fasl-dir)
                 (contrib-dir src-dir))))

(defun delete-stale-contrib-fasl-files (swank-files contrib-files fasl-dir)
  (let ((newest (reduce #'max (mapcar #'file-write-date swank-files))))
    (dolist (src contrib-files)
      (let ((fasl (binary-pathname src fasl-dir)))
        (when (and (probe-file fasl)
                   (<= (file-write-date fasl) newest))
          (delete-file fasl))))))

(defun compile-contribs (&key (src-dir (contrib-dir *source-directory*))
                           (fasl-dir (contrib-dir *fasl-directory*))
                           (swank-src-dir *source-directory*)
                           load quiet)
  (let* ((swank-src-files (src-files *swank-files* swank-src-dir))
         (contrib-src-files (src-files *contribs* src-dir)))
    (delete-stale-contrib-fasl-files swank-src-files contrib-src-files
                                     fasl-dir)
    (compile-files contrib-src-files fasl-dir load quiet)))

(defun loadup ()
  (load-swank)
  (compile-contribs :load t))

(defun setup ()
  (load-site-init-file *source-directory*)
  (load-user-init-file)
  (when (#-clisp probe-file
         #+clisp ext:probe-directory
         (contrib-dir *source-directory*))
    (eval `(pushnew 'compile-contribs ,(q "swank::*after-init-hook*"))))
  (funcall (q "swank::init")))

(defun list-swank-packages ()
  (remove-if-not (lambda (package)
                   (let ((name (package-name package)))
                     (and (string-not-equal name "swank-loader")
                          (string-starts-with name "swank"))))
                 (list-all-packages)))

(defun delete-packages (packages)
  (dolist (package packages)
    (flet ((handle-package-error (c)
             (let ((pkgs (set-difference (package-used-by-list package)
                                         packages)))
               (when pkgs
                 (warn "deleting ~a which is used by ~{~a~^, ~}."
                       package pkgs))
               (continue c))))
      (handler-bind ((package-error #'handle-package-error))
        (delete-package package)))))

(defun init (&key delete reload load-contribs (setup t)
                  (quiet (not *load-verbose*))
                  from-emacs)
  "Load SWANK and initialize some global variables.
If DELETE is true, delete any existing SWANK packages.
If RELOAD is true, reload SWANK, even if the SWANK package already exists.
If LOAD-CONTRIBS is true, load all contribs
If SETUP is true, load user init files and initialize some
global variabes in SWANK."
  (when from-emacs
    (setf *started-from-emacs* t))
  (when (and delete (find-package :swank))
    (delete-packages (list-swank-packages)))
  (cond ((or (not (find-package :swank)) reload)
         (load-swank :quiet quiet))
        (t
         (warn "Not reloading SWANK.  Package already exists.")))
  (when load-contribs
    (compile-contribs :load t :quiet quiet))
  (when setup
    (setup)))

(defun dump-image (filename)
  (init :setup nil)
  (funcall (q "swank/backend:save-image") filename))

(defun list-fasls (&key (include-contribs t) (compile t)
                        (quiet (not *compile-verbose*)))
  "List up SWANK's fasls along with their dependencies."
  (flet ((collect-fasls (files fasl-dir)
           (when compile
             (compile-files files fasl-dir nil quiet))
           (loop for src in files
                 when (probe-file (binary-pathname src fasl-dir))
                   collect it)))
    (append (collect-fasls (src-files *swank-files* *source-directory*)
                           *fasl-directory*)
            (when include-contribs
              (collect-fasls (src-files *contribs*
                                        (contrib-dir *source-directory*))
                             (contrib-dir *fasl-directory*))))))
