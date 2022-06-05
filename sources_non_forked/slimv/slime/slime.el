;;; slime.el --- Superior Lisp Interaction Mode for Emacs -*-lexical-binding:t-*-

;; URL: https://github.com/slime/slime
;; Package-Requires: ((cl-lib "0.5") (macrostep "0.9"))
;; Keywords: languages, lisp, slime
;; Version: 2.26

;;;; License and Commentary

;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
;;     For a detailed list of contributors, see the manual.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

;;; Commentary:

;; SLIME is the ``Superior Lisp Interaction Mode for Emacs.''
;;
;; SLIME extends Emacs with support for interactive programming in
;; Common Lisp. The features are centered around slime-mode, an Emacs
;; minor-mode that complements the standard lisp-mode. While lisp-mode
;; supports editing Lisp source files, slime-mode adds support for
;; interacting with a running Common Lisp process for compilation,
;; debugging, documentation lookup, and so on.
;;
;; The slime-mode programming environment follows the example of
;; Emacs's native Emacs Lisp environment. We have also included good
;; ideas from similar systems (such as ILISP) and some new ideas of
;; our own.
;;
;; SLIME is constructed from two parts: a user-interface written in
;; Emacs Lisp, and a supporting server program written in Common
;; Lisp. The two sides are connected together with a socket and
;; communicate using an RPC-like protocol.
;;
;; The Lisp server is primarily written in portable Common Lisp. The
;; required implementation-specific functionality is specified by a
;; well-defined interface and implemented separately for each Lisp
;; implementation. This makes SLIME readily portable.

;;; Code:


;;;; Dependencies and setup
(eval-and-compile
  (require 'cl-lib nil t)
  ;; For emacs 23, look for bundled version
  (require 'cl-lib "lib/cl-lib"))

(eval-when-compile (require 'cl)) ; defsetf, lexical-let

(eval-and-compile
  (if (< emacs-major-version 23)
      (error "Slime requires an Emacs version of 23, or above")))

(require 'hyperspec "lib/hyperspec")
(require 'thingatpt)
(require 'comint)
(require 'pp)
(require 'easymenu)
(require 'outline)
(require 'arc-mode)
(require 'etags)
(require 'compile)

(eval-when-compile
  (require 'apropos)
  (require 'gud)
  (require 'lisp-mnt))

(declare-function lm-version "lisp-mnt")

(defvar slime-path nil
  "Directory containing the Slime package.
This is used to load the supporting Common Lisp library, Swank.
The default value is automatically computed from the location of
the Emacs Lisp package.")
(setq slime-path (file-name-directory load-file-name))

(defvar slime-version nil
  "The version of SLIME that you're using.")
(setq slime-version
      (eval-when-compile
       (lm-version
        (cl-find "slime.el"
                 (remove nil
                         (list load-file-name
                               (when (boundp 'byte-compile-current-file)
                                 byte-compile-current-file)))
                 :key #'file-name-nondirectory
                 :test #'string-equal))))

(defvar slime-lisp-modes '(lisp-mode))
(defvar slime-contribs '(slime-fancy)
  "A list of contrib packages to load with SLIME.")
(define-obsolete-variable-alias 'slime-setup-contribs
'slime-contribs "2.3.2")

(cl-defun slime-setup (&optional (contribs nil contribs-p))
  "Setup Emacs so that lisp-mode buffers always use SLIME.
CONTRIBS is a list of contrib packages to load. If `nil', use
`slime-contribs'. "
  (interactive)
  (when (member 'lisp-mode slime-lisp-modes)
    (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))
  (when contribs-p
    (setq slime-contribs contribs))
  (slime--setup-contribs))

(defvar slime-required-modules '())

(defun slime--setup-contribs ()
  "Load and initialize contribs."
  (dolist (c slime-contribs)
    (unless (featurep c)
      (require c)
      (let ((init (intern (format "%s-init" c))))
        (when (fboundp init)
          (funcall init))))))

(defun slime-lisp-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))

(defvar slime-protocol-version nil)
(setq slime-protocol-version slime-version)


;;;; Customize groups
;;
;;;;; slime

(defgroup slime nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'applications)

;;;;; slime-ui

(defgroup slime-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
This applies to buffers that present lines as rows of data, such as
debugger backtraces and apropos listings."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-kill-without-query-p nil
  "If non-nil, kill SLIME processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'slime-ui)

;;;;; slime-lisp

(defgroup slime-lisp nil
  "Lisp server configuration."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-backend "swank-loader.lisp"
  "The name of the Lisp file that loads the Swank server.
This name is interpreted relative to the directory containing
slime.el, but could also be set to an absolute filename."
  :type 'string
  :group 'slime-lisp)

(defcustom slime-connected-hook nil
  "List of functions to call when SLIME connects to Lisp."
  :type 'hook
  :group 'slime-lisp)

(defcustom slime-enable-evaluate-in-emacs nil
  "*If non-nil, the inferior Lisp can evaluate arbitrary forms in Emacs.
The default is nil, as this feature can be a security risk."
  :type '(boolean)
  :group 'slime-lisp)

(defcustom slime-lisp-host "localhost"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'slime-lisp)

(defcustom slime-port 4005
  "Port to use as the default for `slime-connect'."
  :type 'integer
  :group 'slime-lisp)

(defvar slime-connect-host-history (list slime-lisp-host))
(defvar slime-connect-port-history (list (prin1-to-string slime-port)))

(defvar slime-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    (utf-8-unix       t   "utf-8-unix")
    (emacs-mule-unix  t   "emacs-mule-unix")
    (euc-jp-unix      t   "euc-jp-unix"))
  "A list of valid coding systems.
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun slime-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `slime-net-valid-coding-systems'
of nil."
  (let ((probe (assq name slime-net-valid-coding-systems)))
    (when (and probe (if (fboundp 'check-coding-system)
                         (ignore-errors (check-coding-system (car probe)))
                       (eq (car probe) 'binary)))
      probe)))

(defcustom slime-net-coding-system
  (car (cl-find-if 'slime-find-coding-system
                   slime-net-valid-coding-systems :key 'car))
  "Coding system used for network connections.
See also `slime-net-valid-coding-systems'."
  :type (cons 'choice
              (mapcar (lambda (x)
                        (list 'const (car x)))
                      slime-net-valid-coding-systems))
  :group 'slime-lisp)

;;;;; slime-mode

(defgroup slime-mode nil
  "Settings for slime-mode Lisp source buffers."
  :prefix "slime-"
  :group 'slime)

(defcustom slime-find-definitions-function 'slime-find-definitions-rpc
  "Function to find definitions for a name.
The function is called with the definition name, a string, as its
argument."
  :type 'function
  :group 'slime-mode
  :options '(slime-find-definitions-rpc
             slime-etags-definitions
             (lambda (name)
               (append (slime-find-definitions-rpc name)
                       (slime-etags-definitions name)))
             (lambda (name)
               (or (slime-find-definitions-rpc name)
                   (and tags-table-list
                        (slime-etags-definitions name))))))

;; FIXME: remove one day
(defcustom slime-complete-symbol-function 'nil
  "Obsolete. Use `slime-completion-at-point-functions' instead."
  :group 'slime-mode
  :type '(choice (const :tag "Compound" slime-complete-symbol*)
                 (const :tag "Fuzzy" slime-fuzzy-complete-symbol)))

(make-obsolete-variable 'slime-complete-symbol-function
                        'slime-completion-at-point-functions
                        "2015-10-18")

(defcustom slime-completion-at-point-functions
  '(slime-filename-completion
    slime-simple-completion-at-point)
  "List of functions to perform completion.
Works like `completion-at-point-functions'.
`slime--completion-at-point' uses this variable."
  :group 'slime-mode)

;;;;; slime-mode-faces

(defgroup slime-mode-faces nil
  "Faces in slime-mode source code buffers."
  :prefix "slime-"
  :group 'slime-mode)

(defface slime-error-face
  `((((class color) (background light))
     (:underline "red"))
    (((class color) (background dark))
     (:underline "red"))
    (t (:underline t)))
  "Face for errors from the compiler."
  :group 'slime-mode-faces)

(defface slime-warning-face
  `((((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t (:underline t)))
  "Face for warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-style-warning-face
  `((((class color) (background light))
     (:underline "brown"))
    (((class color) (background dark))
     (:underline "gold"))
    (t (:underline t)))
  "Face for style-warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-note-face
  `((((class color) (background light))
     (:underline "brown4"))
    (((class color) (background dark))
     (:underline "light goldenrod"))
    (t (:underline t)))
  "Face for notes from the compiler."
  :group 'slime-mode-faces)

(defface slime-early-deprecation-warning-face
  `((((type graphic) (class color) (background light))
     (:strike-through "brown"))
    (((type graphic) (class color) (background dark))
     (:strike-through "gold"))
    (((type graphic))
     (:strike-through t))
    (((class color) (background light))
     (:underline "brown"))
    (((class color) (background dark))
     (:underline "gold"))
    (t
     (:underline t)))
  "Face for early deprecation warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-late-deprecation-warning-face
  `((((type graphic) (class color) (background light))
     (:strike-through "orange"))
    (((type graphic) (class color) (background dark))
     (:strike-through "coral"))
    (((type graphic))
     (:strike-through t))
    (((class color) (background light))
     (:underline "orange"))
    (((class color) (background dark))
     (:underline "coral"))
    (t
     (:underline t)))
  "Face for late deprecation warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-final-deprecation-warning-face
  `((((type graphic) (class color) (background light))
     (:strike-through "red"))
    (((type graphic) (class color) (background dark))
     (:strike-through "red"))
    (((type graphic))
     (:strike-through t))
    (((class color) (background light))
     (:underline "red"))
    (((class color) (background dark))
     (:underline "red"))
    (t
     (:strike-through t)))
  "Face for final deprecation warnings from the compiler."
  :group 'slime-mode-faces)

(defface slime-highlight-face
    '((t (:inherit highlight :underline nil)))
  "Face for compiler notes while selected."
  :group 'slime-mode-faces)

;;;;; sldb

(defgroup slime-debugger nil
  "Backtrace options and fontification."
  :prefix "sldb-"
  :group 'slime)

(defmacro define-sldb-faces (&rest faces)
  "Define the set of SLDB faces.
Each face specifiation is (NAME DESCRIPTION &optional PROPERTIES).
NAME is a symbol; the face will be called sldb-NAME-face.
DESCRIPTION is a one-liner for the customization buffer.
PROPERTIES specifies any default face properties."
  `(progn ,@(cl-loop for face in faces
                     collect `(define-sldb-face ,@face))))

(defmacro define-sldb-face (name description &optional default)
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name)))))
    `(defface ,facename
       (list (list t ,default))
       ,(format "Face for %s." description)
       :group 'slime-debugger)))

(define-sldb-faces
  (topline        "the top line describing the error")
  (condition      "the condition class"
                  '(:inherit font-lock-warning-face))
  (section        "the labels of major sections in the debugger buffer"
                  '(:inherit header-line))
  (frame-label    "backtrace frame numbers"
                  '(:inherit shadow))
  (restart-type   "restart names."
                  '(:inherit font-lock-keyword-face))
  (restart        "restart descriptions")
  (restart-number "restart numbers (correspond to keystrokes to invoke)"
                  '(:bold t))
  (frame-line     "function names and arguments in the backtrace")
  (restartable-frame-line
   "frames which are surely restartable"
   '(:foreground "lime green"))
  (non-restartable-frame-line
   "frames which are surely not restartable")
  (detailed-frame-line
   "function names and arguments in a detailed (expanded) frame")
  (local-name     "local variable names"
                  '(:inherit font-lock-variable-name-face))
  (local-value    "local variable values")
  (catch-tag      "catch tags"
                  '(:inherit highlight)))


;;;; Minor modes

;;;;; slime-mode

(defvar slime-mode-indirect-map (make-sparse-keymap)
  "Empty keymap which has `slime-mode-map' as it's parent.
This is a hack so that we can reinitilize the real slime-mode-map
more easily. See `slime-init-keymaps'.")

(defvar slime-buffer-connection)
(defvar slime-current-thread)

(defun slime--on ()
  (slime-setup-completion))

(defun slime--off ()
  (remove-hook 'completion-at-point-functions #'slime--completion-at-point t))

(define-minor-mode slime-mode
  "\\<slime-mode-map>\
SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).

Commands to compile the current buffer's source file and visually
highlight any resulting compiler notes and warnings:
\\[slime-compile-and-load-file]	- Compile and load the current buffer's file.
\\[slime-compile-file]	- Compile (but not load) the current buffer's file.
\\[slime-compile-defun]	- Compile the top-level form at point.

Commands for visiting compiler notes:
\\[slime-next-note]	- Goto the next form with a compiler note.
\\[slime-previous-note]	- Goto the previous form with a compiler note.
\\[slime-remove-notes]	- Remove compiler-note annotations in buffer.

Finding definitions:
\\[slime-edit-definition]
- Edit the definition of the function called at point.
\\[slime-pop-find-definition-stack]
- Pop the definition stack to go back from a definition.

Documentation commands:
\\[slime-describe-symbol]	- Describe symbol.
\\[slime-apropos]	- Apropos search.
\\[slime-disassemble-symbol]	- Disassemble a function.

Evaluation commands:
\\[slime-eval-defun]	- Evaluate top-level from containing point.
\\[slime-eval-last-expression]	- Evaluate sexp before point.
\\[slime-pprint-eval-last-expression]	\
- Evaluate sexp before point, pretty-print result.

Full set of commands:
\\{slime-mode-map}"
  :keymap slime-mode-indirect-map
  :lighter (:eval (slime-modeline-string))
  (cond (slime-mode (slime--on))
        (t (slime--off))))


;;;;;; Modeline

(defun slime-modeline-string ()
  "Return the string to display in the modeline.
\"Slime\" only appears if we aren't connected.  If connected,
include package-name, connection-name, and possibly some state
information."
  (let ((conn (slime-current-connection)))
    ;; Bail out early in case there's no connection, so we won't
    ;; implicitly invoke `slime-connection' which may query the user.
    (if (not conn)
        (and slime-mode " Slime")
      (let ((local (eq conn slime-buffer-connection))
            (pkg   (slime-current-package)))
        (concat " "
                (if local "{" "[")
                (if pkg (slime-pretty-package-name pkg) "?")
                " "
                ;; ignore errors for closed connections
                (ignore-errors (slime-connection-name conn))
                (slime-modeline-state-string conn)
                (if local "}" "]"))))))

(defun slime-pretty-package-name (name)
  "Return a pretty version of a package name NAME."
  (cond ((string-match "^#?:\\(.*\\)$" name)
         (match-string 1 name))
        ((string-match "^\"\\(.*\\)\"$" name)
         (match-string 1 name))
        (t name)))

(defun slime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
         (format " %s" (process-status conn)))
        ((let ((pending (length (slime-rex-continuations conn)))
               (sldbs (length (sldb-buffers conn))))
           (cond ((and (zerop sldbs) (zerop pending)) nil)
                 ((zerop sldbs) (format " %s" pending))
                 (t (format " %s/%s" pending sldbs)))))))

(defun slime--recompute-modelines ()
  (force-mode-line-update t))


;;;;; Key bindings

(defvar slime-parent-map nil
  "Parent keymap for shared between all Slime related modes.")

(defvar slime-parent-bindings
  '(("\M-."      slime-edit-definition)
    ("\M-,"      slime-pop-find-definition-stack)
    ("\M-_"      slime-edit-uses)    ; for German layout
    ("\M-?"      slime-edit-uses)    ; for USian layout
    ("\C-x4."    slime-edit-definition-other-window)
    ("\C-x5."    slime-edit-definition-other-frame)
    ("\C-x\C-e"  slime-eval-last-expression)
    ("\C-\M-x"   slime-eval-defun)
    ;; Include PREFIX keys...
    ("\C-c"	 slime-prefix-map)))

(defvar slime-prefix-map nil
  "Keymap for commands prefixed with `slime-prefix-key'.")

(defvar slime-prefix-bindings
  '(("\C-r"  slime-eval-region)
    (":"     slime-interactive-eval)
    ("\C-e"  slime-interactive-eval)
    ("E"     slime-edit-value)
    ("\C-l"  slime-load-file)
    ("\C-b"  slime-interrupt)
    ("\M-d"  slime-disassemble-symbol)
    ("\C-t"  slime-toggle-trace-fdefinition)
    ("I"     slime-inspect)
    ("\C-xt" slime-list-threads)
    ("\C-xn" slime-next-connection)
    ("\C-xp" slime-prev-connection)
    ("\C-xc" slime-list-connections)
    ("<"     slime-list-callers)
    (">"     slime-list-callees)
    ;; Include DOC keys...
    ("\C-d"  slime-doc-map)
    ;; Include XREF WHO-FOO keys...
    ("\C-w"  slime-who-map)
    ))

(defvar slime-editing-map nil
  "These keys are useful for buffers where the user can insert and
edit s-exprs, e.g. for source buffers and the REPL.")

(defvar slime-editing-keys
  `(;; Arglist display & completion
    (" "          slime-space)
    ;; Evaluating
    ;;("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
    ("\C-c\C-p"   slime-pprint-eval-last-expression)
    ;; Macroexpand
    ("\C-c\C-m"   slime-expand-1)
    ("\C-c\M-m"   slime-macroexpand-all)
    ;; Misc
    ("\C-c\C-u"   slime-undefine-function)
    (,(kbd "C-M-.")   slime-next-location)
    (,(kbd "C-M-,")   slime-previous-location)
    ;; Obsolete, redundant bindings
    ("\C-c\C-i" completion-at-point)
    ;;("\M-*" pop-tag-mark) ; almost to clever
    ))

(defvar slime-mode-map nil
  "Keymap for slime-mode.")

(defvar slime-keys
  '( ;; Compiler notes
    ("\M-p"       slime-previous-note)
    ("\M-n"       slime-next-note)
    ("\C-c\M-c"   slime-remove-notes)
    ("\C-c\C-k"   slime-compile-and-load-file)
    ("\C-c\M-k"   slime-compile-file)
    ("\C-c\C-c"   slime-compile-defun)))

(defun slime-nop ()
  "The null command. Used to shadow currently-unused keybindings."
  (interactive)
  (call-interactively 'undefined))

(defvar slime-doc-map nil
  "Keymap for documentation commands. Bound to a prefix key.")

(defvar slime-doc-bindings
  '((?a slime-apropos)
    (?z slime-apropos-all)
    (?p slime-apropos-package)
    (?d slime-describe-symbol)
    (?f slime-describe-function)
    (?h slime-documentation-lookup)
    (?~ common-lisp-hyperspec-format)
    (?g common-lisp-hyperspec-glossary-term)
    (?# common-lisp-hyperspec-lookup-reader-macro)))

(defvar slime-who-map nil
  "Keymap for who-xref commands. Bound to a prefix key.")

(defvar slime-who-bindings
  '((?c slime-who-calls)
    (?w slime-calls-who)
    (?r slime-who-references)
    (?b slime-who-binds)
    (?s slime-who-sets)
    (?m slime-who-macroexpands)
    (?a slime-who-specializes)))

(defun slime-init-keymaps ()
  "(Re)initialize the keymaps for `slime-mode'."
  (interactive)
  (slime-init-keymap 'slime-doc-map t t slime-doc-bindings)
  (slime-init-keymap 'slime-who-map t t slime-who-bindings)
  (slime-init-keymap 'slime-prefix-map t nil slime-prefix-bindings)
  (slime-init-keymap 'slime-parent-map nil nil slime-parent-bindings)
  (slime-init-keymap 'slime-editing-map nil nil slime-editing-keys)
  (set-keymap-parent slime-editing-map slime-parent-map)
  (slime-init-keymap 'slime-mode-map nil nil slime-keys)
  (set-keymap-parent slime-mode-map slime-editing-map)
  (set-keymap-parent slime-mode-indirect-map slime-mode-map))

(defun slime-init-keymap (keymap-name prefixp bothp bindings)
  (set keymap-name (make-sparse-keymap))
  (when prefixp (define-prefix-command keymap-name))
  (slime-bind-keys (eval keymap-name) bothp bindings))

(defun slime-bind-keys (keymap bothp bindings)
  "Add BINDINGS to KEYMAP.
If BOTHP is true also add bindings with control modifier."
  (cl-loop for (key command) in bindings do
           (cond (bothp
                  (define-key keymap `[,key] command)
                  (unless (equal key ?h)     ; But don't bind C-h
                    (define-key keymap `[(control ,key)] command)))
                 (t (define-key keymap key command)))))

(slime-init-keymaps)

(define-minor-mode slime-editing-mode
  "Minor mode which makes slime-editing-map available.
\\{slime-editing-map}"
  nil
  nil
  slime-editing-map)


;;;; Framework'ey bits
;;;
;;; This section contains some standard SLIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar

(defmacro slime-dcase (value &rest patterns)
  (declare (indent 1))
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (cl-gensym "op-"))
	(operands (cl-gensym "rand-"))
	(tmp (cl-gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (cl-case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (cl-destructuring-bind ((op &rest rands) &rest body)
                           clause
                         `(,op (cl-destructuring-bind ,rands ,operands
                                 . ,(or body
                                        '((ignore)) ; suppress some warnings
                                        ))))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "slime-dcase failed: %S" ,tmp))))))))

(defmacro slime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  (declare (indent 1))
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))

(cl-defmacro with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (declare (indent 2))
  (let ((struct-var (cl-gensym "struct"))
        (reader (lambda (slot)
                  (intern (concat (symbol-name conc-name)
                                  (symbol-name slot))))))
    `(let ((,struct-var ,struct))
       (cl-symbol-macrolet
           ,(mapcar (lambda (slot)
                      (cl-etypecase slot
                        (symbol `(,slot (,(funcall reader slot) ,struct-var)))
                        (cons `(,(cl-first slot)
                                (,(funcall reader (cl-second slot))
                                 ,struct-var)))))
                    slots)
         . ,body))))

;;;;; Very-commonly-used functions

(defvar slime-message-function 'message)

;; Interface
(defun slime-buffer-name (type &optional hidden)
  (cl-assert (keywordp type))
  (concat (if hidden " " "")
          (format "*slime-%s*" (substring (symbol-name type) 1))))

;; Interface
(defun slime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply slime-message-function format args))

(defun slime-display-warning (message &rest args)
  (display-warning '(slime warning) (apply #'format message args)))

(defvar slime-background-message-function 'slime-display-oneliner)

;; Interface
(defun slime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `slime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply slime-background-message-function format-string format-args))

(defun slime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (slime-oneliner msg)))))

(defun slime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (window-width (minibuffer-window))))))

;; Interface
(defun slime-set-truncate-lines ()
  "Apply `slime-truncate-lines' to the current buffer."
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

;; Interface
(defun slime-read-package-name (prompt &optional initial-value)
  "Read a package name from the minibuffer, prompting with PROMPT."
  (let ((completion-ignore-case t))
    (completing-read prompt (slime-bogus-completion-alist
                             (slime-eval
                              `(swank:list-all-package-names t)))
		     nil t initial-value)))

;; Interface
(defun slime-read-symbol-name (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (slime-symbol-at-point)))
         (slime-read-from-minibuffer prompt (slime-symbol-at-point)))
        (t (slime-symbol-at-point))))

;; Interface
(defmacro slime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun slime-add-face (face string)
  (declare (indent 1))
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

;; Interface
(defsubst slime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (slime-propertize-region props (apply #'insert args)))

(defmacro slime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (declare (indent 1))
  (let ((start (cl-gensym)) (l (cl-gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
         (slime-indent-rigidly ,start (point) ,l)))))

(defun slime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
                  (progn
                    (insert-before-markers indent)
                    (zerop (forward-line -1))))))))

(defun slime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (slime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun slime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (cl-assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun slime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun slime-rcurry (fun &rest args)
  "Like `slime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Temporary popup buffers

;; keep compiler quiet
(defvar slime-buffer-package)
(defvar slime-buffer-connection)

;; Interface
(cl-defmacro slime-with-popup-buffer ((name &key package connection select
                                            mode)
                                      &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PACKAGE is the value `slime-buffer-package'.
CONNECTION is the value for `slime-buffer-connection',
 if nil, no explicit connection is associated with
 the buffer.  If t, the current connection is taken.
MODE is the name of a major mode which will be enabled.
"
  (declare (indent 1))
  (let ((package-sym (cl-gensym "package-"))
        (connection-sym (cl-gensym "connection-")))
    `(let ((,package-sym ,(if (eq package t)
                              `(slime-current-package)
                            package))
           (,connection-sym ,(if (eq connection t)
                                 `(slime-current-connection)
                               connection)))
       (with-current-buffer (get-buffer-create ,name)
         (let ((inhibit-read-only t)
               (standard-output (current-buffer)))
           (erase-buffer)
           (funcall (or ,mode 'fundamental-mode))
           (setq slime-buffer-package ,package-sym
                 slime-buffer-connection ,connection-sym)
           (set-syntax-table lisp-mode-syntax-table)
           ,@body
           (slime-popup-buffer-mode 1)
           (funcall (if ,select 'pop-to-buffer 'display-buffer)
                    (current-buffer))
           (current-buffer))))))

(defvar slime-popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    ;;("\C-c\C-z" . slime-switch-to-output-buffer)
    (define-key map (kbd "M-.") 'slime-edit-definition)
    map))

(define-minor-mode slime-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil nil nil
  (setq buffer-read-only t))

(add-to-list 'minor-mode-alist
             `(slime-popup-buffer-mode
               (:eval (unless slime-mode
                        (slime-modeline-string)))))

(set-keymap-parent slime-popup-buffer-mode-map slime-parent-map)

;;;;; Filename translation
;;;
;;; Filenames passed between Emacs and Lisp should be translated using
;;; these functions. This way users who run Emacs and Lisp on separate
;;; machines have a chance to integrate file operations somehow.

(defvar slime-to-lisp-filename-function #'convert-standard-filename
  "Function to translate Emacs filenames to CL namestrings.")
(defvar slime-from-lisp-filename-function #'identity
  "Function to translate CL namestrings to Emacs filenames.")

(defun slime-to-lisp-filename (filename)
  "Translate the string FILENAME to a Lisp filename."
  (funcall slime-to-lisp-filename-function filename))

(defun slime-from-lisp-filename (filename)
  "Translate the Lisp filename FILENAME to an Emacs filename."
  (funcall slime-from-lisp-filename-function filename))


;;;; Starting SLIME
;;;
;;; This section covers starting an inferior-lisp, compiling and
;;; starting the server, initiating a network connection.

;;;;; Entry points

;; We no longer load inf-lisp, but we use this variable for backward
;; compatibility.
(defvar inferior-lisp-program "lisp"
  "*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar slime-lisp-implementations nil
  "*A list of known Lisp implementations.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.
PROGRAM and PROGRAM-ARGS are strings used to start the Lisp process.
For KEYWORD-ARGS see `slime-start'.

Here's an example:
 ((cmucl (\"/opt/cmucl/bin/lisp\" \"-quiet\") :init slime-init-command)
  (acl (\"acl7\") :coding-system emacs-mule))")

(defvar slime-default-lisp nil
  "*The name of the default Lisp implementation.
See `slime-lisp-implementations'")

;; dummy definitions for the compiler
(defvar slime-net-processes)
(defvar slime-default-connection)

(defun slime (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (slime-setup)
  (let ((inferior-lisp-program (or command inferior-lisp-program))
        (slime-net-coding-system (or coding-system slime-net-coding-system)))
    (slime-start* (cond ((and command (symbolp command))
                         (slime-lisp-options command))
                        (t (slime-read-interactive-args))))))

(defvar slime-inferior-lisp-program-history '()
  "History list of command strings.  Used by `slime'.")

(defun slime-read-interactive-args ()
  "Return the list of args which should be passed to `slime-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `slime-lisp-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slime-lisp-implementations' is non-nil use the
  implementation with name `slime-default-lisp' or if that's nil
  the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  lisps.

- If the prefix-arg is positive, read the command to start the
  process."
  (let ((table slime-lisp-implementations))
    (cond ((not current-prefix-arg) (slime-lisp-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read
                       "Lisp name: " (mapcar (lambda (x)
                                               (list (symbol-name (car x))))
                                             table)
                       nil t)))
             (slime-lookup-lisp-implementation table (intern key))))
          (t
           (cl-destructuring-bind (program &rest program-args)
               (split-string-and-unquote
                (read-shell-command "Run lisp: " inferior-lisp-program
                                    'slime-inferior-lisp-program-history))
             (let ((coding-system
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set slime-coding-system: "
                                            slime-net-coding-system)
                      slime-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

(defun slime-lisp-options (&optional name)
  (let ((table slime-lisp-implementations))
    (cl-assert (or (not name) table))
    (cond (table (slime-lookup-lisp-implementation slime-lisp-implementations
                                                   (or name slime-default-lisp
                                                       (car (car table)))))
          (t (cl-destructuring-bind (program &rest args)
                 (split-string inferior-lisp-program)
               (list :program program :program-args args))))))

(defun slime-lookup-lisp-implementation (table name)
  (let ((arguments (cl-rest (assoc name table))))
    (unless arguments
      (error "Could not find lisp implementation with the name '%S'" name))
    (when (and (= (length arguments) 1)
               (functionp (cl-first arguments)))
      (setf arguments (funcall (cl-first arguments))))
    (cl-destructuring-bind ((prog &rest args) &rest keys) arguments
      (cl-list* :name name :program prog :program-args args keys))))

(cl-defun slime-start (&key (program inferior-lisp-program) program-args
                            directory
                            (coding-system slime-net-coding-system)
                            (init 'slime-init-command)
                            name
                            (buffer "*inferior-lisp*")
                            init-function
                            env)
  "Start a Lisp process and connect to it.
This function is intended for programmatic use if `slime' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `slime-init-command'.
CODING-SYSTEM a symbol for the coding system. The default is
  slime-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer
                    :coding-system coding-system :init init :name name
                    :init-function init-function :env env)))
    (slime-check-coding-system coding-system)
    (when (slime-bytecode-stale-p)
      (slime-urge-bytecode-recompile))
    (let ((proc (slime-maybe-start-lisp program program-args env
                                        directory buffer)))
      (slime-inferior-connect proc args)
      (pop-to-buffer (process-buffer proc)))))

(defun slime-start* (options)
  (apply #'slime-start options))

(defun slime-connect (host port &optional _coding-system interactive-p &rest parameters)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer
                      "Host: " (cl-first slime-connect-host-history)
                      nil nil '(slime-connect-host-history . 1))
                     (string-to-number
                      (read-from-minibuffer
                       "Port: " (cl-first slime-connect-port-history)
                       nil nil '(slime-connect-port-history . 1)))
                     nil t))
  (slime-setup)
  (when (and interactive-p
             slime-net-processes
             (y-or-n-p "Close old connections first? "))
    (slime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (slime-setup-connection (apply 'slime-net-connect host port parameters)))

;; FIXME: seems redundant
(defun slime-start-and-init (options fun)
  (let* ((rest (plist-get options :init-function))
         (init (cond (rest `(lambda () (funcall ',rest) (funcall ',fun)))
                     (t fun))))
    (slime-start* (plist-put (cl-copy-list options) :init-function init))))

;;;;; Start inferior lisp
;;;
;;; Here is the protocol for starting SLIME:
;;;
;;;   0. Emacs recompiles/reloads slime.elc if it exists and is stale.
;;;   1. Emacs starts an inferior Lisp process.
;;;   2. Emacs tells Lisp (via stdio) to load and start Swank.
;;;   3. Lisp recompiles the Swank if needed.
;;;   4. Lisp starts the Swank server and writes its TCP port to a temp file.
;;;   5. Emacs reads the temp file to get the port and then connects.
;;;   6. Emacs prints a message of warm encouragement for the hacking ahead.
;;;
;;; Between steps 2-5 Emacs polls for the creation of the temp file so
;;; that it can make the connection. This polling may continue for a
;;; fair while if Swank needs recompilation.

(defvar slime-connect-retry-timer nil
  "Timer object while waiting for an inferior-lisp to start.")

;;; Recompiling bytecode:

(defun slime-bytecode-stale-p ()
  "Return true if slime.elc is older than slime.el."
  (let ((libfile (locate-library "slime")))
    (when libfile
      (let* ((basename (file-name-sans-extension libfile))
             (sourcefile (concat basename ".el"))
             (bytefile (concat basename ".elc")))
        (and (file-exists-p bytefile)
             (file-newer-than-file-p sourcefile bytefile))))))

(defun slime-recompile-bytecode ()
  "Recompile and reload slime."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension (locate-library "slime"))
                            ".el")))
    (byte-compile-file sourcefile t)))

(defun slime-urge-bytecode-recompile ()
  "Urge the user to recompile slime.elc.
Return true if we have been given permission to continue."
  (when (y-or-n-p "slime.elc is older than source.  Recompile first? ")
    (slime-recompile-bytecode)))

(defun slime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (slime-connect-retry-timer
         (slime-cancel-connect-retry-timer)
         (message "Cancelled connection attempt."))
        (t (error "Not connecting"))))

;;; Starting the inferior Lisp and loading Swank:

(defun slime-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (slime-start-lisp program program-args env directory buffer))
        ((slime-reinitialize-inferior-lisp-p program program-args env buffer)
         (let ((conn (cl-find (get-buffer-process buffer)
                              slime-net-processes
                              :key #'slime-inferior-process)))
           (when conn
             (slime-net-close conn)))
         (get-buffer-process buffer))
        (t (slime-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun slime-reinitialize-inferior-lisp-p (program program-args env buffer)
  (let ((args (slime-inferior-lisp-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
         (equal (plist-get args :program-args) program-args)
         (equal (plist-get args :env) env)
         (not (y-or-n-p "Create an additional *inferior-lisp*? ")))))

(defvar slime-inferior-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun slime-start-lisp (program program-args env directory buffer)
  "Does the same as `inferior-lisp' but less ugly.
Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
          (process-connection-type nil))
      (comint-exec (current-buffer) "inferior-lisp" program nil program-args))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      (slime-set-query-on-exit-flag proc)
      (run-hooks 'slime-inferior-process-start-hook)
      proc)))

(defun slime-inferior-connect (process args)
  "Start a Swank server in the inferior Lisp and connect."
  (slime-delete-swank-port-file 'quiet)
  (slime-start-swank-server process args)
  (slime-read-port-and-connect process))

(defvar slime-inferior-lisp-args nil
  "A buffer local variable in the inferior proccess.
See `slime-start'.")

(defun slime-start-swank-server (process args)
  "Start a Swank server on the inferior lisp."
  (cl-destructuring-bind (&key coding-system init &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'slime-inferior-lisp-args)
      (setq slime-inferior-lisp-args args)
      (let ((str (funcall init (slime-swank-port-file) coding-system)))
        (goto-char (process-mark process))
        (insert-before-markers str)
        (process-send-string process str)))))

(defun slime-inferior-lisp-args (process)
  "Return the initial process arguments.
See `slime-start'."
  (with-current-buffer (process-buffer process)
    slime-inferior-lisp-args))

;; XXX load-server & start-server used to be separated. maybe that was  better.
(defun slime-init-command (port-filename _coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                  (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(slime-to-lisp-filename (expand-file-name loader))
                     :verbose t)
               (funcall (read-from-string "swank-loader:init")
                        :from-emacs t)
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename))))))

(defun slime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (expand-file-name (format "slime.%S" (emacs-pid)) (slime-temp-directory)))

(defun slime-temp-directory ()
  (cond ((fboundp 'temp-directory) (temp-directory))
        ((boundp 'temporary-file-directory) temporary-file-directory)
        (t "/tmp/")))

(defun slime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (slime-swank-port-file))
    (error
     (cl-ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
                         (slime-swank-port-file)))))))

(defun slime-read-port-and-connect (inferior-process)
  (slime-attempt-connection inferior-process nil 1))

(defun slime-attempt-connection (process retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (slime-cancel-connect-retry-timer)
  (let ((file (slime-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S .. %d (Abort with `M-x slime-abort-connection'.)"
               file attempt))
    (cond ((and (file-exists-p file)
                (> (nth 7 (file-attributes file)) 0)) ; file size
           (let ((port (slime-read-swank-port))
                 (args (slime-inferior-lisp-args process)))
             (slime-delete-swank-port-file 'message)
             (let ((c (slime-connect slime-lisp-host port
                                     (plist-get args :coding-system))))
               (slime-set-inferior-process c process))))
          ((and retries (zerop retries))
           (message "Gave up connecting to Swank after %d attempts." attempt))
          ((eq (process-status process) 'exit)
           (message "Failed to connect to Swank: inferior process exited."))
          (t
           (when (and (file-exists-p file)
                      (zerop (nth 7 (file-attributes file))))
             (message "(Zero length port file)")
             ;; the file may be in the filesystem but not yet written
             (unless retries (setq retries 3)))
           (cl-assert (not slime-connect-retry-timer))
           (setq slime-connect-retry-timer
                 (run-with-timer
                  0.3 nil
                  #'slime-timer-call #'slime-attempt-connection
                  process (and retries (1- retries))
                  (1+ attempt)))))))

(defun slime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    ((debug error)
     (debug nil (list "Error in timer" fun args data)))))

(defun slime-cancel-connect-retry-timer ()
  (when slime-connect-retry-timer
    (cancel-timer slime-connect-retry-timer)
    (setq slime-connect-retry-timer nil)))

(defun slime-read-swank-port ()
  "Read the Swank server port number from the `slime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (slime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
        (cl-assert (integerp port))
        port))))

(defun slime-toggle-debug-on-swank-error ()
  (interactive)
  (if (slime-eval `(swank:toggle-debug-on-swank-error))
      (message "Debug on SWANK error enabled.")
    (message "Debug on SWANK error disabled.")))

;;; Words of encouragement

(defun slime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar slime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the source be with you!"
    "Take this REPL, brother, and may it serve you well."
    "Lemonodor-fame is but a hack away!"
    "Are we consing yet?"
    ,(format "%s, this could be the start of a beautiful program."
             (slime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun slime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length slime-words-of-encouragement))
             slime-words-of-encouragement)))


;;;; Networking
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SLIME protocol message beings with a 6-byte header followed
;;; by an S-expression as text. The sexp must be readable both by
;;; Emacs and by Common Lisp, so if it contains any embedded code
;;; fragments they should be sent as strings:
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in swank.lisp.

(defvar slime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar slime-net-process-close-hooks '()
  "List of functions called when a slime network connection closes.
The functions are called with the process as their argument.")

(defun slime-secret ()
  "Find the magic secret from the user's home directory.
Return nil if the file doesn't exist or is empty; otherwise the
first line of the file."
  (condition-case _err
      (with-temp-buffer
	(insert-file-contents "~/.slime-secret")
	(goto-char (point-min))
	(buffer-substring (point-min) (line-end-position)))
    (file-error nil)))

;;; Interface

(defun slime-send-secret (proc)
  (let ((secret (slime-secret)))
    (when secret
      (let* ((payload (encode-coding-string secret 'utf-8-unix))
             (string (concat (slime-net-encode-length (length payload))
                             payload)))
        (process-send-string proc string)))))

(defun slime-net-connect (host port &rest parameters)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
         (proc (apply 'open-network-stream "SLIME Lisp" nil host port parameters))
         (buffer (slime-make-net-buffer " *cl-connection*")))
    (push proc slime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'slime-net-filter)
    (set-process-sentinel proc 'slime-net-sentinel)
    (slime-set-query-on-exit-flag proc)
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system proc 'binary 'binary))
    (slime-send-secret proc)
    proc))

(defun slime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun slime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `slime-kill-without-query-p'."
  (when slime-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
                   'set-process-query-on-exit-flag
                 'process-kill-without-query)))
      (funcall fun process nil))))

;;;;; Coding system madness

(defun slime-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (slime-find-coding-system coding-system)))
    (unless props
      (error "Invalid slime-net-coding-system: %s. %s"
             coding-system (mapcar #'car slime-net-valid-coding-systems)))
    (when (and (cl-second props) (boundp 'default-enable-multibyte-characters))
      (cl-assert default-enable-multibyte-characters))
    t))

(defun slime-coding-system-mulibyte-p (coding-system)
  (cl-second (slime-find-coding-system coding-system)))

(defun slime-coding-system-cl-name (coding-system)
  (cl-third (slime-find-coding-system coding-system)))

;;; Interface
(defun slime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((payload (encode-coding-string
                   (concat (slime-prin1-to-string sexp) "\n")
                   'utf-8-unix))
         (string (concat (slime-net-encode-length (length payload))
                         payload)))
    (slime-log-event sexp)
    (process-send-string proc string)))

(defun slime-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (or (let ((candidates (find-coding-systems-string string))
            (base (coding-system-base coding-system)))
        (or (equal candidates '(undecided))
            (memq base candidates)))
      (and (not (multibyte-string-p string))
           (not (slime-coding-system-mulibyte-p coding-system)))))

(defun slime-net-close (process &optional debug)
  (setq slime-net-processes (remove process slime-net-processes))
  (when (eq process slime-default-connection)
    (setq slime-default-connection nil))
  (cond (debug
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'slime-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun slime-net-sentinel (process message)
  (message "Lisp connection closed unexpectedly: %s" message)
  (slime-net-close process))

;;; Socket input is handled by `slime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun slime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (slime-process-available-input process))

(defun slime-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (slime-net-have-input-p)
      (let ((event (slime-net-read-or-lose process))
            (ok nil))
        (slime-log-event event)
        (unwind-protect
            (save-current-buffer
              (slime-dispatch-event event process)
              (setq ok t))
          (unless ok
            (slime-run-when-idle 'slime-process-available-input process)))))))

(defun slime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (slime-net-decode-length))))

(defun slime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 0 nil function args))

(defun slime-handle-net-read-error (error)
  (let ((packet (buffer-string)))
    (slime-with-popup-buffer ((slime-buffer-name :error))
      (princ (format "%s\nin packet:\n%s" (error-message-string error) packet))
      (goto-char (point-min)))
    (cond ((y-or-n-p "Skip this packet? ")
           `(:emacs-skipped-packet ,packet))
          (t
           (when (y-or-n-p "Enter debugger instead? ")
             (debug 'error error))
           (signal (car error) (cdr error))))))

(defun slime-net-read-or-lose (process)
  (condition-case error
      (slime-net-read)
    (error
     (slime-net-close process t)
     (error "net-read error: %S" error))))

(defun slime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (slime-net-decode-length))
         (start (+ (point) 6))
         (end (+ start length)))
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (condition-case error
                 (progn
                   (decode-coding-region start end 'utf-8-unix)
                   (setq end (point-max))
                   (read (current-buffer)))
               (error
                (slime-handle-net-read-error error))))
      (delete-region (point-min) end))))

(defun slime-net-decode-length ()
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6))
                    16))

(defun slime-net-encode-length (n)
  (format "%06x" n))

(defun slime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (let (print-escape-nonascii
        print-escape-newlines
        print-length
        print-level)
    (prin1-to-string sexp)))


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `slime-dispatching-connection' if dynamically bound, or
;;;   `slime-buffer-connection' if this is set buffer-local, or
;;;   `slime-default-connection' otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `slime-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `slime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `slime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and slime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar slime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `slime-buffer-connection' and `slime-default-connection'.")

(make-variable-buffer-local
 (defvar slime-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `slime-default-connection'."))

(defvar slime-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`slime-dispatching-connection' or `slime-buffer-connection'.")

(defun slime-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or slime-dispatching-connection
      slime-buffer-connection
      slime-default-connection))

(defun slime-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (slime-current-connection)))
    (cond ((and (not conn) slime-net-processes)
           (or (slime-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (slime-auto-start)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(define-obsolete-variable-alias 'slime-auto-connect
'slime-auto-start "2.5")
(defcustom slime-auto-start 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Slime is loaded."
  :group 'slime-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun slime-auto-start ()
  (cond ((or (eq slime-auto-start 'always)
             (and (eq slime-auto-start 'ask)
                  (y-or-n-p "No connection.  Start Slime? ")))
         (save-window-excursion
           (slime)
           (while (not (slime-current-connection))
             (sleep-for 1))
           (slime-connection)))
        (t nil)))

(defcustom slime-auto-select-connection 'ask
  "Controls auto selection after the default connection was closed."
  :group 'slime-mode
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun slime-auto-select-connection ()
  (let* ((c0 (car slime-net-processes))
         (c (cond ((eq slime-auto-select-connection 'always) c0)
                  ((and (eq slime-auto-select-connection 'ask)
                        (y-or-n-p
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (slime-connection-name c0))))
                   c0))))
    (when c
      (slime-select-connection c)
      (message "Switching to connection: %s" (slime-connection-name c))
      c)))

(defun slime-select-connection (process)
  "Make PROCESS the default connection."
  (setq slime-default-connection process))

(defvar slime-cycle-connections-hook nil)

(defun slime-cycle-connections-within (connections)
  (let* ((tail (or (cdr (member (slime-current-connection) connections))
                   connections))        ; loop around to the beginning
         (next (car tail)))
    (slime-select-connection next)
    (run-hooks 'slime-cycle-connections-hook)
    (message "Lisp: %s %s"
             (slime-connection-name next)
             (process-contact next))))

(defun slime-next-connection ()
  "Change current slime connection, cycling through all connections."
  (interactive)
  (slime-cycle-connections-within (reverse slime-net-processes)))

(define-obsolete-function-alias 'slime-cycle-connections
  'slime-next-connection "2.13")

(defun slime-prev-connection ()
  "Change current slime connection, cycling through all connections.
Goes in reverse order, relative to `slime-next-connection'."
  (interactive)
  (slime-cycle-connections-within slime-net-processes))

(cl-defmacro slime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `slime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  (declare (indent 1))
  `(with-current-buffer
       (process-buffer (or ,process (slime-connection)
                           (error "No connection")))
     ,@body))

;;; Connection-local variables:

(defmacro slime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `slime-connection'."
  (declare (indent 2))
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (slime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(slime-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))))
       '(\, varname))))

(slime-def-connection-var slime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(slime-def-connection-var slime-lisp-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(slime-def-connection-var slime-lisp-modules '()
  "The strings of Lisp's *MODULES*.")

(slime-def-connection-var slime-pid nil
  "The process id of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-type nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-version nil
  "The implementation type of the Lisp process.")

(slime-def-connection-var slime-lisp-implementation-name nil
  "The short name for the Lisp implementation.")

(slime-def-connection-var slime-lisp-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(slime-def-connection-var slime-connection-name nil
  "The short name for connection.")

(slime-def-connection-var slime-inferior-process nil
  "The inferior process for the connection if any.")

(slime-def-connection-var slime-communication-style nil
  "The communication style.")

(slime-def-connection-var slime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(slime-def-connection-var slime-connection-coding-systems nil
  "Coding systems supported by the Lisp process.")

;;;;; Connection setup

(defvar slime-connection-counter 0
  "The number of SLIME connections made. For generating serial numbers.")

;;; Interface
(defun slime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((slime-dispatching-connection process))
    (slime-init-connection-state process)
    (slime-select-connection process)
    process))

(defun slime-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal slime-net-processes (list proc))
    (setq slime-connection-counter 0))
  (slime-with-connection-buffer ()
    (setq slime-buffer-connection proc))
  (setf (slime-connection-number proc) (cl-incf slime-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (let ((slime-current-thread t))
    (slime-eval-async '(swank:connection-info)
      (slime-curry #'slime-set-connection-info proc))))

(defun slime-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((slime-dispatching-connection connection)
        (slime-current-thread t))
    (cl-destructuring-bind (&key pid style lisp-implementation machine
                                 features version modules encoding
                                 &allow-other-keys) info
      (slime-check-version version connection)
      (setf (slime-pid) pid
            (slime-communication-style) style
            (slime-lisp-features) features
            (slime-lisp-modules) modules)
      (cl-destructuring-bind (&key type name version program)
          lisp-implementation
        (setf (slime-lisp-implementation-type) type
              (slime-lisp-implementation-version) version
              (slime-lisp-implementation-name) name
              (slime-lisp-implementation-program) program
              (slime-connection-name) (slime-generate-connection-name name)))
      (cl-destructuring-bind (&key instance ((:type _)) ((:version _))) machine
        (setf (slime-machine-instance) instance))
      (cl-destructuring-bind (&key coding-systems) encoding
        (setf (slime-connection-coding-systems) coding-systems)))
    (let ((args (let ((p (slime-inferior-process)))
                  (if p (slime-inferior-lisp-args p)))))
      (let ((name (plist-get args ':name)))
        (when name
          (unless (string= (slime-lisp-implementation-name) name)
            (setf (slime-connection-name)
                  (slime-generate-connection-name (symbol-name name))))))
      (slime-load-contribs)
      (run-hooks 'slime-connected-hook)
      (let ((fun (plist-get args ':init-function)))
        (when fun (funcall fun))))
    (message "Connected. %s" (slime-random-words-of-encouragement))))

(defun slime-check-version (version conn)
  (or (equal version slime-protocol-version)
      (equal slime-protocol-version 'ignore)
      (y-or-n-p
       (format "Versions differ: %s (slime) vs. %s (swank). Continue? "
               slime-protocol-version version))
      (slime-net-close conn)
      (top-level)))

(defun slime-generate-connection-name (lisp-name)
  (cl-loop for i from 1
           for name = lisp-name then (format "%s<%d>" lisp-name i)
           while (cl-find name slime-net-processes
                          :key #'slime-connection-name :test #'equal)
           finally (cl-return name)))

(defun slime-connection-close-hook (process)
  (when (eq process slime-default-connection)
    (when slime-net-processes
      (slime-select-connection (car slime-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (slime-connection-number)
               (slime-connection-name)))))

(add-hook 'slime-net-process-close-hooks 'slime-connection-close-hook)

;;;;; Commands on connections

(defun slime-disconnect ()
  "Close the current connection."
  (interactive)
  (slime-net-close (slime-connection)))

(defun slime-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'slime-net-close slime-net-processes))

(defun slime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (cadr (process-contact connection)))

(defun slime-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `slime-connection').
Return nil if there's no process object for the connection."
  (let ((proc (slime-inferior-process connection)))
    (if (and proc
             (memq (process-status proc) '(run stop)))
        proc)))

;; Non-macro version to keep the file byte-compilable.
(defun slime-set-inferior-process (connection process)
  (setf (slime-inferior-process connection) process))

(defun slime-use-sigint-for-interrupt (&optional connection)
  (let ((c (or connection (slime-connection))))
    (cl-ecase (slime-communication-style c)
      ((:fd-handler nil) t)
      ((:spawn :sigio) nil))))

(defvar slime-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun slime-background-activities-enabled-p ()
  (and (let ((con (slime-current-connection)))
         (and con
              (eq (process-status con) 'open)))
       (or (not (slime-busy-p))
           (not slime-inhibit-pipelining))))


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`slime-eval-async') for
;;; most things. Reserve synchronous evaluations (`slime-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `slime-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (cl-first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `slime-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar slime-current-thread t
   "The id of the current thread on the Lisp side.
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar slime-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))

;;; `slime-rex' is the RPC primitive which is used to implement both
;;; `slime-eval' and `slime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(cl-defmacro slime-rex ((&rest saved-vars)
                        (sexp &optional
                              (package '(slime-current-package))
                              (thread 'slime-current-thread))
                        &rest continuations)
  "(slime-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (slime-current-package).

CLAUSES is a list of patterns with same syntax as
`slime-dcase'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (declare (indent 2))
  (let ((result (cl-gensym)))
    `(lexical-let ,(cl-loop for var in saved-vars
                            collect (cl-etypecase var
                                      (symbol (list var var))
                                      (cons var)))
       (slime-dispatch-event
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (slime-dcase ,result
                  ,@continuations)))))))

;;; Interface
(defun slime-current-package ()
  "Return the Common Lisp package in the current context.
If `slime-buffer-package' has a value then return that, otherwise
search for and read an `in-package' form."
  (or slime-buffer-package
      (save-restriction
        (widen)
        (slime-find-buffer-package))))

(defvar slime-find-buffer-package-function 'slime-search-buffer-package
  "*Function to use for `slime-find-buffer-package'.
The result should be the package-name (a string)
or nil if nothing suitable can be found.")

(defun slime-find-buffer-package ()
  "Figure out which Lisp package the current buffer is associated with."
  (funcall slime-find-buffer-package-function))

(make-variable-buffer-local
 (defvar slime-package-cache nil
   "Cons of the form (buffer-modified-tick . package)"))

;; When modifing this code consider cases like:
;;  (in-package #.*foo*)
;;  (in-package #:cl)
;;  (in-package :cl)
;;  (in-package "CL")
;;  (in-package |CL|)
;;  (in-package #+ansi-cl :cl #-ansi-cl 'lisp)

(defun slime-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^[ \t]*(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar slime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun slime-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (slime-current-package)))
  (let* ((tag (cl-gensym (format "slime-result-%d-"
                                 (1+ (slime-continuation-counter)))))
	 (slime-stack-eval-tags (cons tag slime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (slime-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag slime-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort _condition)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (slime-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (accept-process-output nil 0.01)))))))

(defun slime-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (declare (indent 1))
  (slime-rex (cont (buffer (current-buffer)))
      (sexp (or package (slime-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort condition)
     (message "Evaluation aborted on %s." condition)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; slime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :slime-eval-async)

;;; These functions can be handy too:

(defun slime-connected-p ()
  "Return true if the Swank connection is open."
  (not (null slime-net-processes)))

(defun slime-check-connected ()
  "Signal an error if we are not connected to Lisp."
  (unless (slime-connected-p)
    (error "Not connected. Use `%s' to start a Lisp."
           (substitute-command-keys "\\[slime]"))))

;; UNUSED
(defun slime-debugged-connection-p (conn)
  ;; This previously was (AND (SLDB-DEBUGGED-CONTINUATIONS CONN) T),
  ;; but an SLDB buffer may exist without having continuations
  ;; attached to it, e.g. the one resulting from `slime-interrupt'.
  (cl-loop for b in (sldb-buffers)
           thereis (with-current-buffer b
                     (eq slime-buffer-connection conn))))

(defun slime-busy-p (&optional conn)
  "True if Lisp has outstanding requests.
Debugged requests are ignored."
  (let ((debugged (sldb-debugged-continuations (or conn (slime-connection)))))
    (cl-remove-if (lambda (id)
                    (memq id debugged))
                  (slime-rex-continuations)
                  :key #'car)))

(defun slime-sync ()
  "Block until the most recent request has finished."
  (when (slime-rex-continuations)
    (let ((tag (caar (slime-rex-continuations))))
      (while (cl-find tag (slime-rex-continuations) :key #'car)
        (accept-process-output nil 0.1)))))

(defun slime-ping ()
  "Check that communication works."
  (interactive)
  (message "%s" (slime-eval "PONG")))

;;;;; Protocol event handler (cl-the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(slime-def-connection-var slime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(slime-def-connection-var slime-continuation-counter 0
  "Continuation serial number counter.")

(defvar slime-event-hooks)

(defun slime-dispatch-event (event &optional process)
  (let ((slime-dispatching-connection (or process (slime-connection))))
    (or (run-hook-with-args-until-success 'slime-event-hooks event)
        (slime-dcase event
          ((:emacs-rex form package thread continuation)
           (when (and (slime-use-sigint-for-interrupt) (slime-busy-p))
             (slime-display-oneliner "; pipelined request... %S" form))
           (let ((id (cl-incf (slime-continuation-counter))))
             (slime-send `(:emacs-rex ,form ,package ,thread ,id))
             (push (cons id continuation) (slime-rex-continuations))
             (slime--recompute-modelines)))
          ((:return value id)
           (let ((rec (assq id (slime-rex-continuations))))
             (cond (rec (setf (slime-rex-continuations)
                              (remove rec (slime-rex-continuations)))
                        (slime--recompute-modelines)
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:debug-activate thread level &optional select)
           (cl-assert thread)
           (sldb-activate thread level select))
          ((:debug thread level condition restarts frames conts)
           (cl-assert thread)
           (sldb-setup thread level condition restarts frames conts))
          ((:debug-return thread level stepping)
           (cl-assert thread)
           (sldb-exit thread level stepping))
          ((:emacs-interrupt thread)
           (slime-send `(:emacs-interrupt ,thread)))
          ((:channel-send id msg)
           (slime-channel-send (or (slime-find-channel id)
                                   (error "Invalid channel id: %S %S" id msg))
                               msg))
          ((:emacs-channel-send id msg)
           (slime-send `(:emacs-channel-send ,id ,msg)))
          ((:read-from-minibuffer thread tag prompt initial-value)
           (slime-read-from-minibuffer-for-swank thread tag prompt
                                                 initial-value))
          ((:y-or-n-p thread tag question)
           (slime-y-or-n-p thread tag question))
          ((:emacs-return-string thread tag string)
           (slime-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:new-features features)
           (setf (slime-lisp-features) features))
          ((:indentation-update info)
           (slime-handle-indentation-update info))
          ((:eval-no-wait form)
           (slime-check-eval-in-emacs-enabled)
           (eval (read form)))
          ((:eval thread tag form-string)
           (slime-check-eval-in-emacs-enabled)
           (slime-eval-for-lisp thread tag form-string))
          ((:ed-rpc-no-wait fn-name &rest args)
           (let ((fn (intern-soft fn-name)))
             (slime-check-rpc-allowed fn)
             (apply fn args)))
          ((:ed-rpc thread tag fn-name &rest args)
           (slime-rpc-from-lisp thread tag (intern-soft fn-name) args))
          ((:emacs-return thread tag value)
           (slime-send `(:emacs-return ,thread ,tag ,value)))
          ((:ed what)
           (slime-ed what))
          ((:inspect what thread tag)
           (let ((hook (when (and thread tag)
                         (slime-curry #'slime-send
                                      `(:emacs-return ,thread ,tag nil)))))
             (slime-open-inspector what nil hook)))
          ((:background-message message)
           (slime-background-message "%s" message))
          ((:debug-condition thread message)
           (cl-assert thread)
           (message "%s" message))
          ((:ping thread tag)
           (slime-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (slime-with-popup-buffer ((slime-buffer-name :error))
             (princ (format "Invalid protocol message:\n%s\n\n%s"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))
          ((:invalid-rpc id message)
           (setf (slime-rex-continuations)
                 (cl-remove id (slime-rex-continuations) :key #'car))
           (error "Invalid rpc: %s" message))
          ((:emacs-skipped-packet _pkg))
          ((:test-delay seconds) ; for testing only
           (sit-for seconds))))))

(defun slime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (slime-net-send sexp (slime-connection)))

(defun slime-reset ()
  "Clear all pending continuations and erase connection buffer."
  (interactive)
  (setf (slime-rex-continuations) '())
  (mapc #'kill-buffer (sldb-buffers))
  (slime-with-connection-buffer ()
    (erase-buffer)))

(defun slime-send-sigint ()
  (interactive)
  (signal-process (slime-pid) 'SIGINT))

;;;;; Channels

;;; A channel implements a set of operations.  Those operations can be
;;; invoked by sending messages to the channel.  Channels are used for
;;; protocols which can't be expressed naturally with RPCs, e.g. for
;;; streaming data over the wire.
;;;
;;; A channel can be "remote" or "local".  Remote channels are
;;; represented by integers.  Local channels are structures.  Messages
;;; sent to a closed (remote) channel are ignored.

(slime-def-connection-var slime-channels '()
  "Alist of the form (ID . CHANNEL).")

(slime-def-connection-var slime-channels-counter 0
  "Channel serial number counter.")

(cl-defstruct (slime-channel (:conc-name slime-channel.)
                             (:constructor
                              slime-make-channel% (operations name id plist)))
  operations name id plist)

(defun slime-make-channel (operations &optional name)
  (let* ((id (cl-incf (slime-channels-counter)))
         (ch (slime-make-channel% operations name id nil)))
    (push (cons id ch) (slime-channels))
    ch))

(defun slime-close-channel (channel)
  (setf (slime-channel.operations channel) 'closed-channel)
  (let ((probe (assq (slime-channel.id channel) (slime-channels))))
    (cond (probe (setf (slime-channels) (delete probe (slime-channels))))
          (t (error "Invalid channel: %s" channel)))))

(defun slime-find-channel (id)
  (cdr (assq id (slime-channels))))

(defun slime-channel-send (channel message)
  (apply (or (gethash (car message) (slime-channel.operations channel))
             (error "Unsupported operation: %S %S" message channel))
         channel (cdr message)))

(defun slime-channel-put (channel prop value)
  (setf (slime-channel.plist channel)
        (plist-put (slime-channel.plist channel) prop value)))

(defun slime-channel-get (channel prop)
  (plist-get (slime-channel.plist channel) prop))

(eval-and-compile
  (defun slime-channel-method-table-name (type)
    (intern (format "slime-%s-channel-methods" type))))

(defmacro slime-define-channel-type (name)
  (declare (indent defun))
  (let ((tab (slime-channel-method-table-name name)))
    `(progn
       (defvar ,tab)
       (setq ,tab (make-hash-table :size 10)))))

(defmacro slime-define-channel-method (type method args &rest body)
  (declare (indent 3) (debug (&define name sexp lambda-list
                                      def-body)))
  `(puthash ',method
            (lambda (self . ,args) . ,body)
            ,(slime-channel-method-table-name type)))

(defun slime-send-to-remote-channel (channel-id msg)
  (slime-dispatch-event `(:emacs-channel-send ,channel-id ,msg)))

;;;;; Event logging to *slime-events*
;;;
;;; The *slime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar slime-log-events t
  "*Log protocol events to the *slime-events* buffer.")

(defvar slime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *slime-events*.")

(defvar slime-event-buffer-name (slime-buffer-name :events)
  "The name of the slime event buffer.")

(defun slime-log-event (event)
  "Record the fact that EVENT occurred."
  (when slime-log-events
    (with-current-buffer (slime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (slime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun slime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun slime-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer slime-event-buffer-name)
      (let ((buffer (get-buffer-create slime-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when slime-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))


;;;;; Cleanup after a quit

(defun slime-restart-inferior-lisp ()
  "Kill and restart the Lisp subprocess."
  (interactive)
  (cl-assert (slime-inferior-process) () "No inferior lisp process")
  (slime-quit-lisp-internal (slime-connection) 'slime-restart-sentinel t))

(defun slime-restart-sentinel (process _message)
  "Restart the inferior lisp process.
Also rearrange windows."
  (cl-assert (process-status process) 'closed)
  (let* ((proc (slime-inferior-process process))
         (args (slime-inferior-lisp-args proc))
         (buffer (buffer-name (process-buffer proc)))
         ;;(buffer-window (get-buffer-window buffer))
         (new-proc (slime-start-lisp (plist-get args :program)
                                     (plist-get args :program-args)
                                     (plist-get args :env)
                                     nil
                                     buffer)))
    (slime-net-close process)
    (slime-inferior-connect new-proc args)
    (switch-to-buffer buffer)
    (goto-char (point-max))))


;;;; Compilation and the creation of compiler-note annotations

(defvar slime-highlight-compiler-notes t
  "*When non-nil annotate buffers with compilation notes etc.")

(defvar slime-before-compile-functions nil
  "A list of function called before compiling a buffer or region.
The function receive two arguments: the beginning and the end of the
region that will be compiled.")

;; FIXME: remove some of the options
(defcustom slime-compilation-finished-hook 'slime-maybe-show-compilation-log
  "Hook called with a list of compiler notes after a compilation."
  :group 'slime-mode
  :type 'hook
  :options '(slime-maybe-show-compilation-log
             slime-create-compilation-log
             slime-show-compilation-log
             slime-maybe-list-compiler-notes
             slime-list-compiler-notes
             slime-maybe-show-xrefs-for-notes
             slime-goto-first-note))

;; FIXME: I doubt that anybody uses this directly and it seems to be
;; only an ugly way to pass arguments.
(defvar slime-compilation-policy nil
  "When non-nil compile with these optimization settings.")

(defun slime-compute-policy (arg)
  "Return the policy for the prefix argument ARG."
  (let ((between (lambda (min n max)
                   (cond ((< n min) min)
                         ((> n max) max)
                         (t n)))))
    (let ((n (prefix-numeric-value arg)))
      (cond ((not arg)   slime-compilation-policy)
            ((cl-plusp n)   `((cl:debug . ,(funcall between 0 n 3))))
            ((eq arg '-) `((cl:speed . 3)))
            (t           `((cl:speed . ,(funcall between 0 (abs n) 3))))))))

(cl-defstruct (slime-compilation-result
               (:type list)
               (:conc-name slime-compilation-result.)
               (:constructor nil)
               (:copier nil))
  tag notes successp duration loadp faslfile)

(defvar slime-last-compilation-result nil
  "The result of the most recently issued compilation.")

(defun slime-compiler-notes ()
  "Return all compiler notes, warnings, and errors."
  (slime-compilation-result.notes slime-last-compilation-result))

(defun slime-compile-and-load-file (&optional policy)
  "Compile and load the buffer's file and highlight compiler notes.

With (positive) prefix argument the file is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign.

Each source location that is the subject of a compiler note is
underlined and annotated with the relevant information. The commands
`slime-next-note' and `slime-previous-note' can be used to navigate
between compiler notes and to display their full details."
  (interactive "P")
  (slime-compile-file t (slime-compute-policy policy)))

(defcustom slime-compile-file-options '()
  "Plist of additional options that C-c C-k should pass to Lisp.
Currently only :fasl-directory is supported."
  :group 'slime-lisp
  :type '(plist :key-type symbol :value-type (file :must-match t)))

(defun slime-compile-file (&optional load policy)
  "Compile current buffer's file and highlight resulting compiler notes.

See `slime-compile-and-load-file' for further details."
  (interactive)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (check-parens)
  (slime--maybe-save-buffer)
  (run-hook-with-args 'slime-before-compile-functions (point-min) (point-max))
  (let ((file (slime-to-lisp-filename (buffer-file-name)))
        (options (slime-simplify-plist `(,@slime-compile-file-options
                                         :policy ,policy))))
    (slime-eval-async
        `(swank:compile-file-for-emacs ,file ,(if load t nil)
                                       . ,(slime-hack-quotes options))
      #'slime-compilation-finished)
    (message "Compiling %s..." file)))

;; FIXME: compilation-save-buffers-predicate was introduced in 24.1
(defun slime--maybe-save-buffer ()
  (let ((slime--this-buffer (current-buffer)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda () (eq (current-buffer) slime--this-buffer)))))

(defun slime-hack-quotes (arglist)
  ;; eval is the wrong primitive, we really want funcall
  (cl-loop for arg in arglist collect `(quote ,arg)))

(defun slime-simplify-plist (plist)
  (cl-loop for (key val) on plist by #'cddr
           append (cond ((null val) '())
                        (t (list key val)))))

(defun slime-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form.

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  (interactive "P")
  (let ((slime-compilation-policy (slime-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (slime-compile-region (region-beginning) (region-end))
      (apply #'slime-compile-region (slime-region-for-defun-at-point)))))

(defun slime-compile-region (start end)
  "Compile the region."
  (interactive "r")
  ;; Check connection before running hooks things like
  ;; slime-flash-region don't make much sense if there's no connection
  (slime-connection)
  (slime-flash-region start end)
  (run-hook-with-args 'slime-before-compile-functions start end)
  (slime-compile-string (buffer-substring-no-properties start end) start))

(defun slime-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun slime-compile-string (string start-offset)
  (let* ((line (save-excursion
                 (goto-char start-offset)
                 (list (line-number-at-pos) (1+ (current-column)))))
         (position `((:position ,start-offset) (:line ,@line))))
    (slime-eval-async
        `(swank:compile-string-for-emacs
          ,string
          ,(buffer-name)
          ',position
          ,(if (buffer-file-name) (slime-to-lisp-filename (buffer-file-name)))
          ',slime-compilation-policy)
      #'slime-compilation-finished)))

(defcustom slime-load-failed-fasl 'ask
  "Which action to take when COMPILE-FILE set FAILURE-P to T.
NEVER doesn't load the fasl
ALWAYS loads the fasl
ASK asks the user."
  :type '(choice (const never)
                 (const always)
                 (const ask)))

(defun slime-load-failed-fasl-p ()
  (cl-ecase slime-load-failed-fasl
    (never nil)
    (always t)
    (ask (y-or-n-p "Compilation failed.  Load fasl file anyway? "))))

(defun slime-compilation-finished (result)
  (with-struct (slime-compilation-result. notes duration successp
                                          loadp faslfile) result
    (setf slime-last-compilation-result result)
    (slime-show-note-counts notes duration (cond ((not loadp) successp)
                                                 (t (and faslfile successp))))
    (when slime-highlight-compiler-notes
      (slime-highlight-notes notes))
    (run-hook-with-args 'slime-compilation-finished-hook notes)
    (when (and loadp faslfile
               (or successp
                   (slime-load-failed-fasl-p)))
      (slime-eval-async `(swank:load-file ,faslfile)))))

(defun slime-show-note-counts (notes secs successp)
  (message (concat
            (cond (successp "Compilation finished")
                  (t (slime-add-face 'font-lock-warning-face
                       "Compilation failed")))
            (if (null notes) ". (No warnings)" ": ")
            (mapconcat
             (lambda (messages)
               (cl-destructuring-bind (sev . notes) messages
                 (let ((len (length notes)))
                   (format "%d %s%s" len (slime-severity-label sev)
                           (if (= len 1) "" "s")))))
             (sort (slime-alistify notes #'slime-note.severity #'eq)
                   (lambda (x y) (slime-severity< (car y) (car x))))
             "  ")
            (if secs (format "  [%.2f secs]" secs)))))

(defun slime-highlight-notes (notes)
  "Highlight compiler notes, warnings, and errors in the buffer."
  (interactive (list (slime-compiler-notes)))
  (with-temp-message "Highlighting notes..."
    (save-excursion
      (save-restriction
        (widen)                  ; highlight notes on the whole buffer
        (slime-remove-old-overlays)
        (mapc #'slime-overlay-note (slime-merge-notes-for-display notes))))))

(defvar slime-note-overlays '()
  "List of overlays created by `slime-make-note-overlay'")

(defun slime-remove-old-overlays ()
  "Delete the existing note overlays."
  (mapc #'delete-overlay slime-note-overlays)
  (setq slime-note-overlays '()))

(defun slime-filter-buffers (predicate)
  "Return a list of where PREDICATE returns true.
PREDICATE is executed in the buffer to test."
  (cl-remove-if-not (lambda (%buffer)
                      (with-current-buffer %buffer
                        (funcall predicate)))
                    (buffer-list)))

;;;;; Recompilation.

;; FIXME: This whole idea is questionable since it depends so
;; crucially on precise source-locs.

(defun slime-recompile-location (location)
  (save-excursion
    (slime-goto-source-location location)
    (slime-compile-defun)))

(defun slime-recompile-locations (locations cont)
  (slime-eval-async
      `(swank:compile-multiple-strings-for-emacs
        ',(cl-loop for loc in locations collect
                   (save-excursion
                     (slime-goto-source-location loc)
                     (cl-destructuring-bind (start end)
                         (slime-region-for-defun-at-point)
                       (list (buffer-substring-no-properties start end)
                             (buffer-name)
                             (slime-current-package)
                             start
                             (if (buffer-file-name)
                                 (slime-to-lisp-filename (buffer-file-name))
                               nil)))))
        ',slime-compilation-policy)
    cont))


;;;;; Merging together compiler notes in the same location.

(defun slime-merge-notes-for-display (notes)
  "Merge together notes that refer to the same location.
This operation is \"lossy\" in the broad sense but not for display purposes."
  (mapcar #'slime-merge-notes
          (slime-group-similar 'slime-notes-in-same-location-p notes)))

(defun slime-merge-notes (notes)
  "Merge NOTES together. Keep the highest severity, concatenate the messages."
  (let* ((new-severity (cl-reduce #'slime-most-severe notes
                                  :key #'slime-note.severity))
         (new-message (mapconcat #'slime-note.message notes "\n")))
    (let ((new-note (cl-copy-list (car notes))))
      (setf (cl-getf new-note :message) new-message)
      (setf (cl-getf new-note :severity) new-severity)
      new-note)))

(defun slime-notes-in-same-location-p (a b)
  (equal (slime-note.location a) (slime-note.location b)))


;;;;; Compiler notes list

(defun slime-one-line-ify (string)
  "Return a single-line version of STRING.
Each newlines and following indentation is replaced by a single space."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n[\n \t]*" nil t)
      (replace-match " "))
    (buffer-string)))

(defun slime-xrefs-for-notes (notes)
  (let ((xrefs))
    (dolist (note notes)
      (let* ((location (cl-getf note :location))
             (fn (cadr (assq :file (cdr location))))
             (file (assoc fn xrefs))
             (node
              (list (format "%s: %s"
                            (cl-getf note :severity)
                            (slime-one-line-ify (cl-getf note :message)))
                    location)))
        (when fn
          (if file
              (push node (cdr file))
            (setf xrefs (cl-acons fn (list node) xrefs))))))
    xrefs))

(defun slime-maybe-show-xrefs-for-notes (notes)
  "Show the compiler notes NOTES if they come from more than one file."
  (let ((xrefs (slime-xrefs-for-notes notes)))
    (when (slime-length> xrefs 1)          ; >1 file
      (slime-show-xrefs
       xrefs 'definition "Compiler notes" (slime-current-package)))))

(defun slime-note-has-location-p (note)
  (not (eq ':error (car (slime-note.location note)))))

(defun slime-redefinition-note-p (note)
  (eq (slime-note.severity note) :redefinition))

(defun slime-create-compilation-log (notes)
  "Create a buffer for `next-error' to use."
  (with-current-buffer (get-buffer-create (slime-buffer-name :compilation))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (slime-insert-compilation-log notes)
    (compilation-mode)))

(defun slime-maybe-show-compilation-log (notes)
  "Display the log on failed compilations or if NOTES is non-nil."
  (slime-create-compilation-log notes)
  (with-struct (slime-compilation-result. notes duration successp)
      slime-last-compilation-result
    (unless successp
      (with-current-buffer (slime-buffer-name :compilation)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "Compilation " (if successp "succeeded." "failed."))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun slime-show-compilation-log (notes)
  "Create and display the compilation log buffer."
  (interactive (list (slime-compiler-notes)))
  (slime-with-popup-buffer ((slime-buffer-name :compilation)
                            :mode 'compilation-mode)
    (slime-insert-compilation-log notes)))

(defun slime-insert-compilation-log (notes)
  "Insert NOTES in format suitable for `compilation-mode'."
  (cl-destructuring-bind (grouped-notes canonicalized-locs-table)
      (slime-group-and-sort-notes notes)
    (with-temp-message "Preparing compilation log..."
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)) ; inefficient font-lock-hook
        (insert (format "cd %s\n%d compiler notes:\n\n"
                        default-directory (length notes)))
        (dolist (notes grouped-notes)
          (let ((loc (gethash (cl-first notes) canonicalized-locs-table))
                (start (point)))
            (insert (slime-canonicalized-location-to-string loc) ":")
            (slime-insert-note-group notes)
            (insert "\n")
            (slime-make-note-overlay (cl-first notes) start (1- (point))))))
      (set (make-local-variable 'compilation-skip-threshold) 0)
      (setq next-error-last-buffer (current-buffer)))))

(defun slime-insert-note-group (notes)
  "Insert a group of compiler messages."
  (insert "\n")
  (dolist (note notes)
    (insert "  " (slime-severity-label (slime-note.severity note)) ": ")
    (let ((start (point)))
      (insert (slime-note.message note))
      (let ((ctx (slime-note.source-context note)))
        (if ctx (insert "\n" ctx)))
      (slime-indent-block start 4))
    (insert "\n")))

(defun slime-indent-block (start column)
  "If the region back to START isn't a one-liner indent it."
  (when (< start (line-beginning-position))
    (save-excursion
      (goto-char start)
      (insert "\n"))
    (slime-indent-rigidly start (point) column)))

(defun slime-canonicalized-location (location)
  "Return a list (FILE LINE COLUMN) for slime-location LOCATION.
This is quite an expensive operation so use carefully."
  (save-excursion
    (slime-goto-location-buffer (slime-location.buffer location))
    (save-excursion
      (slime-goto-source-location location)
      (list (or (buffer-file-name) (buffer-name))
            (save-restriction
              (widen)
              (line-number-at-pos))
            (1+ (current-column))))))

(defun slime-canonicalized-location-to-string (loc)
  (if loc
      (cl-destructuring-bind (filename line col) loc
        (format "%s:%d:%d"
                (cond ((not filename) "")
                      ((let ((rel (file-relative-name filename)))
                         (if (< (length rel) (length filename))
                             rel)))
                      (t filename))
                line col))
    (format "Unknown location")))

(defun slime-goto-note-in-compilation-log (note)
  "Find `note' in the compilation log and display it."
  (with-current-buffer (get-buffer (slime-buffer-name :compilation))
    (let ((pos
           (save-excursion
             (goto-char (point-min))
             (cl-loop for overlay = (slime-find-next-note)
                      while overlay
                      for other-note = (overlay-get overlay 'slime-note)
                      when (slime-notes-in-same-location-p note other-note)
                      return (overlay-start overlay)))))
      (when pos
        (slime--display-position pos nil 0)))))

(defun slime-group-and-sort-notes (notes)
  "First sort, then group NOTES according to their canonicalized locs."
  (let ((locs (make-hash-table :test #'eq)))
    (mapc (lambda (note)
            (let ((loc (slime-note.location note)))
              (when (slime-location-p loc)
                (puthash note (slime-canonicalized-location loc) locs))))
          notes)
    (list (slime-group-similar
           (lambda (n1 n2)
             (equal (gethash n1 locs nil) (gethash n2 locs t)))
           (let* ((bottom most-negative-fixnum)
                  (+default+ (list "" bottom bottom)))
             (sort notes
                   (lambda (n1 n2)
                     (cl-destructuring-bind ((filename1 line1 col1)
                                             (filename2 line2 col2))
                         (list (gethash n1 locs +default+)
                               (gethash n2 locs +default+))
                       (cond ((string-lessp filename1 filename2) t)
                             ((string-lessp filename2 filename1) nil)
                             ((< line1 line2) t)
                             ((> line1 line2) nil)
                             (t (< col1 col2))))))))
          locs)))

(defun slime-note.severity (note)
  (plist-get note :severity))

(defun slime-note.message (note)
  (plist-get note :message))

(defun slime-note.source-context (note)
  (plist-get note :source-context))

(defun slime-note.location (note)
  (plist-get note :location))

(defun slime-severity-label (severity)
  (cl-subseq (symbol-name severity) 1))


;;;;; Adding a single compiler note

(defun slime-overlay-note (note)
  "Add a compiler note to the buffer as an overlay.
If an appropriate overlay for a compiler note in the same location
already exists then the new information is merged into it. Otherwise a
new overlay is created."
  (cl-multiple-value-bind (start end) (slime-choose-overlay-region note)
    (when start
      (goto-char start)
      (let ((severity (plist-get note :severity))
            (message (plist-get note :message))
            (overlay (slime-note-at-point)))
        (if overlay
            (slime-merge-note-into-overlay overlay severity message)
          (slime-create-note-overlay note start end severity message))))))

(defun slime-make-note-overlay (note start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'slime-note note)
    (push overlay slime-note-overlays)
    overlay))

(defun slime-create-note-overlay (note start end severity message)
  "Create an overlay representing a compiler note.
The overlay has several properties:
  FACE       - to underline the relevant text.
  SEVERITY   - for future reference :NOTE, :STYLE-WARNING, :WARNING, or :ERROR.
  MOUSE-FACE - highlight the note when the mouse passes over.
  HELP-ECHO  - a string describing the note, both for future reference
               and for display as a tooltip (due to the special
               property name)."
  (let ((overlay (slime-make-note-overlay note start end)))
    (cl-macrolet ((putp (name value) `(overlay-put overlay ,name ,value)))
      (putp 'face (slime-severity-face severity))
      (putp 'severity severity)
      (putp 'mouse-face 'highlight)
      (putp 'help-echo message)
      overlay)))

;; XXX Obsolete due to `slime-merge-notes-for-display' doing the
;; work already -- unless we decide to put several sets of notes on a
;; buffer without clearing in between, which only this handles.
(defun slime-merge-note-into-overlay (overlay severity message)
  "Merge another compiler note into an existing overlay.
The help text describes both notes, and the highest of the severities
is kept."
  (cl-macrolet ((putp (name value) `(overlay-put overlay ,name ,value))
                (getp (name)       `(overlay-get overlay ,name)))
    (putp 'severity (slime-most-severe severity (getp 'severity)))
    (putp 'face (slime-severity-face (getp 'severity)))
    (putp 'help-echo (concat (getp 'help-echo) "\n" message))))

(defun slime-choose-overlay-region (note)
  "Choose the start and end points for an overlay over NOTE.
If the location's sexp is a list spanning multiple lines, then the
region around the first element is used.
Return nil if there's no useful source location."
  (let ((location (slime-note.location note)))
    (when location
      (slime-dcase location
        ((:error _))                 ; do nothing
        ((:location file pos _hints)
         (cond ((eq (car file) ':source-form) nil)
               ((eq (slime-note.severity note) :read-error)
                (slime-choose-overlay-for-read-error location))
               ((equal pos '(:eof))
                (cl-values (1- (point-max)) (point-max)))
               (t
                (slime-choose-overlay-for-sexp location))))))))

(defun slime-choose-overlay-for-read-error (location)
  (let ((pos (slime-location-offset location)))
    (save-excursion
      (goto-char pos)
      (cond ((slime-symbol-at-point)
             ;; package not found, &c.
             (cl-values (slime-symbol-start-pos) (slime-symbol-end-pos)))
            (t
             (cl-values pos (1+ pos)))))))

(defun slime-choose-overlay-for-sexp (location)
  (slime-goto-source-location location)
  (skip-chars-forward "'#`")
  (let ((start (point)))
    (ignore-errors (slime-forward-sexp))
    (if (slime-same-line-p start (point))
        (cl-values start (point))
      (cl-values (1+ start)
                 (progn (goto-char (1+ start))
                        (ignore-errors (forward-sexp 1))
                        (point))))))

(defun slime-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defvar slime-severity-face-plist
  '(:error                     slime-error-face
    :read-error                slime-error-face
    :warning                   slime-warning-face
    :redefinition              slime-style-warning-face
    :style-warning             slime-style-warning-face
    :early-deprecation-warning slime-early-deprecation-warning-face
    :late-deprecation-warning  slime-late-deprecation-warning-face
    :final-deprecation-warning slime-final-deprecation-warning-face
    :note                      slime-note-face))

(defun slime-severity-face (severity)
  "Return the name of the font-lock face representing SEVERITY."
  (or (plist-get slime-severity-face-plist severity)
      (error "No face for: %S" severity)))

(defvar slime-severity-order
  '(:note
    :early-deprecation-warning :style-warning :redefinition
    :late-deprecation-warning :final-deprecation-warning
    :warning :error :read-error))

(defun slime-severity< (sev1 sev2)
  "Return true if SEV1 is less severe than SEV2."
  (< (cl-position sev1 slime-severity-order)
     (cl-position sev2 slime-severity-order)))

(defun slime-most-severe (sev1 sev2)
  "Return the most servere of two conditions."
  (if (slime-severity< sev1 sev2) sev2 sev1))

;; XXX: unused function
(defun slime-visit-source-path (source-path)
  "Visit a full source path including the top-level form."
  (goto-char (point-min))
  (slime-forward-source-path source-path))

(defun slime-forward-positioned-source-path (source-path)
  "Move forward through a sourcepath from a fixed position.
The point is assumed to already be at the outermost sexp, making the
first element of the source-path redundant."
  (ignore-errors
    (slime-forward-sexp)
    (beginning-of-defun))
  (let ((source-path (cdr source-path)))
    (when source-path
      (down-list 1)
      (slime-forward-source-path source-path))))

(defun slime-forward-source-path (source-path)
  (let ((origin (point)))
    (condition-case nil
        (progn
          (cl-loop for (count . more) on source-path
                   do (progn
                        (slime-forward-sexp count)
                        (when more (down-list 1))))
          ;; Align at beginning
          (slime-forward-sexp)
          (beginning-of-sexp))
      (error (goto-char origin)))))


;; FIXME: really fix this mess
;; FIXME: the check shouln't be done here anyway but by M-. itself.

(defun slime-filesystem-toplevel-directory ()
  ;; Windows doesn't have a true toplevel root directory, and all
  ;; filenames look like "c:/foo/bar/quux.baz" from an Emacs
  ;; perspective anyway.
  (if (memq system-type '(ms-dos windows-nt))
      ""
    (file-name-as-directory "/")))

(defun slime-file-name-merge-source-root (target-filename buffer-filename)
  "Returns a filename where the source root directory of TARGET-FILENAME
is replaced with the source root directory of BUFFER-FILENAME.

If no common source root could be determined, return NIL.

E.g. (slime-file-name-merge-source-root
       \"/usr/local/src/joe/upstream/sbcl/code/late-extensions.lisp\"
       \"/usr/local/src/joe/hacked/sbcl/compiler/deftype.lisp\")

        ==> \"/usr/local/src/joe/hacked/sbcl/code/late-extensions.lisp\"
"
  (let ((target-dirs (split-string (file-name-directory target-filename)
                                   "/" t))
        (buffer-dirs (split-string (file-name-directory buffer-filename)
                                   "/" t)))
    ;; Starting from the end, we look if one of the TARGET-DIRS exists
    ;; in BUFFER-FILENAME---if so, it and everything left from that dirname
    ;; is considered to be the source root directory of BUFFER-FILENAME.
    (cl-loop with target-suffix-dirs = nil
             with buffer-dirs* = (reverse buffer-dirs)
             with target-dirs* = (reverse target-dirs)
             for target-dir in target-dirs*
             do (let  ((concat-dirs (lambda (dirs)
                                      (apply #'concat
                                             (mapcar #'file-name-as-directory
                                                     dirs))))
                       (pos (cl-position target-dir buffer-dirs*
                                         :test #'equal)))
                  (if (not pos)    ; TARGET-DIR not in BUFFER-FILENAME?
                      (push target-dir target-suffix-dirs)
                    (let* ((target-suffix
                                        ; PUSH reversed for us!
                            (funcall concat-dirs target-suffix-dirs))
                           (buffer-root
                            (funcall concat-dirs
                                     (reverse (nthcdr pos buffer-dirs*)))))
                      (cl-return (concat (slime-filesystem-toplevel-directory)
                                         buffer-root
                                         target-suffix
                                         (file-name-nondirectory
                                          target-filename)))))))))

(defun slime-highlight-differences-in-dirname (base-dirname contrast-dirname)
  "Returns a copy of BASE-DIRNAME where all differences between
BASE-DIRNAME and CONTRAST-DIRNAME are propertized with a
highlighting face."
  (setq base-dirname (file-name-as-directory base-dirname))
  (setq contrast-dirname (file-name-as-directory contrast-dirname))
  (let ((base-dirs (split-string base-dirname "/" t))
        (contrast-dirs (split-string contrast-dirname "/" t)))
    (with-temp-buffer
      (cl-loop initially (insert (slime-filesystem-toplevel-directory))
               for base-dir in base-dirs do
               (let ((pos (cl-position base-dir contrast-dirs :test #'equal)))
                 (cond ((not pos)
                        (slime-insert-propertized '(face highlight) base-dir)
                        (insert "/"))
                       (t
                        (insert (file-name-as-directory base-dir))
                        (setq contrast-dirs
                              (nthcdr (1+ pos) contrast-dirs))))))
      (buffer-substring (point-min) (point-max)))))

(defvar slime-warn-when-possibly-tricked-by-M-. t
  "When working on multiple source trees simultaneously, the way
`slime-edit-definition' (M-.) works can sometimes be confusing:

`M-.' visits locations that are present in the current Lisp image,
which works perfectly well as long as the image reflects the source
tree that one is currently looking at.

In the other case, however, one can easily end up visiting a file
in a different source root directory (cl-the one corresponding to
the Lisp image), and is thus easily tricked to modify the wrong
source files---which can lead to quite some stressfull cursing.

If this variable is T, a warning message is issued to raise the
user's attention whenever `M-.' is about opening a file in a
different source root that also exists in the source root
directory of the user's current buffer.

There's no guarantee that all possible cases are covered, but
if you encounter such a warning, it's a strong indication that
you should check twice before modifying.")

(defun slime-maybe-warn-for-different-source-root (target-filename
                                                   buffer-filename)
  (let ((guessed-target (slime-file-name-merge-source-root target-filename
                                                           buffer-filename)))
    (when (and guessed-target
               (not (equal guessed-target target-filename))
               (file-exists-p guessed-target))
      (slime-message "Attention: This is `%s'."
                     (concat (slime-highlight-differences-in-dirname
                              (file-name-directory target-filename)
                              (file-name-directory guessed-target))
                             (file-name-nondirectory target-filename))))))

(defun slime-check-location-filename-sanity (filename)
  (when slime-warn-when-possibly-tricked-by-M-.
    (cl-macrolet ((truename-safe (file) `(and ,file (file-truename ,file))))
      (let ((target-filename (truename-safe filename))
            (buffer-filename (truename-safe (buffer-file-name))))
        (when (and target-filename
                   buffer-filename)
          (slime-maybe-warn-for-different-source-root
           target-filename buffer-filename))))))

(defun slime-check-location-buffer-name-sanity (buffer-name)
  (slime-check-location-filename-sanity
   (buffer-file-name (get-buffer buffer-name))))



(defun slime-goto-location-buffer (buffer)
  (slime-dcase buffer
    ((:file filename)
     (let ((filename (slime-from-lisp-filename filename)))
       (slime-check-location-filename-sanity filename)
       (set-buffer (or (get-file-buffer filename)
                       (let ((find-file-suppress-same-file-warnings t))
                         (find-file-noselect filename))))))
    ((:buffer buffer-name)
     (slime-check-location-buffer-name-sanity buffer-name)
     (set-buffer buffer-name))
    ((:buffer-and-file buffer filename)
     (slime-goto-location-buffer
      (if (get-buffer buffer)
          (list :buffer buffer)
        (list :file filename))))
    ((:source-form string)
     (set-buffer (get-buffer-create (slime-buffer-name :source)))
     (erase-buffer)
     (lisp-mode)
     (insert string)
     (goto-char (point-min)))
    ((:zip file entry)
     (require 'arc-mode)
     (set-buffer (find-file-noselect file t))
     (goto-char (point-min))
     (re-search-forward (concat "  " entry "$"))
     (let ((buffer (save-window-excursion
                     (archive-extract)
                     (current-buffer))))
       (set-buffer buffer)
       (goto-char (point-min))))))

(defun slime-goto-location-position (position)
  (slime-dcase position
    ((:position pos)
     (goto-char 1)
     (forward-char (- (1- pos) (slime-eol-conversion-fixup (1- pos)))))
    ((:offset start offset)
     (goto-char start)
     (forward-char offset))
    ((:line start &optional column)
     (goto-char (point-min))
     (beginning-of-line start)
     (cond (column (move-to-column column))
           (t (skip-chars-forward " \t"))))
    ((:function-name name)
     (let ((case-fold-search t)
           (name (regexp-quote name)))
       (goto-char (point-min))
       (when (or
              (re-search-forward
               (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +(*%s\\S_"
                       (regexp-quote name)) nil t)
              (re-search-forward
               (format "[( \t]%s\\>\\(\\s \\|$\\)" name) nil t))
         (goto-char (match-beginning 0)))))
    ((:method name specializers &rest qualifiers)
     (slime-search-method-location name specializers qualifiers))
    ((:source-path source-path start-position)
     (cond (start-position
            (goto-char start-position)
            (slime-forward-positioned-source-path source-path))
           (t
            (slime-forward-source-path source-path))))
    ((:eof)
     (goto-char (point-max)))))

(defun slime-eol-conversion-fixup (n)
  ;; Return the number of \r\n eol markers that we need to cross when
  ;; moving N chars forward.  N is the number of chars but \r\n are
  ;; counted as 2 separate chars.
  (cl-case (coding-system-eol-type buffer-file-coding-system)
    ((1)
     (save-excursion
       (cl-do ((pos (+ (point) n))
               (count 0 (1+ count)))
           ((>= (point) pos) (1- count))
         (forward-line)
         (cl-decf pos))))
    (t 0)))

(defun slime-search-method-location (name specializers qualifiers)
  ;; Look for a sequence of words (def<something> method name
  ;; qualifers specializers don't look for "T" since it isn't requires
  ;; (arg without t) as class is taken as such.
  (let* ((case-fold-search t)
         (name (regexp-quote name))
         (qualifiers (mapconcat (lambda (el) (concat ".+?\\<" el "\\>"))
                                qualifiers ""))
         (specializers (mapconcat
                        (lambda (el)
                          (if (eql (aref el 0) ?\()
                              (let ((spec (read el)))
                                (if (eq (car spec) 'EQL)
                                    (concat
                                     ".*?\\n\\{0,1\\}.*?(EQL.*?'\\{0,1\\}"
                                     (format "%s" (cl-second spec)) ")")
                                  (error "don't understand specializer: %s,%s"
                                         el (car spec))))
                            (concat ".+?\n\\{0,1\\}.+?\\<" el "\\>")))
                        (remove "T" specializers) ""))
         (regexp (format "\\s *(def\\(\\s_\\|\\sw\\)*\\s +%s\\s +%s%s" name
                         qualifiers specializers)))
    (or (and (re-search-forward regexp  nil t)
             (goto-char (match-beginning 0)))
        ;;	(slime-goto-location-position `(:function-name ,name))
        )))

(defun slime-search-call-site (fname)
  "Move to the place where FNAME called.
Don't move if there are multiple or no calls in the current defun."
  (save-restriction
    (narrow-to-defun)
    (let ((start (point))
          (regexp (concat "(" fname "[)\n \t]"))
          (case-fold-search t))
      (cond ((and (re-search-forward regexp nil t)
                  (not (re-search-forward regexp nil t)))
             (goto-char (match-beginning 0)))
            (t (goto-char start))))))

(defun slime-search-edit-path (edit-path)
  "Move to EDIT-PATH starting at the current toplevel form."
  (when edit-path
    (unless (and (= (current-column) 0)
                 (looking-at "("))
      (beginning-of-defun))
    (slime-forward-source-path edit-path)))

(defun slime-goto-source-location (location &optional noerror)
  "Move to the source location LOCATION.  Several kinds of locations
are supported:

<location> ::= (:location <buffer> <position> <hints>)
             | (:error <message>)

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:buffer-and-file <buffername> <filename>)
             | (:source-form <string>)
             | (:zip <file> <entry>)

<position> ::= (:position <fixnum>) ; 1 based (for files)
             | (:offset <start> <offset>) ; start+offset (for C-c C-c)
             | (:line <line> [<column>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>)
             | (:method <name string> <specializers> . <qualifiers>)"
  (slime-dcase location
    ((:location buffer _position _hints)
     (slime-goto-location-buffer buffer)
     (let ((pos (slime-location-offset location)))
       (cond ((and (<= (point-min) pos) (<= pos (point-max))))
             (widen-automatically (widen))
             (t
              (error "Location is outside accessible part of buffer")))
       (goto-char pos)))
    ((:error message)
     (if noerror
         (slime-message "%s" message)
       (error "%s" message)))))

(defun slime-location-offset (location)
  "Return the position, as character number, of LOCATION."
  (save-restriction
    (widen)
    (condition-case nil
        (slime-goto-location-position
         (slime-location.position location))
      (error (goto-char 0)))
    (cl-destructuring-bind (&key snippet edit-path call-site align)
        (slime-location.hints location)
      (when snippet (slime-isearch snippet))
      (when edit-path (slime-search-edit-path edit-path))
      (when call-site (slime-search-call-site call-site))
      (when align
        (slime-forward-sexp)
        (beginning-of-sexp)))
    (point)))


;;;;; Incremental search
;;
;; Search for the longest match of a string in either direction.
;;
;; This is for locating text that is expected to be near the point and
;; may have been modified (but hopefully not near the beginning!)

(defun slime-isearch (string)
  "Find the longest occurence of STRING either backwards of forwards.
If multiple matches exist the choose the one nearest to point."
  (goto-char
   (let* ((start (point))
          (len1 (slime-isearch-with-function 'search-forward string))
          (pos1 (point)))
     (goto-char start)
     (let* ((len2 (slime-isearch-with-function 'search-backward string))
            (pos2 (point)))
       (cond ((and len1 len2)
              ;; Have a match in both directions
              (cond ((= len1 len2)
                     ;; Both are full matches -- choose the nearest.
                     (if (< (abs (- start pos1))
                            (abs (- start pos2)))
                         pos1 pos2))
                    ((> len1 len2) pos1)
                    ((> len2 len1) pos2)))
             (len1 pos1)
             (len2 pos2)
             (t start))))))

(defun slime-isearch-with-function (search-fn string)
  "Search for the longest substring of STRING using SEARCH-FN.
SEARCH-FN is either the symbol `search-forward' or `search-backward'."
  (unless (string= string "")
    (cl-loop for i from 1 to (length string)
             while (funcall search-fn (substring string 0 i) nil t)
             for match-data = (match-data)
             do (cl-case search-fn
                  (search-forward  (goto-char (match-beginning 0)))
                  (search-backward (goto-char (1+ (match-end 0)))))
             finally (cl-return (if (null match-data)
                                    nil
                                  ;; Finish based on the last successful match
                                  (store-match-data match-data)
                                  (goto-char (match-beginning 0))
                                  (- (match-end 0) (match-beginning 0)))))))


;;;;; Visiting and navigating the overlays of compiler notes

(defun slime-next-note ()
  "Go to and describe the next compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (slime-find-next-note)))
    (if note
        (slime-show-note note)
      (goto-char here)
      (message "No next note."))))

(defun slime-previous-note ()
  "Go to and describe the previous compiler note in the buffer."
  (interactive)
  (let ((here (point))
        (note (slime-find-previous-note)))
    (if note
        (slime-show-note note)
      (goto-char here)
      (message "No previous note."))))

(defun slime-goto-first-note (&rest _)
  "Go to the first note in the buffer."
  (let ((point (point)))
    (goto-char (point-min))
    (cond ((slime-find-next-note)
           (slime-show-note (slime-note-at-point)))
          (t (goto-char point)))))

(defun slime-remove-notes ()
  "Remove compiler-note annotations from the current buffer."
  (interactive)
  (slime-remove-old-overlays))

(defun slime-show-note (overlay)
  "Present the details of a compiler note to the user."
  (slime-temporarily-highlight-note overlay)
  (if (get-buffer-window (slime-buffer-name :compilation) t)
      (slime-goto-note-in-compilation-log (overlay-get overlay 'slime-note))
    (let ((message (get-char-property (point) 'help-echo)))
      (slime-message "%s" (if (zerop (length message)) "\"\"" message)))))

;; FIXME: could probably use flash region
(defun slime-temporarily-highlight-note (overlay)
  "Temporarily highlight a compiler note's overlay.
The highlighting is designed to both make the relevant source more
visible, and to highlight any further notes that are nested inside the
current one.

The highlighting is automatically undone with a timer."
  (run-with-timer 0.2 nil
                  #'overlay-put overlay 'face (overlay-get overlay 'face))
  (overlay-put overlay 'face 'slime-highlight-face))


;;;;; Overlay lookup operations

(defun slime-note-at-point ()
  "Return the overlay for a note starting at point, otherwise NIL."
  (cl-find (point) (slime-note-overlays-at-point)
           :key 'overlay-start))

(defun slime-note-overlay-p (overlay)
  "Return true if OVERLAY represents a compiler note."
  (overlay-get overlay 'slime-note))

(defun slime-note-overlays-at-point ()
  "Return a list of all note overlays that are under the point."
  (cl-remove-if-not 'slime-note-overlay-p (overlays-at (point))))

(defun slime-find-next-note ()
  "Go to the next position with the `slime-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (slime-search-property 'slime-note nil #'slime-note-at-point))

(defun slime-find-previous-note ()
  "Go to the next position with the `slime-note' text property.
Retuns the note overlay if such a position is found, otherwise nil."
  (slime-search-property 'slime-note t #'slime-note-at-point))


;;;; Arglist Display

(defun slime-space (n)
  "Insert a space and print some relevant information (function arglist).
Designed to be bound to the SPC key.  Prefix argument can be used to insert
more than one space."
  (interactive "p")
  (self-insert-command n)
  (slime-echo-arglist))

(put 'slime-space 'delete-selection t) ; for delete-section-mode & CUA

(defun slime-echo-arglist ()
  (when (slime-background-activities-enabled-p)
    (let ((op (slime-operator-before-point)))
      (when op
        (slime-eval-async `(swank:operator-arglist ,op
                                                   ,(slime-current-package))
          (lambda (arglist)
            (when arglist
              (slime-message "%s" arglist))))))))

(defvar slime-operator-before-point-function 'slime-lisp-operator-before-point)

(defun slime-operator-before-point ()
  (funcall slime-operator-before-point-function))

(defun slime-lisp-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (slime-symbol-at-point))))

;;;; Completion

;; FIXME: use this in Emacs 24
;;(define-obsolete-function-alias slime-complete-symbol completion-at-point)

(defalias 'slime-complete-symbol #'completion-at-point)
(make-obsolete 'slime-complete-symbol #'completion-at-point "2015-10-17")

;; This is the function that we add to
;; `completion-at-point-functions'.  For backward-compatibilty we look
;; at `slime-complete-symbol-function' first.  The indirection through
;; `slime-completion-at-point-functions' is used so that users don't
;; have to set `completion-at-point-functions' in every slime-like
;; buffer.
(defun slime--completion-at-point ()
  (cond (slime-complete-symbol-function
         slime-complete-symbol-function)
        (t
         (run-hook-with-args-until-success
          'slime-completion-at-point-functions))))

(defun slime-setup-completion ()
  (add-hook 'completion-at-point-functions #'slime--completion-at-point nil t))

(defun slime-simple-completion-at-point ()
  "Complete the symbol at point.
Perform completion similar to `elisp-completion-at-point'."
  (let* ((end (point))
         (beg (slime-symbol-start-pos)))
    (list beg end (completion-table-dynamic #'slime-simple-completions))))

(defun slime-filename-completion ()
  "If point is at a string starting with \", complete it as filename.
Return nil if point is not at filename."
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\="
                                            (max (point-min) (- (point) 1000))
                                            t))
    (let ((comint-completion-addsuffix '("/" . "\"")))
      (comint-filename-completion))))

;; FIXME: for backward compatibility.  Remove it one day
;; together with slime-complete-symbol-function.
(defun slime-simple-complete-symbol ()
  (let ((completion-at-point-functions '(slime-maybe-complete-as-filename
                                         slime-simple-completion-at-point)))
    (completion-at-point)))

;; NOTE: the original idea was to bind this to TAB but that no longer
;; works as `completion-at-point' sets a transient keymap that
;; overrides TAB.  So this is rather useless now.
(defun slime-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line. If indenting doesn't move point, complete
the symbol. If there's no symbol at the point, show the arglist
for the most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (completion-at-point))
            ((memq (char-before) '(?\t ?\ ))
             (slime-echo-arglist))))))

(make-obsolete 'slime-indent-and-complete-symbol
               "Set tab-always-indent to 'complete."
               "2015-10-18")

(defvar slime-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" #'completion-at-point)
    (define-key map "\M-\t" #'completion-at-point)
    map)
  "Minibuffer keymap used for reading CL expressions.")

(defvar slime-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defun slime-minibuffer-setup-hook ()
  (cons (lexical-let ((package (slime-current-package))
                      (connection (slime-connection)))
          (lambda ()
            (setq slime-buffer-package package)
            (setq slime-buffer-connection connection)
            (set-syntax-table lisp-mode-syntax-table)
            (slime-setup-completion)))
        minibuffer-setup-hook))

(defun slime-read-from-minibuffer (prompt &optional initial-value history)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.  The result is a string (\"\" if no input was given)."
  (let ((minibuffer-setup-hook (slime-minibuffer-setup-hook)))
    (read-from-minibuffer prompt initial-value slime-minibuffer-map
			  nil (or history 'slime-minibuffer-history))))

(defun slime-bogus-completion-alist (list)
  "Make an alist out of list.
The same elements go in the CAR, and nil in the CDR. To support the
apparently very stupid `try-completions' interface, that wants an
alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun slime-simple-completions (prefix)
  (cl-destructuring-bind (completions _partial)
      (let ((slime-current-thread t))
        (slime-eval
         `(swank:simple-completions ,(substring-no-properties prefix)
                                    ',(slime-current-package))))
    completions))


;;;; Edit definition

(defun slime-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun slime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(cl-defstruct (slime-xref (:conc-name slime-xref.) (:type list))
  dspec location)

(cl-defstruct (slime-location (:conc-name slime-location.) (:type list)
                              (:constructor nil)
                              (:copier nil))
  tag buffer position hints)

(defun slime-location-p (o) (and (consp o) (eq (car o) :location)))

(defun slime-xref-has-location-p (xref)
  (slime-location-p (slime-xref.location xref)))

(defun make-slime-buffer-location (buffer-name position &optional hints)
  `(:location (:buffer ,buffer-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

(defun make-slime-file-location (file-name position &optional hints)
  `(:location (:file ,file-name) (:position ,position)
              ,(when hints `(:hints ,hints))))

;;; The hooks are tried in order until one succeeds, otherwise the
;;; default implementation involving `slime-find-definitions-function'
;;; is used. The hooks are called with the same arguments as
;;; `slime-edit-definition'.
(defvar slime-edit-definition-hooks)

(defun slime-edit-definition (&optional name where)
  "Lookup the definition of the name at point.
If there's no name at point, or a prefix argument is given, then the
function name is prompted."
  (interactive (list (or (and (not current-prefix-arg)
                              (slime-symbol-at-point))
                         (slime-read-symbol-name "Edit Definition of: "))))
  ;; The hooks might search for a name in a different manner, so don't
  ;; ask the user if it's missing before the hooks are run
  (or (run-hook-with-args-until-success 'slime-edit-definition-hooks
                                        name where)
      (slime-edit-definition-cont (slime-find-definitions name)
                                  name where)))

(defun slime-edit-definition-cont (xrefs name where)
  (cl-destructuring-bind (1loc file-alist) (slime-analyze-xrefs xrefs)
    (cond ((null xrefs)
           (error "No known definition for: %s (in %s)"
                  name (slime-current-package)))
          (1loc
           (slime-push-definition-stack)
           (slime-pop-to-location (slime-xref.location (car xrefs)) where))
          ((slime-length= xrefs 1)      ; ((:error "..."))
           (error "%s" (cadr (slime-xref.location (car xrefs)))))
          (t
           (slime-push-definition-stack)
           (slime-show-xrefs file-alist 'definition name
                             (slime-current-package))))))

(defvar slime-edit-uses-xrefs
  '(:calls :macroexpands :binds :references :sets :specializes))

;;; FIXME. TODO: Would be nice to group the symbols (in each
;;;              type-group) by their home-package.
(defun slime-edit-uses (symbol)
  "Lookup all the uses of SYMBOL."
  (interactive (list (slime-read-symbol-name "Edit Uses of: ")))
  (slime-xrefs slime-edit-uses-xrefs
               symbol
               (lambda (xrefs type symbol package)
                 (cond
                  ((null xrefs)
                   (message "No xref information found for %s." symbol))
                  ((and (slime-length= xrefs 1)          ; one group
                        (slime-length= (cdar  xrefs) 1)) ; one ref in group
                   (cl-destructuring-bind (_ (_ loc)) (cl-first xrefs)
                     (slime-push-definition-stack)
                     (slime-pop-to-location loc)))
                  (t
                   (slime-push-definition-stack)
                   (slime-show-xref-buffer xrefs type symbol package))))))

(defun slime-analyze-xrefs (xrefs)
  "Find common filenames in XREFS.
Return a list (SINGLE-LOCATION FILE-ALIST).
SINGLE-LOCATION is true if all xrefs point to the same location.
FILE-ALIST is an alist of the form ((FILENAME . (XREF ...)) ...)."
  (list (and xrefs
             (let ((loc (slime-xref.location (car xrefs))))
               (and (slime-location-p loc)
                    (cl-every (lambda (x) (equal (slime-xref.location x) loc))
                              (cdr xrefs)))))
        (slime-alistify xrefs #'slime-xref-group #'equal)))

(defun slime-xref-group (xref)
  (cond ((slime-xref-has-location-p xref)
         (slime-dcase (slime-location.buffer (slime-xref.location xref))
           ((:file filename) filename)
           ((:buffer bufname)
            (let ((buffer (get-buffer bufname)))
              (if buffer
                  (format "%S" buffer) ; "#<buffer foo.lisp>"
                (format "%s (previously existing buffer)" bufname))))
           ((:buffer-and-file _buffer filename) filename)
           ((:source-form _) "(S-Exp)")
           ((:zip _zip entry) entry)))
        (t
         "(No location)")))

(defun slime-pop-to-location (location &optional where)
  (slime-goto-source-location location)
  (let ((point (point)))
    (cl-ecase where
      ((nil)  (switch-to-buffer (current-buffer)))
      (window (pop-to-buffer (current-buffer) t))
      (frame  (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t))))
    (goto-char point)))

(defun slime-postprocess-xref (original-xref)
  "Process (for normalization purposes) an Xref comming directly
from SWANK before the rest of Slime sees it. In particular,
convert ETAGS based xrefs to actual file+position based
locations."
  (if (not (slime-xref-has-location-p original-xref))
      (list original-xref)
    (let ((loc (slime-xref.location original-xref)))
      (slime-dcase (slime-location.buffer loc)
        ((:etags-file tags-file)
         (slime-dcase (slime-location.position loc)
           ((:tag &rest tags)
            (visit-tags-table tags-file)
            (mapcar (lambda (xref)
                      (let ((old-dspec (slime-xref.dspec original-xref))
                            (new-dspec (slime-xref.dspec xref)))
                        (setf (slime-xref.dspec xref)
                              (format "%s: %s" old-dspec new-dspec))
                        xref))
                    (cl-mapcan #'slime-etags-definitions tags)))))
        (t
         (list original-xref))))))

(defun slime-postprocess-xrefs (xrefs)
  (cl-mapcan #'slime-postprocess-xref xrefs))

(defun slime-find-definitions (name)
  "Find definitions for NAME."
  (slime-postprocess-xrefs (funcall slime-find-definitions-function name)))

(defun slime-find-definitions-rpc (name)
  (slime-eval `(swank:find-definitions-for-emacs ,name)))

(defun slime-edit-definition-other-window (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'window))

(defun slime-edit-definition-other-frame (name)
  "Like `slime-edit-definition' but switch to the other window."
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (slime-edit-definition name 'frame))

(defun slime-edit-definition-with-etags (name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (let ((xrefs (slime-etags-definitions name)))
    (cond (xrefs
           (message "Using tag file...")
           (slime-edit-definition-cont xrefs name nil))
          (t
           (error "No known definition for: %s" name)))))

(defun slime-etags-to-locations (name)
  "Search for definitions matching `name' in the currently active
tags table. Return a possibly empty list of slime-locations."
  (let ((locs '()))
    (save-excursion
      (let ((first-time t))
        (while (visit-tags-table-buffer (not first-time))
          (setq first-time nil)
          (goto-char (point-min))
          (while (search-forward name nil t)
            (beginning-of-line)
            (cl-destructuring-bind (hint line &rest pos) (etags-snarf-tag)
              (unless (eq hint t) ; hint==t if we are in a filename line
                (push `(:location (:file ,(expand-file-name (file-of-tag)))
                                  (:line ,line)
                                  (:snippet ,hint))
                      locs))))))
      (nreverse locs))))

(defun slime-etags-definitions (name)
  "Search definitions matching NAME in the tags file.
The result is a (possibly empty) list of definitions."
  (mapcar (lambda (loc)
            (make-slime-xref :dspec (cl-second (slime-location.hints loc))
                             :location loc))
          (slime-etags-to-locations name)))

;;;;; first-change-hook

(defun slime-first-change-hook ()
  "Notify Lisp that a source file's buffer has been modified."
  ;; Be careful not to disturb anything!
  ;; In particular if we muck up the match-data then query-replace
  ;; breaks. -luke (26/Jul/2004)
  (save-excursion
    (save-match-data
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (slime-background-activities-enabled-p))
        (let ((filename (slime-to-lisp-filename (buffer-file-name))))
          (slime-eval-async `(swank:buffer-first-change ,filename)))))))

(defun slime-setup-first-change-hook ()
  (add-hook (make-local-variable 'first-change-hook)
            'slime-first-change-hook))

(add-hook 'slime-mode-hook 'slime-setup-first-change-hook)


;;;; Eval for Lisp

(defun slime-lisp-readable-p (x)
  (or (stringp x)
      (memq x '(nil t))
      (integerp x)
      (keywordp x)
      (and (consp x)
           (let ((l x))
             (while (consp l)
               (slime-lisp-readable-p (car x))
               (setq l (cdr l)))
             (slime-lisp-readable-p l)))))

(defun slime--funcall-and-dispatch-result (thread tag fn &rest args)
  (let ((ok nil)
        (value nil)
        (error nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq value (apply fn args))
              (setq ok t))
          ((debug error)
           (setq error err)))
      (let ((result (cond ((and ok
                                (not (slime-lisp-readable-p value)))
                           `(:unreadable ,(slime-prin1-to-string value)))
                          (ok `(:ok ,value))
                          (error `(:error ,(symbol-name (car error))
                                          . ,(mapcar #'slime-prin1-to-string
                                                     (cdr error))))
                          (t `(:abort)))))
        (slime-dispatch-event `(:emacs-return ,thread ,tag ,result))))))

(defun slime-eval-for-lisp (thread tag form-string)
  (slime--funcall-and-dispatch-result thread tag
                                      (lambda (s) (eval (read s)))
                                      form-string))

(defun slime-check-eval-in-emacs-enabled ()
  "Raise an error if `slime-enable-evaluate-in-emacs' isn't true."
  (unless slime-enable-evaluate-in-emacs
    (error (concat "slime-eval-in-emacs disabled for security. "
                   "Set `slime-enable-evaluate-in-emacs' true to enable it."))))


;;;; RPC from Lisp

(defmacro defslimefun (name arglist &rest body)
  "Define a function via `cl-defun' that can be invoked from SWANK."
  `(progn
     (put ',name 'slime-rpc t)
     (cl-defun ,name ,arglist ,@body)))

(defun slime-rpc-allowed-p (fn)
  (get fn 'slime-rpc))

(defun slime-check-rpc-allowed (fn)
  "Raise an error if FN does not denote a function defined via
`defslimefun'."
  (unless (slime-rpc-allowed-p fn)
    (error "Lisp tried to RPC `%s', but it wasn't defined via `defslimefun'."
           fn)))

(defun slime-rpc-from-lisp (thread tag fn args)
  (if (not (slime-rpc-allowed-p fn))
      (slime-dispatch-event '(:ed-rpc-forbidden ,thread ,tag ,fn))
    (apply #'slime--funcall-and-dispatch-result thread tag fn args)))


;;;; `ED'

(defvar slime-ed-frame nil
  "The frame used by `slime-ed'.")

(defcustom slime-ed-use-dedicated-frame t
  "*When non-nil, `slime-ed' will create and reuse a dedicated frame."
  :type 'boolean
  :group 'slime-mode)

(defun slime-ed (what)
  "Edit WHAT.

WHAT can be:
  A filename (string),
  A list (:filename FILENAME &key LINE COLUMN POSITION),
  A function name (:function-name STRING)
  nil.

This is for use in the implementation of COMMON-LISP:ED."
  (when slime-ed-use-dedicated-frame
    (unless (and slime-ed-frame (frame-live-p slime-ed-frame))
      (setq slime-ed-frame (make-frame)))
    (select-frame slime-ed-frame))
  (when what
    (slime-dcase what
      ((:filename file &key line column position bytep)
       (find-file (slime-from-lisp-filename file))
       (when line (slime-goto-line line))
       (when column (move-to-column column))
       (when position
         (goto-char (if bytep
                        (byte-to-position position)
                      position))))
      ((:function-name name)
       (slime-edit-definition name)))))

(defun slime-goto-line (line-number)
  "Move to line LINE-NUMBER (1-based).
This is similar to `goto-line' but without pushing the mark and
the display stuff that we neither need nor want."
  (cl-assert (= (buffer-size) (- (point-max) (point-min))) ()
             "slime-goto-line in narrowed buffer")
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun slime-y-or-n-p (thread tag question)
  (slime-dispatch-event `(:emacs-return ,thread ,tag ,(y-or-n-p question))))

(defun slime-read-from-minibuffer-for-swank (thread tag prompt initial-value)
  (let ((answer (condition-case nil
                    (slime-read-from-minibuffer prompt initial-value)
                  (quit nil))))
    (slime-dispatch-event `(:emacs-return ,thread ,tag ,answer))))

;;;; Interactive evaluation.

(defun slime-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.

Note: If a prefix argument is in effect then the result will be
inserted in the current buffer."
  (interactive (list (slime-read-from-minibuffer "Slime Eval: ")))
  (cl-case current-prefix-arg
    ((nil)
     (slime-eval-with-transcript `(swank:interactive-eval ,string)))
    ((-)
     (slime-eval-save string))
    (t
     (slime-eval-print string))))

(defvar slime-transcript-start-hook nil
  "Hook run before start an evalution.")
(defvar slime-transcript-stop-hook nil
  "Hook run after finishing a evalution.")

(defun slime-display-eval-result (value)
  (slime-message "%s" value))

(defun slime-eval-with-transcript (form)
  "Eval FORM in Lisp.  Display output, if any."
  (run-hooks 'slime-transcript-start-hook)
  (slime-rex () (form)
    ((:ok value)
     (run-hooks 'slime-transcript-stop-hook)
     (slime-display-eval-result value))
    ((:abort condition)
     (run-hooks 'slime-transcript-stop-hook)
     (message "Evaluation aborted on %s." condition))))

(defun slime-eval-print (string)
  "Eval STRING in Lisp; insert any output and the result at point."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (push-mark)
        (insert output value)))))

(defun slime-eval-save (string)
  "Evaluate STRING in Lisp and save the result in the kill ring."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
    (lambda (result)
      (cl-destructuring-bind (output value) result
        (let ((string (concat output value)))
          (kill-new string)
          (message "Evaluation finished; pushed result to kill ring."))))))

(defun slime-eval-describe (form)
  "Evaluate FORM in Lisp and display the result in a new buffer."
  (slime-eval-async form (slime-rcurry #'slime-show-description
                                       (slime-current-package))))

(defvar slime-description-autofocus nil
  "If non-nil select description windows on display.")

(defun slime-show-description (string package)
  ;; So we can have one description buffer open per connection. Useful
  ;; for comparing the output of DISASSEMBLE across implementations.
  ;; FIXME: could easily be achieved with M-x rename-buffer
  (let ((bufname (slime-buffer-name :description)))
    (slime-with-popup-buffer (bufname :package package
                                      :connection t
                                      :select slime-description-autofocus)
      (princ string)
      (goto-char (point-min)))))

(defun slime-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun slime-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (slime-interactive-eval (slime-last-expression)))

(defun slime-eval-defun ()
  "Evaluate the current toplevel form.
Use `slime-re-evaluate-defvar' if the from starts with '(defvar'"
  (interactive)
  (let ((form (slime-defun-at-point)))
    (cond ((string-match "^(defvar " form)
           (slime-re-evaluate-defvar form))
          (t
           (slime-interactive-eval form)))))

(defun slime-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (slime-eval-with-transcript
   `(swank:interactive-eval-region
     ,(buffer-substring-no-properties start end))))

(defun slime-pprint-eval-region (start end)
  "Evaluate region; pprint the value in a buffer."
  (interactive "r")
  (slime-eval-describe
   `(swank:pprint-eval
     ,(buffer-substring-no-properties start end))))

(defun slime-eval-buffer ()
  "Evaluate the current buffer.
The value is printed in the echo area."
  (interactive)
  (slime-eval-region (point-min) (point-max)))

(defun slime-re-evaluate-defvar (form)
  "Force the re-evaluaton of the defvar form before point.

First make the variable unbound, then evaluate the entire form."
  (interactive (list (slime-last-expression)))
  (slime-eval-with-transcript `(swank:re-evaluate-defvar ,form)))

(defun slime-pprint-eval-last-expression ()
  "Evaluate the form before point; pprint the value in a buffer."
  (interactive)
  (slime-eval-describe `(swank:pprint-eval ,(slime-last-expression))))

(defun slime-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer"
  (interactive (list (slime-last-expression)))
  (insert "\n")
  (slime-eval-print string))

;;;; Edit Lisp value
;;;
(defun slime-edit-value (form-string)
  "\\<slime-edit-value-mode-map>\
Edit the value of a setf'able form in a new buffer.
The value is inserted into a temporary buffer for editing and then set
in Lisp when committed with \\[slime-edit-value-commit]."
  (interactive
   (list (slime-read-from-minibuffer "Edit value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:value-for-editing ,form-string)
    (lexical-let ((form-string form-string)
                  (package (slime-current-package)))
      (lambda (result)
        (slime-edit-value-callback form-string result
                                   package)))))

(make-variable-buffer-local
 (defvar slime-edit-form-string nil
   "The form being edited by `slime-edit-value'."))

(define-minor-mode slime-edit-value-mode
  "Mode for editing a Lisp value."
  nil
  " Edit-Value"
  '(("\C-c\C-c" . slime-edit-value-commit)))

(defun slime-edit-value-callback (form-string current-value package)
  (let* ((name (generate-new-buffer-name (format "*Edit %s*" form-string)))
         (buffer (slime-with-popup-buffer (name :package package
                                                :connection t
                                                :select t
                                                :mode 'lisp-mode)
                   (slime-popup-buffer-mode -1) ; don't want binding of 'q'
                   (slime-mode 1)
                   (slime-edit-value-mode 1)
                   (setq slime-edit-form-string form-string)
                   (insert current-value)
                   (current-buffer))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (message "Type C-c C-c when done"))))

(defun slime-edit-value-commit ()
  "Commit the edited value to the Lisp image.
\\(See `slime-edit-value'.)"
  (interactive)
  (if (null slime-edit-form-string)
      (error "Not editing a value.")
    (let ((value (buffer-substring-no-properties (point-min) (point-max))))
      (lexical-let ((buffer (current-buffer)))
        (slime-eval-async `(swank:commit-edited-value ,slime-edit-form-string
                                                      ,value)
          (lambda (_)
            (with-current-buffer buffer
              (quit-window t))))))))

;;;; Tracing

(defun slime-untrace-all ()
  "Untrace all functions."
  (interactive)
  (slime-eval `(swank:untrace-all)))

(defun slime-toggle-trace-fdefinition (spec)
  "Toggle trace."
  (interactive (list (slime-read-from-minibuffer
                      "(Un)trace: " (slime-symbol-at-point))))
  (message "%s" (slime-eval `(swank:swank-toggle-trace ,spec))))



(defun slime-disassemble-symbol (symbol-name)
  "Display the disassembly for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Disassemble: ")))
  (slime-eval-describe `(swank:disassemble-form ,(concat "'" symbol-name))))

(defun slime-undefine-function (symbol-name)
  "Unbind the function slot of SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "fmakunbound: " t)))
  (slime-eval-async `(swank:undefine-function ,symbol-name)
    (lambda (result) (message "%s" result))))

(defun slime-unintern-symbol (symbol-name package)
  "Unintern the symbol given with SYMBOL-NAME PACKAGE."
  (interactive (list (slime-read-symbol-name "Unintern symbol: " t)
                     (slime-read-package-name "from package: "
                                              (slime-current-package))))
  (slime-eval-async `(swank:unintern-symbol ,symbol-name ,package)
    (lambda (result) (message "%s" result))))

(defun slime-delete-package (package-name)
  "Delete the package with name PACKAGE-NAME."
  (interactive (list (slime-read-package-name "Delete package: "
                                              (slime-current-package))))
  (slime-eval-async `(cl:delete-package
                      (swank::guess-package ,package-name))))

(defun slime-load-file (filename)
  "Load the Lisp file FILENAME."
  (interactive (list
		(read-file-name "Load file: " nil nil
				nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (let ((lisp-filename (slime-to-lisp-filename (expand-file-name filename))))
    (slime-eval-with-transcript `(swank:load-file ,lisp-filename))))

(defvar slime-change-directory-hooks nil
  "Hook run by `slime-change-directory'.
The functions are called with the new (absolute) directory.")

(defun slime-change-directory (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (let ((dir (expand-file-name directory)))
    (prog1 (slime-eval `(swank:set-default-directory
                         ,(slime-to-lisp-filename dir)))
      (slime-with-connection-buffer nil (cd-absolute dir))
      (run-hook-with-args 'slime-change-directory-hooks dir))))

(defun slime-cd (directory)
  "Make DIRECTORY become Lisp's current directory.
Return whatever swank:set-default-directory returns."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (message "default-directory: %s" (slime-change-directory directory)))

(defun slime-pwd ()
  "Show Lisp's default directory."
  (interactive)
  (message "Directory %s" (slime-eval `(swank:default-directory))))


;;;; Profiling

(defun slime-toggle-profile-fdefinition (fname-string)
  "Toggle profiling for FNAME-STRING."
  (interactive (list (slime-read-from-minibuffer
                      "(Un)Profile: "
                      (slime-symbol-at-point))))
  (slime-eval-async `(swank:toggle-profile-fdefinition ,fname-string)
    (lambda (r) (message "%s" r))))

(defun slime-unprofile-all ()
  "Unprofile all functions."
  (interactive)
  (slime-eval-async '(swank:unprofile-all)
    (lambda (r) (message "%s" r))))

(defun slime-profile-report ()
  "Print profile report."
  (interactive)
  (slime-eval-with-transcript '(swank:profile-report)))

(defun slime-profile-reset ()
  "Reset profile counters."
  (interactive)
  (slime-eval-async (slime-eval `(swank:profile-reset))
    (lambda (r) (message "%s" r))))

(defun slime-profiled-functions ()
  "Return list of names of currently profiled functions."
  (interactive)
  (slime-eval-async `(swank:profiled-functions)
    (lambda (r) (message "%s" r))))

(defun slime-profile-package (package callers methods)
  "Profile all functions in PACKAGE.
If CALLER is non-nil names have counts of the most common calling
functions recorded.
If METHODS is non-nil, profile all methods of all generic function
having names in the given package."
  (interactive (list (slime-read-package-name "Package: ")
                     (y-or-n-p "Record the most common callers? ")
                     (y-or-n-p "Profile methods? ")))
  (slime-eval-async `(swank:swank-profile-package ,package ,callers ,methods)
    (lambda (r) (message "%s" r))))

(defun slime-profile-by-substring (substring &optional package)
  "Profile all functions which names contain SUBSTRING.
If PACKAGE is NIL, then search in all packages."
  (interactive (list
                (slime-read-from-minibuffer
                 "Profile by matching substring: "
                 (slime-symbol-at-point))
                (slime-read-package-name "Package (RET for all packages): ")))
  (let ((package (unless (equal package "") package)))
    (slime-eval-async `(swank:profile-by-substring ,substring ,package)
      (lambda (r) (message "%s" r)) )))

;;;; Documentation

(defvar slime-documentation-lookup-function
  'slime-hyperspec-lookup)

(defun slime-documentation-lookup ()
  "Generalized documentation lookup. Defaults to hyperspec lookup."
  (interactive)
  (call-interactively slime-documentation-lookup-function))

(defun slime-hyperspec-lookup (symbol-name)
  "A wrapper for `hyperspec-lookup'"
  (interactive (list (common-lisp-hyperspec-read-symbol-name
                      (slime-symbol-at-point))))
  (hyperspec-lookup symbol-name))

(defun slime-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-symbol ,symbol-name)))

(defun slime-documentation (symbol-name)
  "Display function- or symbol-documentation for SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Documentation for symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe
   `(swank:documentation-symbol ,symbol-name)))

(defun slime-describe-function (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol's function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (slime-eval-describe `(swank:describe-function ,symbol-name)))

(defface slime-apropos-symbol
  '((t (:inherit bold)))
  "Face for the symbol name in Apropos output."
  :group 'slime)

(defface slime-apropos-label
  '((t (:inherit italic)))
  "Face for label (`Function', `Variable' ...) in Apropos output."
  :group 'slime)

(defun slime-apropos-summary (string case-sensitive-p package only-external-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          "Apropos for "
          (format "%S" string)
          (if package (format " in package %S" package) "")
          (if only-external-p " (external symbols only)" "")))

(defun slime-apropos (string &optional only-external-p package
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLIME Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slime-current-package))))
    (slime-eval-async
        `(swank:apropos-list-for-emacs ,string ,only-external-p
                                       ,case-sensitive-p ',package)
      (slime-rcurry #'slime-show-apropos string buffer-package
                    (slime-apropos-summary string case-sensitive-p
                                           package only-external-p)))))

(defun slime-apropos-all ()
  "Shortcut for (slime-apropos <string> nil nil)"
  (interactive)
  (slime-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (slime-apropos "" (not internal) package))

(autoload 'apropos-mode "apropos")
(defun slime-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slime-with-popup-buffer ((slime-buffer-name :apropos)
                              :package package :connection t
                              :mode 'apropos-mode)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (slime-set-truncate-lines)
      (slime-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(defvar slime-apropos-namespaces
  '((:variable "Variable")
    (:function "Function")
    (:generic-function "Generic Function")
    (:macro "Macro")
    (:special-operator "Special Operator")
    (:setf "Setf")
    (:type "Type")
    (:class "Class")
    (:alien-type "Alien type")
    (:alien-struct "Alien struct")
    (:alien-union "Alien type")
    (:alien-enum "Alien enum")))

(defun slime-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (cl-assert designator)
      (slime-insert-propertized `(face slime-apropos-symbol) designator))
    (terpri)
    (cl-loop for (prop value) on plist by #'cddr
             unless (eq prop :designator) do
             (let ((namespace (cadr (or (assq prop slime-apropos-namespaces)
                                        (error "Unknown property: %S" prop))))
                   (start (point)))
               (princ "  ")
               (slime-insert-propertized `(face slime-apropos-label) namespace)
               (princ ": ")
               (princ (cl-etypecase value
                        (string value)
                        ((member nil :not-documented) "(not documented)")))
               (add-text-properties
                start (point)
                (list 'type prop 'action 'slime-call-describer
                      'button t 'apropos-label namespace
                      'item (plist-get plist :designator)))
               (terpri)))))

(defun slime-call-describer (arg)
  (let* ((pos (if (markerp arg) arg (point)))
         (type (get-text-property pos 'type))
         (item (get-text-property pos 'item)))
    (slime-eval-describe `(swank:describe-definition-for-emacs ,item ,type))))

(defun slime-info ()
  "Open Slime manual"
  (interactive)
  (let ((file (expand-file-name "doc/slime.info" slime-path)))
    (if (file-exists-p file)
        (info file)
      (message "No slime.info, run `make slime.info' in %s"
               (expand-file-name "doc/" slime-path)))))


;;;; XREF: cross-referencing

(defvar slime-xref-mode-map)

(define-derived-mode slime-xref-mode lisp-mode "Xref"
  "slime-xref-mode: Major mode for cross-referencing.
\\<slime-xref-mode-map>\
The most important commands:
\\[slime-xref-quit]	- Dismiss buffer.
\\[slime-show-xref]	- Display referenced source and keep xref window.
\\[slime-goto-xref]	- Jump to referenced source and dismiss xref window.

\\{slime-xref-mode-map}
\\{slime-popup-buffer-mode-map}
"
  (slime-popup-buffer-mode)
  (setq font-lock-defaults nil)
  (setq delayed-mode-hooks nil)
  (slime-mode -1))

(slime-define-keys slime-xref-mode-map
  ((kbd "RET") 'slime-goto-xref)
  ((kbd "SPC") 'slime-goto-xref)
  ("v" 'slime-show-xref)
  ("n" 'slime-xref-next-line)
  ("p" 'slime-xref-prev-line)
  ("." 'slime-xref-next-line)
  ("," 'slime-xref-prev-line)
  ("\C-c\C-c" 'slime-recompile-xref)
  ("\C-c\C-k" 'slime-recompile-all-xrefs)
  ("\M-," 'slime-xref-retract)
  ([remap next-line] 'slime-xref-next-line)
  ([remap previous-line] 'slime-xref-prev-line)
  )


;;;;; XREF results buffer and window management

(cl-defmacro slime-with-xref-buffer ((_xref-type _symbol &optional package)
                                     &body body)
  "Execute BODY in a xref buffer, then show that buffer."
  (declare (indent 1))
  `(slime-with-popup-buffer ((slime-buffer-name :xref)
                             :package ,package
                             :connection t
                             :select t
                             :mode 'slime-xref-mode)
     (slime-set-truncate-lines)
     ,@body))

(defun slime-insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . ((LABEL LOCATION) ...)) ...).
GROUP and LABEL are for decoration purposes.  LOCATION is a
source-location."
  (cl-loop for (group . refs) in xref-alist do
           (slime-insert-propertized '(face bold) group "\n")
           (cl-loop for (label location) in refs do
                    (slime-insert-propertized
                     (list 'slime-location location
                           'face 'font-lock-keyword-face)
                     "  " (slime-one-line-ify label) "\n")))
  ;; Remove the final newline to prevent accidental window-scrolling
  (backward-delete-char 1))

(defun slime-xref-next-line ()
  (interactive)
  (slime-xref-show-location (slime-search-property 'slime-location)))

(defun slime-xref-prev-line ()
  (interactive)
  (slime-xref-show-location (slime-search-property 'slime-location t)))

(defun slime-xref-show-location (loc)
  (cl-ecase (car loc)
    (:location (slime-show-source-location loc nil 1))
    (:error (message "%s" (cadr loc)))
    ((nil))))

(defvar slime-next-location-function nil
  "Function to call for going to the next location.")

(defvar slime-previous-location-function nil
  "Function to call for going to the previous location.")

(defvar slime-xref-last-buffer nil
  "The most recent XREF results buffer.
This is used by `slime-goto-next-xref'")

(defun slime-show-xref-buffer (xrefs _type _symbol package)
  (slime-with-xref-buffer (_type _symbol package)
    (slime-insert-xrefs xrefs)
    (setq slime-next-location-function 'slime-goto-next-xref)
    (setq slime-previous-location-function 'slime-goto-previous-xref)
    (setq slime-xref-last-buffer (current-buffer))
    (goto-char (point-min))))

(defun slime-show-xrefs (xrefs type symbol package)
  "Show the results of an XREF query."
  (if (null xrefs)
      (message "No references found for %s." symbol)
    (slime-show-xref-buffer xrefs type symbol package)))


;;;;; XREF commands

(defun slime-who-calls (symbol)
  "Show all known callers of the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls symbol))

(defun slime-calls-who (symbol)
  "Show all known functions called by the function SYMBOL."
  (interactive (list (slime-read-symbol-name "Who calls: " t)))
  (slime-xref :calls-who symbol))

(defun slime-who-references (symbol)
  "Show all known referrers of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who references: " t)))
  (slime-xref :references symbol))

(defun slime-who-binds (symbol)
  "Show all known binders of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who binds: " t)))
  (slime-xref :binds symbol))

(defun slime-who-sets (symbol)
  "Show all known setters of the global variable SYMBOL."
  (interactive (list (slime-read-symbol-name "Who sets: " t)))
  (slime-xref :sets symbol))

(defun slime-who-macroexpands (symbol)
  "Show all known expanders of the macro SYMBOL."
  (interactive (list (slime-read-symbol-name "Who macroexpands: " t)))
  (slime-xref :macroexpands symbol))

(defun slime-who-specializes (symbol)
  "Show all known methods specialized on class SYMBOL."
  (interactive (list (slime-read-symbol-name "Who specializes: " t)))
  (slime-xref :specializes symbol))

(defun slime-list-callers (symbol-name)
  "List the callers of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callers: ")))
  (slime-xref :callers symbol-name))

(defun slime-list-callees (symbol-name)
  "List the callees of SYMBOL-NAME in a xref window."
  (interactive (list (slime-read-symbol-name "List callees: ")))
  (slime-xref :callees symbol-name))

;; FIXME: whats the call (slime-postprocess-xrefs result) good for?
(defun slime-xref (type symbol &optional continuation)
  "Make an XREF request to Lisp."
  (slime-eval-async
      `(swank:xref ',type ',symbol)
    (slime-rcurry (lambda (result type symbol package cont)
                    (slime-check-xref-implemented type result)
                    (let* ((_xrefs (slime-postprocess-xrefs result))
                           (file-alist (cadr (slime-analyze-xrefs result))))
                      (funcall (or cont 'slime-show-xrefs)
                               file-alist type symbol package)))
                  type
                  symbol
                  (slime-current-package)
                  continuation)))

(defun slime-check-xref-implemented (type xrefs)
  (when (eq xrefs :not-implemented)
    (error "%s is not implemented yet on %s."
           (slime-xref-type type)
           (slime-lisp-implementation-name))))

(defun slime-xref-type (type)
  (format "who-%s" (slime-cl-symbol-name type)))

(defun slime-xrefs (types symbol &optional continuation)
  "Make multiple XREF requests at once."
  (slime-eval-async
      `(swank:xrefs ',types ',symbol)
    #'(lambda (result)
        (funcall (or continuation
                     #'slime-show-xrefs)
                 (cl-loop for (key . val) in result
                          collect (cons (slime-xref-type key) val))
                 types symbol (slime-current-package)))))


;;;;; XREF navigation

(defun slime-xref-location-at-point ()
  (save-excursion
    ;; When the end of the last line is at (point-max) we can't find
    ;; the text property there. Going to bol avoids this problem.
    (beginning-of-line 1)
    (or (get-text-property (point) 'slime-location)
        (error "No reference at point."))))

(defun slime-xref-dspec-at-point ()
  (save-excursion
    (beginning-of-line 1)
    (with-syntax-table lisp-mode-syntax-table
      (forward-sexp)                    ; skip initial whitespaces
      (backward-sexp)
      (slime-sexp-at-point))))

(defun slime-all-xrefs ()
  (let ((xrefs nil))
    (save-excursion
     (goto-char (point-min))
     (while (zerop (forward-line 1))
       (let ((loc (get-text-property (point) 'slime-location)))
         (when loc
           (let* ((dspec (slime-xref-dspec-at-point))
                  (xref  (make-slime-xref :dspec dspec :location loc)))
             (push xref xrefs))))))
    (nreverse xrefs)))

(defun slime-goto-xref ()
  "Goto the cross-referenced location at point."
  (interactive)
  (slime-show-xref)
  (quit-window))

(defun slime-show-xref ()
  "Display the xref at point in the other window."
  (interactive)
  (let ((location (slime-xref-location-at-point)))
    (slime-show-source-location location t 1)))

(defun slime-goto-next-xref (&optional backward)
  "Goto the next cross-reference location."
  (if (not (buffer-live-p slime-xref-last-buffer))
      (error "No XREF buffer alive.")
    (cl-destructuring-bind (location pos)
        (with-current-buffer slime-xref-last-buffer
          (list (slime-search-property 'slime-location backward)
                (point)))
      (cond ((slime-location-p location)
             (slime-pop-to-location location)
             ;; We do this here because changing the location can take
             ;; a while when Emacs needs to read a file from disk.
             (with-current-buffer slime-xref-last-buffer
               (goto-char pos)
               (slime-highlight-line 0.35)))
            ((null location)
             (message (if backward "No previous xref" "No next xref.")))
            (t ; error location
             (slime-goto-next-xref backward))))))

(defun slime-goto-previous-xref ()
  "Goto the previous cross-reference location."
  (slime-goto-next-xref t))

(defun slime-search-property (prop &optional backward prop-value-fn)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward.
If PROP-VALUE-FN is non-nil use it to extract PROP's value."
  (let ((next-candidate (if backward
                            #'previous-single-char-property-change
                          #'next-single-char-property-change))
        (prop-value-fn  (or prop-value-fn
                            (lambda ()
                              (get-text-property (point) prop))))
        (start (point))
        (prop-value))
    (while (progn
             (goto-char (funcall next-candidate (point) prop))
             (not (or (setq prop-value (funcall prop-value-fn))
                      (eobp)
                      (bobp)))))
    (cond (prop-value)
          (t (goto-char start) nil))))

(defun slime-next-location ()
  "Go to the next location, depending on context.
When displaying XREF information, this goes to the next reference."
  (interactive)
  (when (null slime-next-location-function)
    (error "No context for finding locations."))
  (funcall slime-next-location-function))

(defun slime-previous-location ()
  "Go to the previous location, depending on context.
When displaying XREF information, this goes to the previous reference."
  (interactive)
  (when (null slime-previous-location-function)
    (error "No context for finding locations."))
  (funcall slime-previous-location-function))

(defun slime-recompile-xref (&optional raw-prefix-arg)
  (interactive "P")
  (let ((slime-compilation-policy (slime-compute-policy raw-prefix-arg)))
    (let ((location (slime-xref-location-at-point))
          (dspec    (slime-xref-dspec-at-point)))
      (slime-recompile-locations
       (list location)
       (slime-rcurry #'slime-xref-recompilation-cont
                     (list dspec) (current-buffer))))))

(defun slime-recompile-all-xrefs (&optional raw-prefix-arg)
  (interactive "P")
  (let ((slime-compilation-policy (slime-compute-policy raw-prefix-arg)))
    (let ((dspecs) (locations))
      (dolist (xref (slime-all-xrefs))
        (when (slime-xref-has-location-p xref)
          (push (slime-xref.dspec xref) dspecs)
          (push (slime-xref.location xref) locations)))
      (slime-recompile-locations
       locations
       (slime-rcurry #'slime-xref-recompilation-cont
                     dspecs (current-buffer))))))

(defun slime-xref-recompilation-cont (results dspecs buffer)
  ;; Extreme long-windedness to insert status of recompilation;
  ;; sometimes Elisp resembles more of an Ewwlisp.

  ;; FIXME: Should probably throw out the whole recompilation cruft
  ;; anyway.  -- helmut
  ;; TODO: next iteration of fixme cleanup this is going in a contrib -- jt
  (with-current-buffer buffer
    (slime-compilation-finished (slime-aggregate-compilation-results results))
    (save-excursion
      (slime-xref-insert-recompilation-flags
       dspecs (cl-loop for r in results collect
                       (or (slime-compilation-result.successp r)
                           (and (slime-compilation-result.notes r)
                                :complained)))))))

(defun slime-aggregate-compilation-results (results)
  `(:compilation-result
    ,(cl-reduce #'append (mapcar #'slime-compilation-result.notes results))
    ,(cl-every #'slime-compilation-result.successp results)
    ,(cl-reduce #'+ (mapcar #'slime-compilation-result.duration results))))

(defun slime-xref-insert-recompilation-flags (dspecs compilation-results)
  (let* ((buffer-read-only nil)
         (max-column (slime-column-max)))
    (goto-char (point-min))
    (cl-loop for dspec in dspecs
             for result in compilation-results
             do (save-excursion
                  (cl-loop for dspec2 = (progn (search-forward dspec)
                                               (slime-xref-dspec-at-point))
                           until (equal dspec2 dspec))
                  (end-of-line) ; skip old status information.
                  (insert-char ?\  (1+ (- max-column (current-column))))
                  (insert (format "[%s]"
                                  (cl-case result
                                    ((t)   :success)
                                    ((nil) :failure)
                                    (t     result))))))))


;;;; Macroexpansion

(define-minor-mode slime-macroexpansion-minor-mode
  "SLIME mode for macroexpansion"
  nil
  " Macroexpand"
  '(("g" . slime-macroexpand-again)))

(cl-macrolet ((remap (from to)
                     `(dolist (mapping
                               (where-is-internal ,from slime-mode-map))
                        (define-key slime-macroexpansion-minor-mode-map
                                    mapping ,to))))
  (remap 'slime-macroexpand-1 'slime-macroexpand-1-inplace)
  (remap 'slime-macroexpand-all 'slime-macroexpand-all-inplace)
  (remap 'slime-compiler-macroexpand-1 'slime-compiler-macroexpand-1-inplace)
  (remap 'slime-expand-1
         'slime-expand-1-inplace)
  (remap 'advertised-undo 'slime-macroexpand-undo)
  (remap 'undo 'slime-macroexpand-undo))

(defun slime-macroexpand-undo (&optional arg)
  (interactive)
  ;; Emacs 22.x introduced `undo-only' which
  ;; works by binding `undo-no-redo' to t. We do
  ;; it this way so we don't break prior Emacs
  ;; versions.
  (cl-macrolet ((undo-only (arg) `(let ((undo-no-redo t)) (undo ,arg))))
    (let ((inhibit-read-only t))
      (when (fboundp 'slime-remove-edits)
        (slime-remove-edits (point-min) (point-max)))
      (undo-only arg))))

(defvar slime-eval-macroexpand-expression nil
  "Specifies the last macroexpansion preformed.
This variable specifies both what was expanded and how.")

(defun slime-eval-macroexpand (expander &optional string)
  (let ((string (or string (slime-sexp-at-point-or-error))))
    (setq slime-eval-macroexpand-expression `(,expander ,string))
    (slime-eval-async slime-eval-macroexpand-expression
      #'slime-initialize-macroexpansion-buffer)))

(defun slime-macroexpand-again ()
  "Reperform the last macroexpansion."
  (interactive)
  (slime-eval-async slime-eval-macroexpand-expression
    (slime-rcurry #'slime-initialize-macroexpansion-buffer
                  (current-buffer))))

(defun slime-initialize-macroexpansion-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (slime-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                                        ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun slime-create-macroexpansion-buffer ()
  (let ((name (slime-buffer-name :macroexpansion)))
    (slime-with-popup-buffer (name :package t :connection t
                                   :mode 'lisp-mode)
      (slime-mode 1)
      (slime-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun slime-eval-macroexpand-inplace (expander)
  "Substitute the sexp at point with its macroexpansion.

NB: Does not affect slime-eval-macroexpand-expression"
  (interactive)
  (let* ((bounds (or (slime-bounds-of-sexp-at-point)
                     (user-error "No sexp at point"))))
    (lexical-let* ((start (copy-marker (car bounds)))
                   (end (copy-marker (cdr bounds)))
                   (point (point))
                   (package (slime-current-package))
                   (buffer (current-buffer)))
      (slime-eval-async
          `(,expander ,(buffer-substring-no-properties start end))
        (lambda (expansion)
          (with-current-buffer buffer
            (let ((buffer-read-only nil))
              (when (fboundp 'slime-remove-edits)
                (slime-remove-edits (point-min) (point-max)))
              (goto-char start)
              (delete-region start end)
              (slime-insert-indented expansion)
              (goto-char point))))))))

(defun slime-macroexpand-1 (&optional repeatedly)
  "Display the macro expansion of the form starting at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-1-inplace (&optional repeatedly)
  (interactive "P")
  (slime-eval-macroexpand-inplace
   (if repeatedly 'swank:swank-macroexpand 'swank:swank-macroexpand-1)))

(defun slime-macroexpand-all ()
  "Display the recursively macro expanded sexp starting at
point."
  (interactive)
  (slime-eval-macroexpand 'swank:swank-macroexpand-all))

(defun slime-macroexpand-all-inplace ()
  "Display the recursively macro expanded sexp starting at point."
  (interactive)
  (slime-eval-macroexpand-inplace 'swank:swank-macroexpand-all))

(defun slime-compiler-macroexpand-1 (&optional repeatedly)
  "Display the compiler-macro expansion of sexp starting at point."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly
       'swank:swank-compiler-macroexpand
     'swank:swank-compiler-macroexpand-1)))

(defun slime-compiler-macroexpand-1-inplace (&optional repeatedly)
  "Display the compiler-macro expansion of sexp starting at point."
  (interactive "P")
  (slime-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-compiler-macroexpand
     'swank:swank-compiler-macroexpand-1)))

(defun slime-expand-1 (&optional repeatedly)
  "Display the macro expansion of the form starting at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND.  If the form denotes a
compiler macro, SWANK/BACKEND:COMPILER-MACROEXPAND or
SWANK/BACKEND:COMPILER-MACROEXPAND-1 are used instead."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly
       'swank:swank-expand
     'swank:swank-expand-1)))

(defun slime-expand-1-inplace (&optional repeatedly)
  "Display the macro expansion of the form at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND."
  (interactive "P")
  (slime-eval-macroexpand-inplace
   (if repeatedly
       'swank:swank-expand
     'swank:swank-expand-1)))

(defun slime-format-string-expand (&optional string)
  "Expand the format-string at point and display it."
  (interactive (list (or (and (not current-prefix-arg)
                              (slime-string-at-point))
                         (slime-read-from-minibuffer "Expand format: "
                                                     (slime-string-at-point)))))
  (slime-eval-macroexpand 'swank:swank-format-string-expand string))


;;;; Subprocess control

(defun slime-interrupt ()
  "Interrupt Lisp."
  (interactive)
  (cond ((slime-use-sigint-for-interrupt) (slime-send-sigint))
        (t (slime-dispatch-event `(:emacs-interrupt ,slime-current-thread)))))

(defun slime-quit ()
  (error "Not implemented properly.  Use `slime-interrupt' instead."))

(defun slime-quit-lisp (&optional kill)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive "P")
  (slime-quit-lisp-internal (slime-connection) 'slime-quit-sentinel kill))

(defun slime-quit-lisp-internal (connection sentinel kill)
  (let ((slime-dispatching-connection connection))
    (slime-eval-async '(swank:quit-lisp))
    (let* ((process (slime-inferior-process connection)))
      (set-process-filter connection  nil)
      (set-process-sentinel connection sentinel)
      (when (and kill process)
        (sleep-for 0.2)
        (unless (memq (process-status process) '(exit signal))
          (kill-process process))))))

(defun slime-quit-sentinel (process _message)
  (cl-assert (process-status process) 'closed)
  (let* ((inferior (slime-inferior-process process))
         (inferior-buffer (if inferior (process-buffer inferior))))
    (when inferior (delete-process inferior))
    (when inferior-buffer (kill-buffer inferior-buffer))
    (slime-net-close process)
    (message "Connection closed.")))


;;;; Debugger (SLDB)

(defvar sldb-hook nil
  "Hook run on entry to the debugger.")

(defcustom sldb-initial-restart-limit 6
  "Maximum number of restarts to display initially."
  :group 'slime-debugger
  :type 'integer)


;;;;; Local variables in the debugger buffer

;; Small helper.
(defun slime-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(slime-make-variables-buffer-local
 (defvar sldb-condition nil
   "A list (DESCRIPTION TYPE) describing the condition being debugged.")

 (defvar sldb-restarts nil
   "List of (NAME DESCRIPTION) for each available restart.")

 (defvar sldb-level nil
   "Current debug level (recursion depth) displayed in buffer.")

 (defvar sldb-backtrace-start-marker nil
   "Marker placed at the first frame of the backtrace.")

 (defvar sldb-restart-list-start-marker nil
   "Marker placed at the first restart in the restart list.")

 (defvar sldb-continuations nil
   "List of ids for pending continuation."))

;;;;; SLDB macros

;; some macros that we need to define before the first use

(defmacro sldb-in-face (name string)
  "Return STRING propertised with face sldb-NAME-face."
  (declare (indent 1))
  (let ((facename (intern (format "sldb-%s-face" (symbol-name name))))
	(var (cl-gensym "string")))
    `(let ((,var ,string))
       (slime-add-face ',facename ,var)
       ,var)))


;;;;; sldb-mode

(defvar sldb-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This enables autodoc-mode to match
    ;; #<unreadable> actual arguments in the backtraces with formal
    ;; arguments of the function.  (For Lisp mode, this is not
    ;; desirable, since we do not wish to get a mismatched paren
    ;; highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(" table)
    (modify-syntax-entry ?> ")" table)
    table)
  "Syntax table for SLDB mode.")

(define-derived-mode sldb-mode fundamental-mode "sldb"
  "Superior lisp debugger mode.
In addition to ordinary SLIME commands, the following are
available:\\<sldb-mode-map>

Commands to examine the selected frame:
   \\[sldb-toggle-details]   - toggle details (local bindings, CATCH tags)
   \\[sldb-show-source]   - view source for the frame
   \\[sldb-eval-in-frame]   - eval in frame
   \\[sldb-pprint-eval-in-frame]   - eval in frame, pretty-print result
   \\[sldb-disassemble]   - disassemble
   \\[sldb-inspect-in-frame]   - inspect

Commands to invoke restarts:
   \\[sldb-quit]   - quit
   \\[sldb-abort]   - abort
   \\[sldb-continue]   - continue
   \\[sldb-invoke-restart-0]-\\[sldb-invoke-restart-9] - restart shortcuts
   \\[sldb-invoke-restart-by-name]   - invoke restart by name

Commands to navigate frames:
   \\[sldb-down]   - down
   \\[sldb-up]   - up
   \\[sldb-details-down] - down, with details
   \\[sldb-details-up] - up, with details
   \\[sldb-cycle] - cycle between restarts & backtrace
   \\[sldb-beginning-of-backtrace]   - beginning of backtrace
   \\[sldb-end-of-backtrace]   - end of backtrace

Miscellaneous commands:
   \\[sldb-restart-frame]   - restart frame
   \\[sldb-return-from-frame]   - return from frame
   \\[sldb-step]   - step
   \\[sldb-break-with-default-debugger]   - switch to native debugger
   \\[sldb-break-with-system-debugger]   - switch to system debugger (gdb)
   \\[slime-interactive-eval]   - eval
   \\[sldb-inspect-condition]   - inspect signalled condition

Full list of commands:

\\{sldb-mode-map}"
  (erase-buffer)
  (set-syntax-table sldb-mode-syntax-table)
  (slime-set-truncate-lines)
  ;; Make original slime-connection "sticky" for SLDB commands in this buffer
  (setq slime-buffer-connection (slime-connection)))

(set-keymap-parent sldb-mode-map slime-parent-map)

(slime-define-keys sldb-mode-map

  ((kbd "RET") 'sldb-default-action)
  ("\C-m"      'sldb-default-action)
  ([return] 'sldb-default-action)
  ([mouse-2]  'sldb-default-action/mouse)
  ([follow-link] 'mouse-face)
  ("\C-i" 'sldb-cycle)
  ("h"    'describe-mode)
  ("v"    'sldb-show-source)
  ("e"    'sldb-eval-in-frame)
  ("d"    'sldb-pprint-eval-in-frame)
  ("D"    'sldb-disassemble)
  ("i"    'sldb-inspect-in-frame)
  ("n"    'sldb-down)
  ("p"    'sldb-up)
  ("\M-n" 'sldb-details-down)
  ("\M-p" 'sldb-details-up)
  ("<"    'sldb-beginning-of-backtrace)
  (">"    'sldb-end-of-backtrace)
  ("t"    'sldb-toggle-details)
  ("r"    'sldb-restart-frame)
  ("I"    'sldb-invoke-restart-by-name)
  ("R"    'sldb-return-from-frame)
  ("c"    'sldb-continue)
  ("s"    'sldb-step)
  ("x"    'sldb-next)
  ("o"    'sldb-out)
  ("b"    'sldb-break-on-return)
  ("a"    'sldb-abort)
  ("q"    'sldb-quit)
  ("A"    'sldb-break-with-system-debugger)
  ("B"    'sldb-break-with-default-debugger)
  ("P"    'sldb-print-condition)
  ("C"    'sldb-inspect-condition)
  (":"    'slime-interactive-eval)
  ("\C-c\C-c" 'sldb-recompile-frame-source))

;; Keys 0-9 are shortcuts to invoke particular restarts.
(dotimes (number 10)
  (let ((fname (intern (format "sldb-invoke-restart-%S" number)))
        (docstring (format "Invoke restart numbered %S." number)))
    (eval `(defun ,fname ()
             ,docstring
             (interactive)
             (sldb-invoke-restart ,number)))
    (define-key sldb-mode-map (number-to-string number) fname)))


;;;;; SLDB buffer creation & update

(defun sldb-buffers (&optional connection)
  "Return a list of all sldb buffers (belonging to CONNECTION.)"
  (if connection
      (slime-filter-buffers (lambda ()
                              (and (eq slime-buffer-connection connection)
                                   (eq major-mode 'sldb-mode))))
    (slime-filter-buffers (lambda () (eq major-mode 'sldb-mode)))))

(defun sldb-find-buffer (thread &optional connection)
  (let ((connection (or connection (slime-connection))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (eq slime-buffer-connection connection)
                         (eq slime-current-thread thread))))
                (sldb-buffers))))

(defun sldb-get-default-buffer ()
  "Get a sldb buffer.
The chosen buffer the default connection's it if exists."
  (car (sldb-buffers slime-default-connection)))

(defun sldb-get-buffer (thread &optional connection)
  "Find or create a sldb-buffer for THREAD."
  (let ((connection (or connection (slime-connection))))
    (or (sldb-find-buffer thread connection)
        (let ((name (format "*sldb %s/%s*" (slime-connection-name) thread)))
          (with-current-buffer (generate-new-buffer name)
            (setq slime-buffer-connection connection
                  slime-current-thread thread)
            (current-buffer))))))

(defun sldb-debugged-continuations (connection)
  "Return the all debugged continuations for CONNECTION across SLDB buffers."
  (cl-loop for b in (sldb-buffers)
           append (with-current-buffer b
                    (and (eq slime-buffer-connection connection)
                         sldb-continuations))))

(defun sldb--display-buffer-reuse-last-window (buffer _alist)
  (let ((window
         (get-window-with-predicate (lambda (w)
                                      (window-parameter w 'sldb-last-window)))))
    (when (and window
               (not (with-current-buffer (window-buffer window)
                      (derived-mode-p 'sldb-mode))))
      (display-buffer-record-window 'reuse window buffer)
      (set-window-buffer window buffer)
      window)))

(defun sldb-display-buffer (buffer)
  "Pop to BUFFER reusing the last SLDB window, if any."
  (pop-to-buffer buffer '(sldb--display-buffer-reuse-last-window)))

(defun sldb-setup (thread level condition restarts frames conts)
  "Setup a new SLDB buffer.
CONDITION is a string describing the condition to debug.
RESTARTS is a list of strings (NAME DESCRIPTION) for each available restart.
FRAMES is a list (NUMBER DESCRIPTION &optional PLIST) describing the initial
portion of the backtrace. Frames are numbered from 0.
CONTS is a list of pending Emacs continuations."
  (with-current-buffer (sldb-get-buffer thread)
    (cl-assert (if (equal sldb-level level)
                   (equal sldb-condition condition)
                 t)
               () "Bug: sldb-level is equal but condition differs\n%s\n%s"
               sldb-condition condition)
    (unless (equal sldb-level level)
      (setq buffer-read-only nil)
      (sldb-mode)
      (setq slime-current-thread thread)
      (setq sldb-level level)
      (setq mode-name (format "sldb[%d]" sldb-level))
      (setq sldb-condition condition)
      (setq sldb-restarts restarts)
      (setq sldb-continuations conts)
      (sldb-insert-condition condition)
      (insert "\n\n" (sldb-in-face section "Restarts:") "\n")
      (setq sldb-restart-list-start-marker (point-marker))
      (sldb-insert-restarts restarts 0 sldb-initial-restart-limit)
      (insert "\n" (sldb-in-face section "Backtrace:") "\n")
      (setq sldb-backtrace-start-marker (point-marker))
      (save-excursion
        (if frames
            (sldb-insert-frames (sldb-prune-initial-frames frames) t)
          (insert "[No backtrace]")))
      (run-hooks 'sldb-hook)
      (set-syntax-table lisp-mode-syntax-table))
    ;; FIXME: remove when dropping Emacs23 support
    (let ((saved (selected-window)))
      (sldb-display-buffer (current-buffer))
      (set-window-parameter (selected-window) 'sldb-restore saved))
    (unless noninteractive ; needed for tests in batch-mode
      (slime--display-region (point-min) (point)))
    (setq buffer-read-only t)
    (when (and slime-stack-eval-tags
               ;; (y-or-n-p "Enter recursive edit? ")
               )
      (message "Entering recursive edit..")
      (recursive-edit))))

(defun sldb-activate (thread level select)
  "Display the debugger buffer for THREAD.
If LEVEL isn't the same as in the buffer reinitialize the buffer."
  (or (let ((buffer (sldb-find-buffer thread)))
        (when buffer
          (with-current-buffer buffer
            (when (equal sldb-level level)
              (when select (pop-to-buffer (current-buffer)))
              t))))
      (sldb-reinitialize thread level)))

(defun sldb-reinitialize (thread level)
  (slime-rex (thread level)
      ('(swank:debugger-info-for-emacs 0 10)
       nil thread)
    ((:ok result)
     (apply #'sldb-setup thread level result))))

(defun sldb--mark-last-window (window)
  (dolist (window (window-list))
    (when (window-parameter window 'sldb-last-window)
      (set-window-parameter window 'sldb-last-window nil)))
  (set-window-parameter (selected-window) 'sldb-last-window t))

(defun sldb-exit (thread _level &optional stepping)
  "Exit from the debug level LEVEL."
  (let ((sldb (sldb-find-buffer thread)))
    (when sldb
      (with-current-buffer sldb
        (cond (stepping
               (setq sldb-level nil)
               (run-with-timer 0.4 nil 'sldb-close-step-buffer sldb))
              ((not (eq sldb (window-buffer (selected-window))))
               ;; A different window selection means an indirect,
               ;; non-interactive exit, we just kill the sldb buffer.
               (kill-buffer))
              (t
               (sldb--mark-last-window (selected-window))
               ;; An interactive exit should restore configuration per
               ;; `quit-window's protocol. FIXME: remove
               ;; `previous-window' hack when dropping Emacs23 support
               (let ((previous-window (window-parameter (selected-window)
                                                        'sldb-restore)))
                 (quit-window t)
                 (if (and (not (>= emacs-major-version 24))
                          (window-live-p previous-window))
                     (select-window previous-window)))))))))

(defun sldb-close-step-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (not sldb-level)
        (quit-window t)))))


;;;;;; SLDB buffer insertion

(defun sldb-insert-condition (condition)
  "Insert the text for CONDITION.
CONDITION should be a list (MESSAGE TYPE EXTRAS).
EXTRAS is currently used for the stepper."
  (cl-destructuring-bind (message type extras) condition
    (slime-insert-propertized '(sldb-default-action sldb-inspect-condition)
                              (sldb-in-face topline message)
                              "\n"
                              (sldb-in-face condition type))
    (sldb-dispatch-extras extras)))

(defvar sldb-extras-hooks)

(defun sldb-dispatch-extras (extras)
  ;; this is (mis-)used for the stepper
  (dolist (extra extras)
    (slime-dcase extra
      ((:show-frame-source n)
       (sldb-show-frame-source n))
      (t
       (or (run-hook-with-args-until-success 'sldb-extras-hooks extra)
           ;;(error "Unhandled extra element:" extra)
           )))))

(defun sldb-insert-restarts (restarts start count)
  "Insert RESTARTS and add the needed text props
RESTARTS should be a list ((NAME DESCRIPTION) ...)."
  (let* ((len (length restarts))
         (end (if count (min (+ start count) len) len)))
    (cl-loop for (name string) in (cl-subseq restarts start end)
             for number from start
             do (slime-insert-propertized
                 `(,@nil restart ,number
                         sldb-default-action sldb-invoke-restart
                         mouse-face highlight)
                 " " (sldb-in-face restart-number (number-to-string number))
                 ": ["  (sldb-in-face restart-type name) "] "
                 (sldb-in-face restart string))
             (insert "\n"))
    (when (< end len)
      (let ((pos (point)))
        (slime-insert-propertized
         (list 'sldb-default-action
               (slime-rcurry #'sldb-insert-more-restarts restarts pos end))
         " --more--\n")))))

(defun sldb-insert-more-restarts (restarts position start)
  (goto-char position)
  (let ((inhibit-read-only t))
    (delete-region position (1+ (line-end-position)))
    (sldb-insert-restarts restarts start nil)))

(defun sldb-frame.string (frame)
  (cl-destructuring-bind (_ str &optional _) frame str))

(defun sldb-frame.number (frame)
  (cl-destructuring-bind (n _ &optional _) frame n))

(defun sldb-frame.plist (frame)
  (cl-destructuring-bind (_ _ &optional plist) frame plist))

(defun sldb-frame-restartable-p (frame)
  (and (plist-get (sldb-frame.plist frame) :restartable) t))

(defun sldb-prune-initial-frames (frames)
  "Return the prefix of FRAMES to initially present to the user.
Regexp heuristics are used to avoid showing SWANK-internal frames."
  (let* ((case-fold-search t)
         (rx "^\\([() ]\\|lambda\\)*swank\\>"))
    (or (cl-loop for frame in frames
                 until (string-match rx (sldb-frame.string frame))
                 collect frame)
        frames)))

(defun sldb-insert-frames (frames more)
  "Insert FRAMES into buffer.
If MORE is non-nil, more frames are on the Lisp stack."
  (mapc #'sldb-insert-frame frames)
  (when more
    (slime-insert-propertized
     `(,@nil sldb-default-action sldb-fetch-more-frames
             sldb-previous-frame-number
             ,(sldb-frame.number (cl-first (last frames)))
             point-entered sldb-fetch-more-frames
             start-open t
             face sldb-section-face
             mouse-face highlight)
     " --more--")
    (insert "\n")))

(defun sldb-compute-frame-face (frame)
  (if (sldb-frame-restartable-p frame)
      'sldb-restartable-frame-line-face
    'sldb-frame-line-face))

(defun sldb-insert-frame (frame &optional face)
  "Insert FRAME with FACE at point.
If FACE is nil, `sldb-compute-frame-face' is used to determine the face."
  (setq face (or face (sldb-compute-frame-face frame)))
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame))
        (props `(frame ,frame sldb-default-action sldb-toggle-details)))
    (slime-propertize-region props
      (slime-propertize-region '(mouse-face highlight)
        (insert " " (sldb-in-face frame-label (format "%2d:" number)) " ")
        (slime-insert-indented
         (slime-add-face face string)))
      (insert "\n"))))

(defun sldb-fetch-more-frames (&rest _)
  "Fetch more backtrace frames.
Called on the `point-entered' text-property hook."
  (let ((inhibit-point-motion-hooks t)
        (inhibit-read-only t)
        (prev (get-text-property (point) 'sldb-previous-frame-number)))
    ;; we may be called twice, PREV is nil the second time
    (when prev
      (let* ((count 40)
             (from (1+ prev))
             (to (+ from count))
             (frames (slime-eval `(swank:backtrace ,from ,to)))
             (more (slime-length= frames count))
             (pos (point)))
        (delete-region (line-beginning-position) (point-max))
        (sldb-insert-frames frames more)
        (goto-char pos)))))


;;;;;; SLDB examining text props

(defun sldb-restart-at-point ()
  (or (get-text-property (point) 'restart)
      (error "No restart at point")))

(defun sldb-frame-number-at-point ()
  (let ((frame (get-text-property (point) 'frame)))
    (cond (frame (car frame))
	  (t (error "No frame at point")))))

(defun sldb-var-number-at-point ()
  (let ((var (get-text-property (point) 'var)))
    (cond (var var)
	  (t (error "No variable at point")))))

(defun sldb-previous-frame-number ()
  (save-excursion
    (sldb-backward-frame)
    (sldb-frame-number-at-point)))

(defun sldb-frame-details-visible-p ()
  (and (get-text-property (point) 'frame)
       (get-text-property (point) 'details-visible-p)))

(defun sldb-frame-region ()
  (slime-property-bounds 'frame))

(defun sldb-forward-frame ()
  (goto-char (next-single-char-property-change (point) 'frame)))

(defun sldb-backward-frame ()
  (when (> (point) sldb-backtrace-start-marker)
    (goto-char (previous-single-char-property-change
                (if (get-text-property (point) 'frame)
                    (car (sldb-frame-region))
                  (point))
                'frame
                nil sldb-backtrace-start-marker))))

(defun sldb-goto-last-frame ()
  (goto-char (point-max))
  (while (not (get-text-property (point) 'frame))
    (goto-char (previous-single-property-change (point) 'frame))
    ;; Recenter to bottom of the window; -2 to account for the
    ;; empty last line displayed in sldb buffers.
    (recenter -2)))

(defun sldb-beginning-of-backtrace ()
  "Goto the first frame."
  (interactive)
  (goto-char sldb-backtrace-start-marker))


;;;;;; SLDB recenter & redisplay
;; not sure yet, whether this is a good idea.
;;
;; jt: seconded. Only `sldb-show-frame-details' and
;; `sldb-hide-frame-details' use this. They could avoid it by not
;; removing and reinserting the frame's name line.
(defmacro slime-save-coordinates (origin &rest body)
  "Restore line and column relative to ORIGIN, after executing BODY.

This is useful if BODY deletes and inserts some text but we want to
preserve the current row and column as closely as possible."
  (let ((base (make-symbol "base"))
        (goal (make-symbol "goal"))
        (mark (make-symbol "mark")))
    `(let* ((,base ,origin)
            (,goal (slime-coordinates ,base))
            (,mark (point-marker)))
       (set-marker-insertion-type ,mark t)
       (prog1 (save-excursion ,@body)
         (slime-restore-coordinate ,base ,goal ,mark)))))

(put 'slime-save-coordinates 'lisp-indent-function 1)

(defun slime-coordinates (origin)
  ;; Return a pair (X . Y) for the column and line distance to ORIGIN.
  (let ((y (slime-count-lines origin (point)))
        (x (save-excursion
             (- (current-column)
                (progn (goto-char origin) (current-column))))))
    (cons x y)))

(defun slime-restore-coordinate (base goal limit)
  ;; Move point to GOAL. Coordinates are relative to BASE.
  ;; Don't move beyond LIMIT.
  (save-restriction
    (narrow-to-region base limit)
    (goto-char (point-min))
    (let ((col (current-column)))
      (forward-line (cdr goal))
      (when (and (eobp) (bolp) (not (bobp)))
        (backward-char))
      (move-to-column (+ col (car goal))))))

(defun slime-count-lines (start end)
  "Return the number of lines between START and END.
This is 0 if START and END at the same line."
  (- (count-lines start end)
     (if (save-excursion (goto-char end) (bolp)) 0 1)))


;;;;; SLDB commands

(defun sldb-default-action ()
  "Invoke the action at point."
  (interactive)
  (let ((fn (get-text-property (point) 'sldb-default-action)))
    (if fn (funcall fn))))

(defun sldb-default-action/mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (cl-destructuring-bind (_mouse-1 (_w pos &rest ignore)) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 'sldb-default-action)))
	(if fn (funcall fn))))))

(defun sldb-cycle ()
  "Cycle between restart list and backtrace."
  (interactive)
  (let ((pt (point)))
    (cond ((< pt sldb-restart-list-start-marker)
           (goto-char sldb-restart-list-start-marker))
          ((< pt sldb-backtrace-start-marker)
           (goto-char sldb-backtrace-start-marker))
          (t
           (goto-char sldb-restart-list-start-marker)))))

(defun sldb-end-of-backtrace ()
  "Fetch the entire backtrace and go to the last frame."
  (interactive)
  (sldb-fetch-all-frames)
  (sldb-goto-last-frame))

(defun sldb-fetch-all-frames ()
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-goto-last-frame)
    (let ((last (sldb-frame-number-at-point)))
      (goto-char (next-single-char-property-change (point) 'frame))
      (delete-region (point) (point-max))
      (save-excursion
        (sldb-insert-frames (slime-eval `(swank:backtrace ,(1+ last) nil))
                            nil)))))


;;;;;; SLDB show source

(defun sldb-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-show-frame-source (frame-number)
  (slime-eval-async
      `(swank:frame-source-location ,frame-number)
    (lambda (source-location)
      (slime-dcase source-location
        ((:error message)
         (message "%s" message)
         (ding))
        (t
         (slime-show-source-location source-location t nil))))))

(defun slime-show-source-location (source-location
                                   &optional highlight recenter-arg)
  "Go to SOURCE-LOCATION and display the buffer in the other window."
  (slime-goto-source-location source-location)
  ;; show the location, but don't hijack focus.
  (slime--display-position (point) t recenter-arg)
  (when highlight (slime-highlight-sexp)))

(defun slime--display-position (pos other-window recenter-arg)
  (with-selected-window (display-buffer (current-buffer) other-window)
    (goto-char pos)
    (recenter recenter-arg)))

;; Set window-start so that the region from START to END becomes visible.
;; START is inclusive; END is exclusive.
(defun slime--adjust-window-start (start end)
  (let* ((last (max start (1- end)))
         (window-height (window-text-height))
         (region-height (count-screen-lines start last t)))
    ;; if needed, make the region visible
    (when (or (not (pos-visible-in-window-p start))
              (not (pos-visible-in-window-p last)))
      (let* ((nlines (cond ((or (< start (window-start))
                                (>= region-height window-height))
                            0)
                           (t
                            (- region-height)))))
        (goto-char start)
        (recenter nlines)))
    (cl-assert (pos-visible-in-window-p start))
    (cl-assert (or (pos-visible-in-window-p last)
                   (> region-height window-height)))
    (cl-assert (pos-visible-in-window-p (1- (window-end nil t)) nil t))))

;; move POS to visible region
(defun slime--adjust-window-point (pos)
  (cond ((pos-visible-in-window-p pos)
         (goto-char pos))
        ((< pos (window-start))
         (goto-char (window-start)))
        (t
         (goto-char (1- (window-end nil t)))
         (move-to-column 0)))
  (cl-assert (pos-visible-in-window-p (point) nil t)))

(defun slime--display-region (start end)
  "Make the region from START to END visible.
Minimize point motion."
  (cl-assert (<= start end))
  (cl-assert (eq (window-buffer (selected-window))
                 (current-buffer)))
  (let ((pos (point)))
    (slime--adjust-window-start start end)
    (slime--adjust-window-point pos)))

(defun slime-highlight-sexp (&optional start end)
  "Highlight the first sexp after point."
  (let ((start (or start (point)))
	(end (or end (save-excursion (ignore-errors (forward-sexp)) (point)))))
    (slime-flash-region start end)))

(defun slime-highlight-line (&optional timeout)
  (slime-flash-region (+ (line-beginning-position) (current-indentation))
                      (line-end-position)
                      timeout))


;;;;;; SLDB toggle details

(defun sldb-toggle-details (&optional on)
  "Toggle display of details for the current frame.
The details include local variable bindings and CATCH-tags."
  (interactive)
  (cl-assert (sldb-frame-number-at-point))
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (if (or on (not (sldb-frame-details-visible-p)))
	(sldb-show-frame-details)
      (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  ;; fetch and display info about local variables and catch tags
  (cl-destructuring-bind (start end frame locals catches) (sldb-frame-details)
    (slime-save-coordinates start
      (delete-region start end)
      (slime-propertize-region `(frame ,frame details-visible-p t)
        (sldb-insert-frame frame (if (sldb-frame-restartable-p frame)
                                     'sldb-restartable-frame-line-face
                                   ;; FIXME: can we somehow merge the two?
                                   'sldb-detailed-frame-line-face))
        (let ((indent1 "      ")
              (indent2 "        "))
          (insert indent1 (sldb-in-face section
                            (if locals "Locals:" "[No Locals]")) "\n")
          (sldb-insert-locals locals indent2 frame)
          (when catches
            (insert indent1 (sldb-in-face section "Catch-tags:") "\n")
            (dolist (tag catches)
              (slime-propertize-region `(catch-tag ,tag)
                (insert indent2 (sldb-in-face catch-tag (format "%s" tag))
                        "\n"))))
          (setq end (point)))))
    (slime--display-region (point) end)))

(defun sldb-frame-details ()
  ;; Return a list (START END FRAME LOCALS CATCHES) for frame at point.
  (let* ((frame (get-text-property (point) 'frame))
         (num (car frame)))
    (cl-destructuring-bind (start end) (sldb-frame-region)
      (cl-list* start end frame
                (slime-eval `(swank:frame-locals-and-catch-tags ,num))))))

(defvar sldb-insert-frame-variable-value-function
  'sldb-insert-frame-variable-value)

(defun sldb-insert-locals (vars prefix frame)
  "Insert VARS and add PREFIX at the beginning of each inserted line.
VAR should be a plist with the keys :name, :id, and :value."
  (cl-loop for i from 0
           for var in vars do
           (cl-destructuring-bind (&key name id value) var
             (slime-propertize-region
                 (list 'sldb-default-action 'sldb-inspect-var 'var i)
               (insert prefix
                       (sldb-in-face local-name
                         (concat name (if (zerop id) "" (format "#%d" id))))
                       " = ")
               (funcall sldb-insert-frame-variable-value-function
                        value frame i)
               (insert "\n")))))

(defun sldb-insert-frame-variable-value (value _frame _index)
  (insert (sldb-in-face local-value value)))

(defun sldb-hide-frame-details ()
  ;; delete locals and catch tags, but keep the function name and args.
  (cl-destructuring-bind (start end) (sldb-frame-region)
    (let ((frame (get-text-property (point) 'frame)))
      (slime-save-coordinates start
        (delete-region start end)
        (slime-propertize-region '(details-visible-p nil)
          (sldb-insert-frame frame))))))

(defun sldb-disassemble ()
  "Disassemble the code for the current frame."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-disassemble ,frame)
      (lambda (result)
        (slime-show-description result nil)))))


;;;;;; SLDB eval and inspect

(defun sldb-eval-in-frame (frame string package)
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive (sldb-read-form-for-frame "Eval in frame (%s)> "))
  (slime-eval-async `(swank:eval-string-in-frame ,string ,frame ,package)
    (if current-prefix-arg
        'slime-write-string
      'slime-display-eval-result)))

(defun sldb-pprint-eval-in-frame (frame string package)
  "Prompt for an expression, evaluate in selected frame, pretty-print result."
  (interactive (sldb-read-form-for-frame "Eval in frame (%s)> "))
  (slime-eval-async
      `(swank:pprint-eval-string-in-frame ,string ,frame ,package)
    (lambda (result)
      (slime-show-description result nil))))

(defun sldb-read-form-for-frame (fstring)
  (let* ((frame (sldb-frame-number-at-point))
         (pkg (slime-eval `(swank:frame-package-name ,frame))))
    (list frame
          (let ((slime-buffer-package pkg))
            (slime-read-from-minibuffer (format fstring pkg)))
          pkg)))

(defun sldb-inspect-in-frame (string)
  "Prompt for an expression and inspect it in the selected frame."
  (interactive (list (slime-read-from-minibuffer
                      "Inspect in frame (evaluated): "
                      (slime-sexp-at-point))))
  (let ((number (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:inspect-in-frame ,string ,number)
      'slime-open-inspector)))

(defun sldb-inspect-var ()
  (let ((frame (sldb-frame-number-at-point))
        (var (sldb-var-number-at-point)))
    (slime-eval-async `(swank:inspect-frame-var ,frame ,var)
      'slime-open-inspector)))

(defun sldb-inspect-condition ()
  "Inspect the current debugger condition."
  (interactive)
  (slime-eval-async '(swank:inspect-current-condition)
    'slime-open-inspector))

(defun sldb-print-condition ()
  (interactive)
  (slime-eval-describe `(swank:sdlb-print-condition)))


;;;;;; SLDB movement

(defun sldb-down ()
  "Select next frame."
  (interactive)
  (sldb-forward-frame))

(defun sldb-up ()
  "Select previous frame."
  (interactive)
  (sldb-backward-frame)
  (when (= (point) sldb-backtrace-start-marker)
    (recenter (1+ (count-lines (point-min) (point))))))

(defun sldb-sugar-move (move-fn)
  (let ((inhibit-read-only t))
    (when (sldb-frame-details-visible-p) (sldb-hide-frame-details))
    (funcall move-fn)
    (sldb-show-source)
    (sldb-toggle-details t)))

(defun sldb-details-up ()
  "Select previous frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-up))

(defun sldb-details-down ()
  "Select next frame and show details."
  (interactive)
  (sldb-sugar-move 'sldb-down))


;;;;;; SLDB restarts

(defun sldb-quit ()
  "Quit to toplevel."
  (interactive)
  (cl-assert sldb-restarts () "sldb-quit called outside of sldb buffer")
  (slime-rex () ('(swank:throw-to-toplevel))
    ((:ok x) (error "sldb-quit returned [%s]" x))
    ((:abort _))))

(defun sldb-continue ()
  "Invoke the \"continue\" restart."
  (interactive)
  (cl-assert sldb-restarts () "sldb-continue called outside of sldb buffer")
  (slime-rex ()
      ('(swank:sldb-continue))
    ((:ok _)
     (message "No restart named continue")
     (ding))
    ((:abort _))))

(defun sldb-abort ()
  "Invoke the \"abort\" restart."
  (interactive)
  (slime-eval-async '(swank:sldb-abort)
    (lambda (v) (message "Restart returned: %S" v))))

(defun sldb-invoke-restart (&optional number)
  "Invoke a restart.
Optional NUMBER (index into `sldb-restarts') specifies the
restart to invoke, otherwise use the restart at point."
  (interactive)
  (let ((restart (or number (sldb-restart-at-point))))
    (slime-rex ()
        ((list 'swank:invoke-nth-restart-for-emacs sldb-level restart))
      ((:ok value) (message "Restart returned: %s" value))
      ((:abort _)))))

(defun sldb-invoke-restart-by-name (restart-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Restart: " sldb-restarts nil t
                                        ""
                                        'sldb-invoke-restart-by-name))))
  (sldb-invoke-restart (cl-position restart-name sldb-restarts
                                    :test 'string= :key 'first)))

(defun sldb-break-with-default-debugger (&optional dont-unwind)
  "Enter default debugger."
  (interactive "P")
  (slime-rex ()
      ((list 'swank:sldb-break-with-default-debugger
             (not (not dont-unwind)))
       nil slime-current-thread)
    ((:abort _))))

(defun sldb-break-with-system-debugger (&optional lightweight)
  "Enter system debugger (gdb)."
  (interactive "P")
  (slime-attach-gdb slime-buffer-connection lightweight))

(defun slime-attach-gdb (connection &optional lightweight)
  "Run `gud-gdb'on the connection with PID `pid'.

If `lightweight' is given, do not send any request to the
inferior Lisp (e.g. to obtain default gdb config) but only
operate from the Emacs side; intended for cases where the Lisp is
truly screwed up."
  (interactive
   (list (slime-read-connection "Attach gdb to: " (slime-connection)) "P"))
  (let ((pid  (slime-pid connection))
        (file (slime-lisp-implementation-program connection))
        (commands (unless lightweight
                    (let ((slime-dispatching-connection connection))
                      (slime-eval `(swank:gdb-initial-commands))))))
    (gud-gdb (format "gdb -p %d %s" pid (or file "")))
    (with-current-buffer gud-comint-buffer
      (dolist (cmd commands)
        ;; First wait until gdb was initialized, then wait until current
        ;; command was processed.
        (while (not (looking-back comint-prompt-regexp nil))
          (sit-for 0.01))
        ;; We do not use `gud-call' because we want the initial commands
        ;; to be displayed by the user so he knows what he's got.
        (insert cmd)
        (comint-send-input)))))

(defun slime-read-connection (prompt &optional initial-value)
  "Read a connection from the minibuffer.
Return the net process, or nil."
  (cl-assert (memq initial-value slime-net-processes))
  (let* ((to-string (lambda (p)
                      (format "%s (pid %d)"
                              (slime-connection-name p) (slime-pid p))))
         (candidates (mapcar (lambda (p) (cons (funcall to-string p) p))
                             slime-net-processes)))
    (cdr (assoc (completing-read prompt candidates
                                 nil t (funcall to-string initial-value))
                candidates))))

(defun sldb-step ()
  "Step to next basic-block boundary."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-step ,frame))))

(defun sldb-next ()
  "Step over call."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-next ,frame))))

(defun sldb-out ()
  "Resume stepping after returning from this function."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-out ,frame))))

(defun sldb-break-on-return ()
  "Set a breakpoint at the current frame.
The debugger is entered when the frame exits."
  (interactive)
  (let ((frame (sldb-frame-number-at-point)))
    (slime-eval-async `(swank:sldb-break-on-return ,frame)
      (lambda (msg) (message "%s" msg)))))

(defun sldb-break (name)
  "Set a breakpoint at the start of the function NAME."
  (interactive (list (slime-read-symbol-name "Function: " t)))
  (slime-eval-async `(swank:sldb-break ,name)
    (lambda (msg) (message "%s" msg))))

(defun sldb-return-from-frame (string)
  "Reads an expression in the minibuffer and causes the function to
return that value, evaluated in the context of the frame."
  (interactive (list (slime-read-from-minibuffer "Return from frame: ")))
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:sldb-return-from-frame number string))
      ((:ok value) (message "%s" value))
      ((:abort _)))))

(defun sldb-restart-frame ()
  "Causes the frame to restart execution with the same arguments as it
was called originally."
  (interactive)
  (let* ((number (sldb-frame-number-at-point)))
    (slime-rex ()
        ((list 'swank:restart-frame number))
      ((:ok value) (message "%s" value))
      ((:abort _)))))

(defun slime-toggle-break-on-signals ()
  "Toggle the value of *break-on-signals*."
  (interactive)
  (slime-eval-async `(swank:toggle-break-on-signals)
    (lambda (msg) (message "%s" msg))))


;;;;;; SLDB recompilation commands

(defun sldb-recompile-frame-source (&optional raw-prefix-arg)
  (interactive "P")
  (slime-eval-async
      `(swank:frame-source-location ,(sldb-frame-number-at-point))
    (lexical-let ((policy (slime-compute-policy raw-prefix-arg)))
      (lambda (source-location)
        (slime-dcase source-location
          ((:error message)
           (message "%s" message)
           (ding))
          (t
           (let ((slime-compilation-policy policy))
             (slime-recompile-location source-location))))))))


;;;; Thread control panel

(defvar slime-threads-buffer-name (slime-buffer-name :threads))
(defvar slime-threads-buffer-timer nil)

(defcustom slime-threads-update-interval nil
  "Interval at which the list of threads will be updated."
  :type '(choice
          (number :value 0.5)
          (const nil))
  :group 'slime-ui)

(defun slime-list-threads ()
  "Display a list of threads."
  (interactive)
  (let ((name slime-threads-buffer-name))
    (slime-with-popup-buffer (name :connection t
                                   :mode 'slime-thread-control-mode)
      (slime-update-threads-buffer)
      (goto-char (point-min))
      (when slime-threads-update-interval
        (when slime-threads-buffer-timer
          (cancel-timer slime-threads-buffer-timer))
        (setq slime-threads-buffer-timer
              (run-with-timer
               slime-threads-update-interval
               slime-threads-update-interval
               'slime-update-threads-buffer))))))

(defun slime-quit-threads-buffer ()
  (when slime-threads-buffer-timer
    (cancel-timer slime-threads-buffer-timer))
  (quit-window t)
  (slime-eval-async `(swank:quit-thread-browser)))

(defun slime-update-threads-buffer ()
  (interactive)
  (with-current-buffer slime-threads-buffer-name
    (slime-eval-async '(swank:list-threads)
      'slime-display-threads)))

(defun slime-move-point (position)
  "Move point in the current buffer and in the window the buffer is displayed."
  (let ((window (get-buffer-window (current-buffer) t)))
    (goto-char position)
    (when window
      (set-window-point window position))))

(defun slime-display-threads (threads)
  (with-current-buffer slime-threads-buffer-name
    (let* ((inhibit-read-only t)
           (old-thread-id (get-text-property (point) 'thread-id))
           (old-line (line-number-at-pos))
           (old-column (current-column)))
      (erase-buffer)
      (slime-insert-threads threads)
      (let ((new-position (cl-position old-thread-id (cdr threads)
                                       :key #'car :test #'equal)))
        (goto-char (point-min))
        (forward-line (or new-position (1- old-line)))
        (move-to-column old-column)
        (slime-move-point (point))))))

(defun slime-transpose-lists (list-of-lists)
  (let ((ncols (length (car list-of-lists))))
    (cl-loop for col-index below ncols
             collect (cl-loop for row in list-of-lists
                              collect (elt row col-index)))))

(defun slime-insert-table-row (line line-props col-props col-widths)
  (slime-propertize-region line-props
    (cl-loop for string in line
             for col-prop in col-props
             for width in col-widths do
             (slime-insert-propertized col-prop string)
             (insert-char ?\ (- width (length string))))))

(defun slime-insert-table (rows header row-properties column-properties)
  "Insert a \"table\" so that the columns are nicely aligned."
  (let* ((ncols (length header))
         (lines (cons header rows))
         (widths (cl-loop for columns in (slime-transpose-lists lines)
                          collect (1+ (cl-loop for cell in columns
                                               maximize (length cell)))))
         (header-line (with-temp-buffer
                        (slime-insert-table-row
                         header nil (make-list ncols nil) widths)
                        (buffer-string))))
    (cond ((boundp 'header-line-format)
           (setq header-line-format header-line))
          (t (insert header-line "\n")))
    (cl-loop for line in rows  for line-props in row-properties do
             (slime-insert-table-row line line-props column-properties widths)
             (insert "\n"))))

(defvar slime-threads-table-properties
  '(nil (face bold)))

(defun slime-insert-threads (threads)
  (let* ((labels (car threads))
         (threads (cdr threads))
         (header (cl-loop for label in labels collect
                          (capitalize (substring (symbol-name label) 1))))
         (rows (cl-loop for thread in threads collect
                        (cl-loop for prop in thread collect
                                 (format "%s" prop))))
         (line-props (cl-loop for (id) in threads for i from 0
                              collect `(thread-index ,i thread-id ,id)))
         (col-props (cl-loop for nil in labels for i from 0 collect
                             (nth i slime-threads-table-properties))))
    (slime-insert-table rows header line-props col-props)))


;;;;; Major mode

(define-derived-mode slime-thread-control-mode fundamental-mode
  "Threads"
  "SLIME Thread Control Panel Mode.

\\{slime-thread-control-mode-map}
\\{slime-popup-buffer-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t))
  (setq buffer-undo-list t))

(slime-define-keys slime-thread-control-mode-map
  ("a" 'slime-thread-attach)
  ("d" 'slime-thread-debug)
  ("g" 'slime-update-threads-buffer)
  ("k" 'slime-thread-kill)
  ("q" 'slime-quit-threads-buffer))

(defun slime-thread-kill ()
  (interactive)
  (slime-eval `(cl:mapc 'swank:kill-nth-thread
                        ',(slime-get-properties 'thread-index)))
  (call-interactively 'slime-update-threads-buffer))

(defun slime-get-region-properties (prop start end)
  (cl-loop for position = (if (get-text-property start prop)
                              start
                            (next-single-property-change start prop))
           then (next-single-property-change position prop)
           while (<= position end)
           collect (get-text-property position prop)))

(defun slime-get-properties (prop)
  (if (use-region-p)
      (slime-get-region-properties prop
                                   (region-beginning)
                                   (region-end))
    (let ((value (get-text-property (point) prop)))
      (when value
        (list value)))))

(defun slime-thread-attach ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index))
        (file (slime-swank-port-file)))
    (slime-eval-async `(swank:start-swank-server-in-thread ,id ,file)))
  (slime-read-port-and-connect nil))

(defun slime-thread-debug ()
  (interactive)
  (let ((id (get-text-property (point) 'thread-index)))
    (slime-eval-async `(swank:debug-nth-thread ,id))))


;;;;; Connection listing

(define-derived-mode slime-connection-list-mode fundamental-mode
  "Slime-Connections"
  "SLIME Connection List Mode.

\\{slime-connection-list-mode-map}
\\{slime-popup-buffer-mode-map}"
  (when slime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(slime-define-keys slime-connection-list-mode-map
  ("d"         'slime-connection-list-make-default)
  ("g"         'slime-update-connection-list)
  ((kbd "C-k") 'slime-quit-connection-at-point)
  ("R"         'slime-restart-connection-at-point))

(defun slime-connection-at-point ()
  (or (get-text-property (point) 'slime-connection)
      (error "No connection at point")))

(defun slime-quit-connection-at-point (connection)
  (interactive (list (slime-connection-at-point)))
  (let ((slime-dispatching-connection connection)
        (end (time-add (current-time) (seconds-to-time 3))))
    (slime-quit-lisp t)
    (while (memq connection slime-net-processes)
      (when (time-less-p end (current-time))
        (message "Quit timeout expired.  Disconnecting.")
        (delete-process connection))
      (sit-for 0 100)))
  (slime-update-connection-list))

(defun slime-restart-connection-at-point (connection)
  (interactive (list (slime-connection-at-point)))
  (let ((slime-dispatching-connection connection))
    (slime-restart-inferior-lisp)))

(defun slime-connection-list-make-default ()
  "Make the connection at point the default connection."
  (interactive)
  (slime-select-connection (slime-connection-at-point))
  (slime-update-connection-list))

(defvar slime-connections-buffer-name (slime-buffer-name :connections))

(defun slime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (slime-with-popup-buffer (slime-connections-buffer-name
                            :mode 'slime-connection-list-mode)
    (slime-draw-connection-list)))

(defun slime-update-connection-list ()
  "Display a list of all connections."
  (interactive)
  (let ((pos (point))
        (inhibit-read-only t))
    (erase-buffer)
    (slime-draw-connection-list)
    (goto-char pos)))

(defun slime-draw-connection-list ()
  (let ((default-pos nil)
        (default slime-default-connection)
        (fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
            (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse slime-net-processes))
      (when (eq default p) (setf default-pos (point)))
      (slime-insert-propertized
       (list 'slime-connection p)
       (format fstring
               (if (eq default p) "*" " ")
               (slime-connection-number p)
               (slime-connection-name p)
               (or (process-id p) (process-contact p))
               (slime-pid p)
               (slime-lisp-implementation-type p))))
    (when default-pos
      (goto-char default-pos))))


;;;; Inspector

(defgroup slime-inspector nil
  "Inspector faces."
  :prefix "slime-inspector-"
  :group 'slime)

(defface slime-inspector-topline-face
  '((t ()))
  "Face for top line describing object."
  :group 'slime-inspector)

(defface slime-inspector-label-face
  '((t (:inherit font-lock-constant-face)))
  "Face for labels in the inspector."
  :group 'slime-inspector)

(defface slime-inspector-value-face
    '((t (:inherit font-lock-builtin-face)))
  "Face for things which can themselves be inspected."
  :group 'slime-inspector)

(defface slime-inspector-action-face
    '((t (:inherit font-lock-warning-face)))
  "Face for labels of inspector actions."
  :group 'slime-inspector)

(defface slime-inspector-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for type description in inspector."
  :group 'slime-inspector)

(defvar slime-inspector-mark-stack '())

(defun slime-inspect (string)
  "Eval an expression and inspect the result."
  (interactive
   (list (slime-read-from-minibuffer "Inspect value (evaluated): "
				     (slime-sexp-at-point))))
  (slime-eval-async `(swank:init-inspector ,string) 'slime-open-inspector))

(define-derived-mode slime-inspector-mode fundamental-mode
  "Slime-Inspector"
  "
\\{slime-inspector-mode-map}
\\{slime-popup-buffer-mode-map}"
  (set-syntax-table lisp-mode-syntax-table)
  (slime-set-truncate-lines)
  (setq buffer-read-only t))

(defun slime-inspector-buffer ()
  (or (get-buffer (slime-buffer-name :inspector))
      (slime-with-popup-buffer ((slime-buffer-name :inspector)
                                :mode 'slime-inspector-mode)
        (setq slime-inspector-mark-stack '())
        (buffer-disable-undo)
        (current-buffer))))

(defmacro slime-inspector-fontify (face string)
  `(slime-add-face ',(intern (format "slime-inspector-%s-face" face)) ,string))

(defvar slime-inspector-insert-ispec-function 'slime-inspector-insert-ispec)

(defun slime-open-inspector (inspected-parts &optional point hook)
  "Display INSPECTED-PARTS in a new inspector window.
Optionally set point to POINT. If HOOK is provided, it is added to local
KILL-BUFFER hooks for the inspector buffer."
  (with-current-buffer (slime-inspector-buffer)
    (when hook
      (add-hook 'kill-buffer-hook hook t t))
    (setq slime-buffer-connection (slime-current-connection))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (cl-destructuring-bind (&key id title content) inspected-parts
        (cl-macrolet ((fontify (face string)
                               `(slime-inspector-fontify ,face ,string)))
          (slime-propertize-region
              (list 'slime-part-number id
                    'mouse-face 'highlight
                    'face 'slime-inspector-value-face)
            (insert title))
          (while (eq (char-before) ?\n)
            (backward-delete-char 1))
          (insert "\n" (fontify label "--------------------") "\n")
          (save-excursion
            (slime-inspector-insert-content content))
          (when point
            (cl-check-type point cons)
            (ignore-errors
              (goto-char (point-min))
              (forward-line (1- (car point)))
              (move-to-column (cdr point)))))))))

(defvar slime-inspector-limit 500)

(defun slime-inspector-insert-content (content)
  (slime-inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((inhibit-read-only t))
       (slime-inspector-insert-chunk chunk t t)))))

(defun slime-inspector-insert-chunk (chunk prev next)
  "Insert CHUNK at point.
If PREV resp. NEXT are true insert more-buttons as needed."
  (cl-destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (slime-inspector-insert-more-button start t))
    (mapc slime-inspector-insert-ispec-function ispecs)
    (when (and next (< end len))
      (slime-inspector-insert-more-button end nil))))

(defun slime-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (slime-dcase ispec
      ((:value string id)
       (slime-propertize-region
           (list 'slime-part-number id
                 'mouse-face 'highlight
                 'face 'slime-inspector-value-face)
         (insert string)))
      ((:label string)
       (insert (slime-inspector-fontify label string)))
      ((:action string id)
       (slime-insert-propertized (list 'slime-action-number id
                                       'mouse-face 'highlight
                                       'face 'slime-inspector-action-face)
                                 string)))))

(defun slime-inspector-position ()
  "Return a pair (Y-POSITION X-POSITION) representing the
position of point in the current buffer."
  ;; We make sure we return absolute coordinates even if the user has
  ;; narrowed the buffer.
  ;; FIXME: why would somebody narrow the buffer?
  (save-restriction
    (widen)
    (cons (line-number-at-pos)
          (current-column))))

(defun slime-inspector-property-at-point ()
  (let* ((properties '(slime-part-number slime-range-button
                                         slime-action-number))
         (find-property
          (lambda (point)
            (cl-loop for property in properties
                     for value = (get-text-property point property)
                     when value
                     return (list property value)))))
    (or (funcall find-property (point))
        (funcall find-property (1- (point))))))

(defun slime-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursivly call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (let ((opener (lexical-let ((point (slime-inspector-position)))
                  (lambda (parts)
                    (when parts
                      (slime-open-inspector parts point)))))
        (new-opener (lambda (parts)
                      (when parts
                        (slime-open-inspector parts)))))
    (cl-destructuring-bind (&optional property value)
        (slime-inspector-property-at-point)
      (cl-case property
        (slime-part-number
         (slime-eval-async `(swank:inspect-nth-part ,value)
           new-opener)
         (push (slime-inspector-position) slime-inspector-mark-stack))
        (slime-range-button
         (slime-inspector-fetch-more value))
        (slime-action-number
         (slime-eval-async `(swank:inspector-call-nth-action ,value)
           opener))
        (t (error "No object at point"))))))

(defun slime-inspector-operate-on-click (event)
  "Move to events' position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'slime-part-number)
                    (get-text-property point 'slime-range-button)
                    (get-text-property point 'slime-action-number)))
           (goto-char point)
           (slime-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun slime-inspector-pop ()
  "Reinspect the previous object."
  (interactive)
  (slime-eval-async
      `(swank:inspector-pop)
    (lambda (result)
      (cond (result
             (slime-open-inspector result (pop slime-inspector-mark-stack)))
            (t
             (message "No previous object")
             (ding))))))

(defun slime-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (let ((result (slime-eval `(swank:inspector-next))))
    (cond (result
	   (push (slime-inspector-position) slime-inspector-mark-stack)
	   (slime-open-inspector result))
	  (t (message "No next object")
	     (ding)))))

(defun slime-inspector-quit ()
  "Quit the inspector and kill the buffer."
  (interactive)
  (slime-eval-async `(swank:quit-inspector))
  (quit-window t))

;; FIXME: first return value is just point.
;; FIXME: could probably use slime-search-property.
(defun slime-find-inspectable-object (direction limit)
  "Find the next/previous inspectable object.
DIRECTION can be either 'next or 'prev.
LIMIT is the maximum or minimum position in the current buffer.

Return a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned."
  (let ((finder (cl-ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'slime-part-number nil limit)))
          (setq prop (get-text-property newpos 'slime-part-number))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun slime-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (cl-destructuring-bind (pos foundp)
          (slime-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char minpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (cl-destructuring-bind (pos foundp)
          (slime-find-inspectable-object 'prev minpos)
        ;; SLIME-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char maxpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))))

(defun slime-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (slime-inspector-next-inspectable-object (- arg)))

(defun slime-inspector-describe ()
  (interactive)
  (slime-eval-describe `(swank:describe-inspectee)))

(defun slime-inspector-pprint (part)
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-eval-describe `(swank:pprint-inspector-part ,part)))

(defun slime-inspector-eval (string)
  "Eval an expression in the context of the inspected object.
The `*' variable will be bound to the inspected object."
  (interactive (list (slime-read-from-minibuffer "Inspector eval: ")))
  (slime-eval-with-transcript `(swank:inspector-eval ,string)))

(defun slime-inspector-history ()
  "Show the previously inspected objects."
  (interactive)
  (slime-eval-describe `(swank:inspector-history)))

(defun slime-inspector-show-source (part)
  (interactive (list (or (get-text-property (point) 'slime-part-number)
                         (error "No part at point"))))
  (slime-eval-async
      `(swank:find-source-location-for-emacs '(:inspector ,part))
    #'slime-show-source-location))

(defun slime-inspector-reinspect ()
  (interactive)
  (slime-eval-async `(swank:inspector-reinspect)
    (lexical-let ((point (slime-inspector-position)))
      (lambda (parts)
        (slime-open-inspector parts point)))))

(defun slime-inspector-toggle-verbose ()
  (interactive)
  (slime-eval-async `(swank:inspector-toggle-verbose)
    (lexical-let ((point (slime-inspector-position)))
      (lambda (parts)
        (slime-open-inspector parts point)))))

(defun slime-inspector-insert-more-button (index previous)
  (slime-insert-propertized
   (list 'slime-range-button (list index previous)
         'mouse-face 'highlight
         'face 'slime-inspector-action-face)
   (if previous " [--more--]\n" " [--more--]")))

(defun slime-inspector-fetch-all ()
  "Fetch all inspector contents and go to the end."
  (interactive)
  (goto-char (1- (point-max)))
  (let ((button (get-text-property (point) 'slime-range-button)))
    (when button
      (let (slime-inspector-limit)
        (slime-inspector-fetch-more button)))))

(defun slime-inspector-fetch-more (button)
  (cl-destructuring-bind (index prev) button
    (slime-inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (slime-rcurry
      (lambda (chunk prev)
        (let ((inhibit-read-only t))
          (apply #'delete-region (slime-property-bounds 'slime-range-button))
          (slime-inspector-insert-chunk chunk prev (not prev))))
      prev))))

(defun slime-inspector-fetch-chunk (chunk prev cont)
  (slime-inspector-fetch chunk slime-inspector-limit prev cont))

(defun slime-inspector-fetch (chunk limit prev cont)
  (cl-destructuring-bind (from to)
      (slime-inspector-next-range chunk limit prev)
    (cond ((and from to)
           (slime-eval-async
               `(swank:inspector-range ,from ,to)
             (slime-rcurry (lambda (chunk2 chunk1 limit prev cont)
                             (slime-inspector-fetch
                              (slime-inspector-join-chunks chunk1 chunk2)
                              limit prev cont))
                           chunk limit prev cont)))
          (t (funcall cont chunk)))))

(defun slime-inspector-next-range (chunk limit prev)
  (cl-destructuring-bind (_ len start end) chunk
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun slime-inspector-join-chunks (chunk1 chunk2)
  (cl-destructuring-bind (i1 _l1 s1 e1) chunk1
    (cl-destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(set-keymap-parent slime-inspector-mode-map slime-parent-map)

(slime-define-keys slime-inspector-mode-map
  ([return] 'slime-inspector-operate-on-point)
  ("\C-m"   'slime-inspector-operate-on-point)
  ([mouse-1] 'slime-inspector-operate-on-click)
  ([mouse-2] 'slime-inspector-operate-on-click)
  ([mouse-6] 'slime-inspector-pop)
  ([mouse-7] 'slime-inspector-next)
  ("l" 'slime-inspector-pop)
  ("n" 'slime-inspector-next)
  (" " 'slime-inspector-next)
  ("d" 'slime-inspector-describe)
  ("p" 'slime-inspector-pprint)
  ("e" 'slime-inspector-eval)
  ("h" 'slime-inspector-history)
  ("g" 'slime-inspector-reinspect)
  ("v" 'slime-inspector-toggle-verbose)
  ("\C-i" 'slime-inspector-next-inspectable-object)
  ([(shift tab)]
   'slime-inspector-previous-inspectable-object) ; Emacs translates S-TAB
  ([backtab] 'slime-inspector-previous-inspectable-object) ; to BACKTAB on X.
  ("." 'slime-inspector-show-source)
  (">" 'slime-inspector-fetch-all)
  ("q" 'slime-inspector-quit))


;;;; Buffer selector

(defvar slime-selector-methods nil
  "List of buffer-selection methods for the `slime-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar slime-selector-other-window nil
  "If non-nil use switch-to-buffer-other-window.")

(defun slime-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer. The `?' character describes the
available methods.

See `def-slime-selector-method' for defining new methods."
  (interactive "P")
  (message "Select [%s]: "
           (apply #'string (mapcar #'car slime-selector-methods)))
  (let* ((slime-selector-other-window other-window)
         (sequence (save-window-excursion
                    (select-window (minibuffer-window))
                    (key-description (read-key-sequence nil))))
         (ch (cond ((equal sequence "C-g")
                    (keyboard-quit))
                   ((equal sequence "TAB")
                    ?i)
                   ((= (length sequence) 1)
                    (elt sequence 0))
                   ((= (length sequence) 3)
                    (elt sequence 2))))
         (method (cl-find ch slime-selector-methods :key #'car)))
    (cond (method
           (funcall (cl-third method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (slime-selector)))))

(defmacro def-slime-selector-method (key description &rest body)
  "Define a new `slime-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen. The returned buffer is selected with
switch-to-buffer."
  (let ((method `(lambda ()
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (slime-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq slime-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key slime-selector-methods :key #'car))
                    #'< :key #'car))))

(def-slime-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (cl-loop for (key line nil) in slime-selector-methods
             do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (slime-selector)
  (current-buffer))

(cl-pushnew (list ?4 "Select in other window" (lambda () (slime-selector t)))
            slime-selector-methods :key #'car)

(def-slime-selector-method ?q "Abort."
  (top-level))

(def-slime-selector-method ?i
  "*inferior-lisp* buffer."
  (cond ((and (slime-connected-p) (slime-process))
         (process-buffer (slime-process)))
        (t
         "*inferior-lisp*")))

(def-slime-selector-method ?v
  "*slime-events* buffer."
  slime-event-buffer-name)

(def-slime-selector-method ?l
  "most recently visited lisp-mode buffer."
  (slime-recently-visited-buffer 'lisp-mode))

(def-slime-selector-method ?d
  "*sldb* buffer for the current connection."
  (or (sldb-get-default-buffer)
      (error "No debugger buffer")))

(def-slime-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (slime-recently-visited-buffer 'emacs-lisp-mode))

(def-slime-selector-method ?c
  "SLIME connections buffer."
  (slime-list-connections)
  slime-connections-buffer-name)

(def-slime-selector-method ?n
  "Cycle to the next Lisp connection."
  (slime-next-connection)
  (concat "*slime-repl "
          (slime-connection-name (slime-current-connection))
          "*"))

(def-slime-selector-method ?p
  "Cycle to the previous Lisp connection."
  (slime-prev-connection)
  (concat "*slime-repl "
          (slime-connection-name (slime-current-connection))
          "*"))

(def-slime-selector-method ?t
  "SLIME threads buffer."
  (slime-list-threads)
  slime-threads-buffer-name)

(defun slime-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose major-mode is MODE.
Only considers buffers that are not already visible."
  (cl-loop for buffer in (buffer-list)
           when (and (with-current-buffer buffer (eq major-mode mode))
                     (not (string-match "^ " (buffer-name buffer)))
                     (null (get-buffer-window buffer 'visible)))
           return buffer
           finally (error "Can't find unshown buffer in %S" mode)))


;;;; Indentation

(defun slime-update-indentation ()
  "Update indentation for all macros defined in the Lisp system."
  (interactive)
  (slime-eval-async '(swank:update-indentation-information)))

(defvar slime-indentation-update-hooks)

(defun slime-intern-indentation-spec (spec)
  (cond ((consp spec)
         (cons (slime-intern-indentation-spec (car spec))
               (slime-intern-indentation-spec (cdr spec))))
        ((stringp spec)
         (intern spec))
        (t
         spec)))

;; FIXME: restore the old version without per-package
;; stuff. slime-indentation.el should be able tho disable the simple
;; version if needed.
(defun slime-handle-indentation-update (alist)
  "Update Lisp indent information.

ALIST is a list of (SYMBOL-NAME . INDENT-SPEC) of proposed indentation
settings for `common-lisp-indent-function'. The appropriate property
is setup, unless the user already set one explicitly."
  (dolist (info alist)
    (let ((symbol (intern (car info)))
          (indent (slime-intern-indentation-spec (cl-second info)))
          (packages (cl-third info)))
      (if (and (boundp 'common-lisp-system-indentation)
               (fboundp 'slime-update-system-indentation))
          ;; A table provided by slime-cl-indent.el.
          (funcall #'slime-update-system-indentation symbol indent packages)
        ;; Does the symbol have an indentation value that we set?
        (when (equal (get symbol 'common-lisp-indent-function)
                     (get symbol 'slime-indent))
          (put symbol 'common-lisp-indent-function indent)
          (put symbol 'slime-indent indent)))
      (run-hook-with-args 'slime-indentation-update-hooks
                          symbol indent packages))))


;;;; Contrib modules

(defun slime-require (module)
  (cl-pushnew module slime-required-modules)
  (when (slime-connected-p)
    (slime-load-contribs)))

(defun slime-load-contribs ()
  (let ((needed (cl-remove-if (lambda (s)
                                (member (cl-subseq (symbol-name s) 1)
                                        (mapcar #'downcase
                                                (slime-lisp-modules))))
                              slime-required-modules)))
    (when needed
      ;; No asynchronous request because with :SPAWN that could result
      ;; in the attempt to load modules concurrently which may not be
      ;; supported by the host Lisp.
      (setf (slime-lisp-modules)
            (slime-eval `(swank:swank-require ',needed))))))

(cl-defstruct slime-contrib
  name
  slime-dependencies
  swank-dependencies
  enable
  disable
  authors
  license)

(defun slime-contrib--enable-fun (name)
  (intern (concat (symbol-name name) "-init")))

(defun slime-contrib--disable-fun (name)
  (intern (concat (symbol-name name) "-unload")))

(defmacro define-slime-contrib (name _docstring &rest clauses)
  (declare (indent 1))
  (cl-destructuring-bind (&key slime-dependencies
                               swank-dependencies
                               on-load
                               on-unload
                               authors
                               license)
      (cl-loop for (key . value) in clauses append `(,key ,value))
    `(progn
       ,@(mapcar (lambda (d) `(require ',d)) slime-dependencies)
       (defun ,(slime-contrib--enable-fun name) ()
         (mapc #'funcall ',(mapcar
                            #'slime-contrib--enable-fun
                            slime-dependencies))
         (mapc #'slime-require ',swank-dependencies)
         ,@on-load)
       (defun ,(slime-contrib--disable-fun name) ()
         ,@on-unload
         (mapc #'funcall ',(mapcar
                            #'slime-contrib--disable-fun
                            slime-dependencies)))
       (put 'slime-contribs ',name
            (make-slime-contrib
             :name ',name :authors ',authors :license ',license
             :slime-dependencies ',slime-dependencies
             :swank-dependencies ',swank-dependencies
             :enable ',(slime-contrib--enable-fun name)
             :disable ',(slime-contrib--disable-fun name))))))

(defun slime-all-contribs ()
  (cl-loop for (nil val) on (symbol-plist 'slime-contribs) by #'cddr
           when (slime-contrib-p val)
           collect val))

(defun slime-contrib-all-dependencies (contrib)
  "List all contribs recursively needed by CONTRIB, including self."
  (cons contrib
        (cl-mapcan #'slime-contrib-all-dependencies
                   (slime-contrib-slime-dependencies
                    (slime-find-contrib contrib)))))

(defun slime-find-contrib (name)
  (get 'slime-contribs name))

(defun slime-read-contrib-name ()
  (let ((names (cl-loop for c in (slime-all-contribs) collect
                        (symbol-name (slime-contrib-name c)))))
    (intern (completing-read "Contrib: " names nil t))))

(defun slime-enable-contrib (name)
  (interactive (list (slime-read-contrib-name)))
  (let ((c (or (slime-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (slime-contrib-enable c))))

(defun slime-disable-contrib (name)
  (interactive (list (slime-read-contrib-name)))
  (let ((c (or (slime-find-contrib name)
               (error "Unknown contrib: %S" name))))
    (funcall (slime-contrib-disable c))))


;;;;; Pull-down menu

(defvar slime-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLIME"
      [ "Edit Definition..."       slime-edit-definition ,C ]
      [ "Return From Definition"   slime-pop-find-definition-stack ,C ]
      [ "Complete Symbol"          completion-at-point ,C ]
      "--"
      ("Evaluation"
       [ "Eval Defun"              slime-eval-defun ,C ]
       [ "Eval Last Expression"    slime-eval-last-expression ,C ]
       [ "Eval And Pretty-Print"   slime-pprint-eval-last-expression ,C ]
       [ "Eval Region"             slime-eval-region ,C ]
       [ "Eval Region And Pretty-Print" slime-pprint-eval-region ,C ]
       [ "Interactive Eval..."     slime-interactive-eval ,C ]
       [ "Edit Lisp Value..."      slime-edit-value ,C ]
       [ "Call Defun"              slime-call-defun ,C ])
      ("Debugging"
       [ "Macroexpand Once..."     slime-macroexpand-1 ,C ]
       [ "Macroexpand All..."      slime-macroexpand-all ,C ]
       [ "Create Trace Buffer"     slime-redirect-trace-output ,C ]
       [ "Toggle Trace..."         slime-toggle-trace-fdefinition ,C ]
       [ "Untrace All"             slime-untrace-all ,C]
       [ "Disassemble..."          slime-disassemble-symbol ,C ]
       [ "Inspect..."              slime-inspect ,C ])
      ("Compilation"
       [ "Compile Defun"           slime-compile-defun ,C ]
       [ "Compile/Load File"       slime-compile-and-load-file ,C ]
       [ "Compile File"            slime-compile-file ,C ]
       [ "Compile Region"          slime-compile-region ,C ]
       "--"
       [ "Next Note"               slime-next-note t ]
       [ "Previous Note"           slime-previous-note t ]
       [ "Remove Notes"            slime-remove-notes t ]
       [ "List Notes"              slime-list-compiler-notes ,C ])
      ("Cross Reference"
       [ "Who Calls..."            slime-who-calls ,C ]
       [ "Who References... "      slime-who-references ,C ]
       [ "Who Sets..."             slime-who-sets ,C ]
       [ "Who Binds..."            slime-who-binds ,C ]
       [ "Who Macroexpands..."     slime-who-macroexpands ,C ]
       [ "Who Specializes..."      slime-who-specializes ,C ]
       [ "List Callers..."         slime-list-callers ,C ]
       [ "List Callees..."         slime-list-callees ,C ]
       [ "Next Location"           slime-next-location t ])
      ("Editing"
       [ "Check Parens"            check-parens t]
       [ "Update Indentation"      slime-update-indentation ,C]
       [ "Select Buffer"           slime-selector t])
      ("Profiling"
       [ "Toggle Profiling..."     slime-toggle-profile-fdefinition ,C ]
       [ "Profile Package"         slime-profile-package ,C]
       [ "Profile by Substring"    slime-profile-by-substring ,C ]
       [ "Unprofile All"           slime-unprofile-all ,C ]
       [ "Show Profiled"           slime-profiled-functions ,C ]
       "--"
       [ "Report"                  slime-profile-report ,C ]
       [ "Reset Counters"          slime-profile-reset ,C ])
      ("Documentation"
       [ "Describe Symbol..."      slime-describe-symbol ,C ]
       [ "Lookup Documentation..." slime-documentation-lookup t ]
       [ "Apropos..."              slime-apropos ,C ]
       [ "Apropos all..."          slime-apropos-all ,C ]
       [ "Apropos Package..."      slime-apropos-package ,C ]
       [ "Hyperspec..."            slime-hyperspec-lookup t ])
      "--"
      [ "Interrupt Command"        slime-interrupt ,C ]
      [ "Abort Async. Command"     slime-quit ,C ]
      [ "Sync Package & Directory" slime-sync-package-and-default-directory ,C]
      )))

(defvar slime-sldb-easy-menu
  (let ((C '(slime-connected-p)))
    `("SLDB"
      [ "Next Frame" sldb-down t ]
      [ "Previous Frame" sldb-up t ]
      [ "Toggle Frame Details" sldb-toggle-details t ]
      [ "Next Frame (Details)" sldb-details-down t ]
      [ "Previous Frame (Details)" sldb-details-up t ]
      "--"
      [ "Eval Expression..." slime-interactive-eval ,C ]
      [ "Eval in Frame..." sldb-eval-in-frame ,C ]
      [ "Eval in Frame (pretty print)..." sldb-pprint-eval-in-frame ,C ]
      [ "Inspect In Frame..." sldb-inspect-in-frame ,C ]
      [ "Inspect Condition Object" sldb-inspect-condition ,C ]
      "--"
      [ "Restart Frame" sldb-restart-frame ,C ]
      [ "Return from Frame..." sldb-return-from-frame ,C ]
      ("Invoke Restart"
       [ "Continue" sldb-continue ,C ]
       [ "Abort"    sldb-abort ,C ]
       [ "Step"      sldb-step ,C ]
       [ "Step next" sldb-next ,C ]
       [ "Step out"  sldb-out ,C ]
       )
      "--"
      [ "Quit (throw)" sldb-quit ,C ]
      [ "Break With Default Debugger" sldb-break-with-default-debugger ,C ])))

(easy-menu-define menubar-slime slime-mode-map "SLIME" slime-easy-menu)

(defun slime-add-easy-menu ()
  (easy-menu-add slime-easy-menu 'slime-mode-map))

(add-hook 'slime-mode-hook 'slime-add-easy-menu)

(defun slime-sldb-add-easy-menu ()
  (easy-menu-define menubar-slime-sldb
    sldb-mode-map "SLDB" slime-sldb-easy-menu)
  (easy-menu-add slime-sldb-easy-menu 'sldb-mode-map))

(add-hook 'sldb-mode-hook 'slime-sldb-add-easy-menu)


;;;; Cheat Sheet

(defvar
  slime-cheat-sheet-table
  '((:title
     "Editing lisp code"
     :map slime-mode-map
     :bindings ((slime-eval-defun "Evaluate current top level form")
                (slime-compile-defun "Compile current top level form")
                (slime-interactive-eval "Prompt for form and eval it")
                (slime-compile-and-load-file "Compile and load current file")
                (slime-sync-package-and-default-directory
                 "Synch default package and directory with current buffer")
                (slime-next-note "Next compiler note")
                (slime-previous-note "Previous compiler note")
                (slime-remove-notes "Remove notes")
                slime-documentation-lookup))
    (:title "Completion"
            :map slime-mode-map
            :bindings (slime-indent-and-complete-symbol
                       slime-fuzzy-complete-symbol))
    (:title
     "Within SLDB buffers"
     :map sldb-mode-map
     :bindings ((sldb-default-action "Do 'whatever' with thing at point")
                (sldb-toggle-details "Toggle frame details visualization")
                (sldb-quit "Quit to REPL")
                (sldb-abort "Invoke ABORT restart")
                (sldb-continue "Invoke CONTINUE restart (if available)")
                (sldb-show-source "Jump to frame's source code")
                (sldb-eval-in-frame "Evaluate in frame at point")
                (sldb-inspect-in-frame
                 "Evaluate in frame at point and inspect result")))
    (:title
     "Within the Inspector"
     :map slime-inspector-mode-map
     :bindings ((slime-inspector-next-inspectable-object
                 "Jump to next inspectable object")
                (slime-inspector-operate-on-point
                 "Inspect object or execute action at point")
                (slime-inspector-reinspect "Reinspect current object")
                (slime-inspector-pop "Return to previous object")
                ;;(slime-inspector-copy-down "Send object at point to REPL")
                (slime-inspector-toggle-verbose "Toggle verbose mode")
                (slime-inspector-quit "Quit")))
    (:title
     "Finding Definitions"
     :map slime-mode-map
     :bindings (slime-edit-definition
                slime-pop-find-definition-stack))))

(defun slime-cheat-sheet ()
  (interactive)
  (switch-to-buffer-other-frame
   (get-buffer-create (slime-buffer-name :cheat-sheet)))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (insert
   "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode).\n\n")
  (dolist (mode slime-cheat-sheet-table)
    (let ((title (cl-getf mode :title))
          (mode-map (cl-getf mode :map))
          (mode-keys (cl-getf mode :bindings)))
      (insert title)
      (insert ":\n")
      (insert (make-string (1+ (length title)) ?-))
      (insert "\n")
      (let ((keys '())
            (descriptions '()))
        (dolist (func mode-keys)
          ;; func is eithor the function name or a list (NAME DESCRIPTION)
          (push (if (symbolp func)
                    (prin1-to-string func)
                  (cl-second func))
                descriptions)
          (let ((all-bindings (where-is-internal (if (symbolp func)
                                                     func
                                                   (cl-first func))
                                                 (symbol-value mode-map)))
                (key-bindings '()))
            (dolist (binding all-bindings)
              (when (and (vectorp binding)
                         (integerp (aref binding 0)))
                (push binding key-bindings)))
            (push (mapconcat 'key-description key-bindings " or ") keys)))
        (cl-loop with desc-length = (apply 'max (mapcar 'length descriptions))
                 for key in (nreverse keys)
                 for desc in (nreverse descriptions)
                 do (insert desc)
                 do (insert (make-string (- desc-length (length desc)) ? ))
                 do (insert " => ")
                 do (insert (if (string= "" key)
                                "<not on any key>"
                              key))
                 do (insert "\n")
                 finally do (insert "\n")))))
  (setq buffer-read-only t)
  (goto-char (point-min)))


;;;; Utilities (no not Paul Graham style)

;; XXX: unused function
(defun slime-intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list)
      '()
    (cons (car list)
          (cl-mapcan (lambda (x) (list element x)) (cdr list)))))

;;; FIXME: this looks almost slime `slime-alistify', perhaps the two
;;;        functions can be merged.
(defun slime-group-similar (similar-p list)
  "Return the list of lists of 'similar' adjacent elements of LIST.
The function SIMILAR-P is used to test for similarity.
The order of the input list is preserved."
  (if (null list)
      nil
    (let ((accumulator (list (list (car list)))))
      (dolist (x (cdr list))
        (if (funcall similar-p x (caar accumulator))
            (push x (car accumulator))
          (push (list x) accumulator)))
      (reverse (mapcar #'reverse accumulator)))))

(defun slime-alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
	     (probe (cl-assoc k alist :test test)))
	(if probe
	    (push e (cdr probe))
          (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (cl-loop for (key . value) in (reverse alist)
             collect (cons key (reverse value)))))

;;;;; Misc.

(defun slime-length= (seq n)
  "Return (= (length SEQ) N)."
  (cl-etypecase seq
    (list
     (cond ((zerop n) (null seq))
           ((let ((tail (nthcdr (1- n) seq)))
              (and tail (null (cdr tail)))))))
    (sequence
     (= (length seq) n))))

(defun slime-length> (seq n)
  "Return (> (length SEQ) N)."
  (cl-etypecase seq
    (list (nthcdr n seq))
    (sequence (> (length seq) n))))

(defun slime-trim-whitespace (str)
  (let ((start (cl-position-if-not (lambda (x)
                                     (memq x '(?\t ?\n ?\s ?\r)))
                                   str))

        (end (cl-position-if-not (lambda (x)
                                   (memq x '(?\t ?\n ?\s ?\r)))
                                 str
                                 :from-end t)))
    (if start
        (substring str start (1+ end))
        "")))

;;;;; Buffer related

(defun slime-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun slime-column-max ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop for column = (prog2 (end-of-line) (current-column) (forward-line))
             until (= (point) (point-max))
             maximizing column)))

;;;;; CL symbols vs. Elisp symbols.

(defun slime-cl-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(let ((symbol-part (match-string 1 n)))
          (if (string-match "^|\\(.*\\)|$" symbol-part)
              (match-string 1 symbol-part)
            symbol-part))
      n)))

(defun slime-cl-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
      default)))

(defun slime-qualify-cl-symbol-name (symbol-or-name)
  "Return a package-qualified string for SYMBOL-OR-NAME.
If SYMBOL-OR-NAME doesn't already have a package prefix the
current package is used."
  (let ((s (if (stringp symbol-or-name)
               symbol-or-name
             (symbol-name symbol-or-name))))
    (if (slime-cl-symbol-package s)
        s
      (format "%s::%s"
              (let* ((package (slime-current-package)))
                ;; package is a string like ":cl-user"
                ;; or "CL-USER", or "\"CL-USER\"".
                (if package
                    (slime-pretty-package-name package)
                  "CL-USER"))
              (slime-cl-symbol-name s)))))

;;;;; Moving, CL idiosyncracies aware (reader conditionals &c.)

(defmacro slime-point-moves-p (&rest body)
  "Execute BODY and return true if the current buffer's point moved."
  (declare (indent 0))
  (let ((pointvar (cl-gensym "point-")))
    `(let ((,pointvar (point)))
       (save-current-buffer ,@body)
       (/= ,pointvar (point)))))

(defun slime-forward-sexp (&optional count)
  "Like `forward-sexp', but understands reader-conditionals (#- and #+),
and skips comments."
  (dotimes (_i (or count 1))
    (slime-forward-cruft)
    (forward-sexp)))

(defconst slime-reader-conditionals-regexp
  ;; #!+, #!- are SBCL specific reader-conditional syntax.
  ;; We need this for the source files of SBCL itself.
  (regexp-opt '("#+" "#-" "#!+" "#!-")))

(defun slime-forward-reader-conditional ()
  "Move past any reader conditional (#+ or #-) at point."
  (when (looking-at slime-reader-conditionals-regexp)
    (goto-char (match-end 0))
    (let* ((plus-conditional-p (eq (char-before) ?+))
           (result (slime-eval-feature-expression
                    (condition-case e
                        (read (current-buffer))
                      (invalid-read-syntax
                       (signal 'slime-unknown-feature-expression (cdr e)))))))
      (unless (if plus-conditional-p result (not result))
        ;; skip this sexp
        (slime-forward-sexp)))))

(defun slime-forward-cruft ()
  "Move forward over whitespace, comments, reader conditionals."
  (while (slime-point-moves-p (skip-chars-forward " \t\n")
                              (forward-comment (buffer-size))
                              (inline (slime-forward-reader-conditional)))))

(defun slime-keywordify (symbol)
  "Make a keyword out of the symbol SYMBOL."
  (let ((name (downcase (symbol-name symbol))))
    (intern (if (eq ?: (aref name 0))
                name
              (concat ":" name)))))

(put 'slime-incorrect-feature-expression
     'error-conditions '(slime-incorrect-feature-expression error))

(put 'slime-unknown-feature-expression
     'error-conditions '(slime-unknown-feature-expression
                         slime-incorrect-feature-expression
                         error))

;; FIXME: let it crash
;; FIXME: the length=1 constraint is bogus
(defun slime-eval-feature-expression (e)
  "Interpret a reader conditional expression."
  (cond ((symbolp e)
         (memq (slime-keywordify e) (slime-lisp-features)))
        ((and (consp e) (symbolp (car e)))
         (funcall (let ((head (slime-keywordify (car e))))
                    (cl-case head
                      (:and #'cl-every)
                      (:or #'cl-some)
                      (:not
                       (lexical-let ((feature-expression e))
                         (lambda (f l)
                           (cond
                            ((slime-length= l 0) t)
                            ((slime-length= l 1) (not (apply f l)))
                            (t (signal 'slime-incorrect-feature-expression
                                       feature-expression))))))
                      (t (signal 'slime-unknown-feature-expression head))))
                  #'slime-eval-feature-expression
                  (cdr e)))
        (t (signal 'slime-incorrect-feature-expression e))))

;;;;; Extracting Lisp forms from the buffer or user

(defun slime-defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (slime-region-for-defun-at-point)))

(defun slime-region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun slime-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\s\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\=#[-+.<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun slime-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\s_\\|\\s\\.\\|#:\\|[@|]\\)*"))

(put 'slime-symbol 'end-op 'slime-end-of-symbol)
(put 'slime-symbol 'beginning-op 'slime-beginning-of-symbol)

(defun slime-symbol-start-pos ()
  "Return the starting position of the symbol under point.
The result is unspecified if there isn't a symbol under the point."
  (save-excursion (slime-beginning-of-symbol) (point)))

(defun slime-symbol-end-pos ()
  (save-excursion (slime-end-of-symbol) (point)))

(defun slime-bounds-of-symbol-at-point ()
  "Return the bounds of the symbol around point.
The returned bounds are either nil or non-empty."
  (let ((bounds (bounds-of-thing-at-point 'slime-symbol)))
    (if (and bounds
             (< (car bounds)
                (cdr bounds)))
        bounds)))

(defun slime-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  ;; (thing-at-point 'symbol) returns "" in empty buffers
  (let ((bounds (slime-bounds-of-symbol-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun slime-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (or (slime-bounds-of-symbol-at-point)
      (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)))

(defun slime-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (slime-bounds-of-sexp-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun slime-sexp-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (slime-sexp-at-point) (user-error "No expression at point")))

(defun slime-string-at-point ()
  "Returns the string at point as a string, otherwise nil."
  (let ((sexp (slime-sexp-at-point)))
    (if (and sexp
             (eql (char-syntax (aref sexp 0)) ?\"))
        sexp
        nil)))

(defun slime-string-at-point-or-error ()
  "Return the sexp at point as a string, othwise signal an error."
  (or (slime-string-at-point) (error "No string at point.")))

(defun slime-input-complete-p (start end)
  "Return t if the region from START to END contains a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *['`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))


;;;; slime.el in pretty colors

(cl-loop for sym in (list 'slime-def-connection-var
                          'slime-define-channel-type
                          'slime-define-channel-method
                          'define-slime-contrib
                          'slime-defun-if-undefined
                          'slime-defmacro-if-undefined)
         for regexp = (format "(\\(%S\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
                              sym)
         do (font-lock-add-keywords
             'emacs-lisp-mode
             `((,regexp (1 font-lock-keyword-face)
                        (2 font-lock-variable-name-face)))))

;;;; target manipulation (used by slime-presentations, slime-media,
;;;;                      slime-repl and slime-buffer-streams, at
;;;;                      least)

(defvar slime-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun slime-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (gethash target slime-output-target-to-marker))

(defun slime-emit-to-target (string target)
  "Insert STRING at target TARGET.
See `slime-output-target-to-marker'."
  (let* ((marker (slime-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

;;;; Finishing up

(eval-when-compile
  (require 'bytecomp))

(defun slime--byte-compile (symbol)
  (require 'bytecomp) ;; tricky interaction between autoload and let.
  (let ((byte-compile-warnings '()))
    (byte-compile symbol)))

(defun slime--compile-hotspots ()
  (mapc (lambda (sym)
          (cond ((fboundp sym)
                 (unless (byte-code-function-p (symbol-function sym))
                   (slime--byte-compile sym)))
                (t (error "%S is not fbound" sym))))
        '(slime-alistify
          slime-log-event
          slime-events-buffer
          slime-process-available-input
          slime-dispatch-event
          slime-net-filter
          slime-net-have-input-p
          slime-net-decode-length
          slime-net-read
          slime-print-apropos
          slime-insert-propertized
          slime-beginning-of-symbol
          slime-end-of-symbol
          slime-eval-feature-expression
          slime-forward-sexp
          slime-forward-cruft
          slime-forward-reader-conditional)))

(slime--compile-hotspots)

(add-to-list 'load-path (expand-file-name "contrib" slime-path))

(run-hooks 'slime-load-hook)
(provide 'slime)

(when (member 'lisp-mode slime-lisp-modes)
  (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook))

;; Local Variables:
;; outline-regexp: ";;;;+"
;; indent-tabs-mode: nil
;; coding: latin-1-unix
;; End:
;;; slime.el ends here
