;;;; Source-file cache
;;;
;;; To robustly find source locations in CMUCL and SBCL it's useful to
;;; have the exact source code that the loaded code was compiled from.
;;; In this source we can accurately find the right location, and from
;;; that location we can extract a "snippet" of code to show what the
;;; definition looks like. Emacs can use this snippet in a best-match
;;; search to locate the right definition, which works well even if
;;; the buffer has been modified.
;;;
;;; The idea is that if a definition previously started with
;;; `(define-foo bar' then it probably still does.
;;;
;;; Whenever we see that the file on disk has the same
;;; `file-write-date' as a location we're looking for we cache the
;;; whole file inside Lisp. That way we will still have the matching
;;; version even if the file is later modified on disk. If the file is
;;; later recompiled and reloaded then we replace our cache entry.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.

(defpackage swank/source-file-cache
  (:use cl)
  (:import-from swank/backend
		defimplementation buffer-first-change
		guess-external-format
		find-external-format)
  (:export
   get-source-code
   source-cache-get ;FIXME: isn't it odd that both are exported?

   *source-snippet-size*
   read-snippet
   read-snippet-from-string
   ))

(in-package swank/source-file-cache)

(defvar *cache-sourcecode* t
  "When true complete source files are cached.
The cache is used to keep known good copies of the source text which
correspond to the loaded code. Finding definitions is much more
reliable when the exact source is available, so we cache it in case it
gets edited on disk later.")

(defvar *source-file-cache* (make-hash-table :test 'equal)
  "Cache of source file contents.
Maps from truename to source-cache-entry structure.")

(defstruct (source-cache-entry
             (:conc-name source-cache-entry.)
             (:constructor make-source-cache-entry (text date)))
  text date)

(defimplementation buffer-first-change (filename)
  "Load a file into the cache when the user modifies its buffer.
This is a win if the user then saves the file and tries to M-. into it."
  (unless (source-cached-p filename)
    (ignore-errors
      (source-cache-get filename (file-write-date filename))))
  nil)

(defun get-source-code (filename code-date)
  "Return the source code for FILENAME as written on DATE in a string.
If the exact version cannot be found then return the current one from disk."
  (or (source-cache-get filename code-date)
      (read-file filename)))

(defun source-cache-get (filename date)
  "Return the source code for FILENAME as written on DATE in a string.
Return NIL if the right version cannot be found."
  (when *cache-sourcecode*
    (let ((entry (gethash filename *source-file-cache*)))
      (cond ((and entry (equal date (source-cache-entry.date entry)))
             ;; Cache hit.
             (source-cache-entry.text entry))
            ((or (null entry)
                 (not (equal date (source-cache-entry.date entry))))
             ;; Cache miss.
             (if (equal (file-write-date filename) date)
                 ;; File on disk has the correct version.
                 (let ((source (read-file filename)))
                   (setf (gethash filename *source-file-cache*)
                         (make-source-cache-entry source date))
                   source)
                 nil))))))

(defun source-cached-p (filename)
  "Is any version of FILENAME in the source cache?"
  (if (gethash filename *source-file-cache*) t))

(defun read-file (filename)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input
		     :external-format (or (guess-external-format filename)
					  (find-external-format "latin-1")
					  :default))
    (let* ((string (make-string (file-length s)))
           (length (read-sequence string s)))
      (subseq string 0 length))))

;;;; Snippets

(defvar *source-snippet-size* 256
  "Maximum number of characters in a snippet of source code.
Snippets at the beginning of definitions are used to tell Emacs what
the definitions looks like, so that it can accurately find them by
text search.")

(defun read-snippet (stream &optional position)
  "Read a string of upto *SOURCE-SNIPPET-SIZE* characters from STREAM.
If POSITION is given, set the STREAM's file position first."
  (when position
    (file-position stream position))
  #+sbcl (skip-comments-and-whitespace stream)
  (read-upto-n-chars stream *source-snippet-size*))

(defun read-snippet-from-string (string &optional position)
  (with-input-from-string (s string)
    (read-snippet s position)))

(defun skip-comments-and-whitespace (stream)
  (case (peek-char nil stream nil nil)
    ((#\Space #\Tab #\Newline #\Linefeed #\Page)
     (read-char stream)
     (skip-comments-and-whitespace stream))
    (#\;
     (read-line stream)
     (skip-comments-and-whitespace stream))))

(defun read-upto-n-chars (stream n)
  "Return a string of upto N chars from STREAM."
  (let* ((string (make-string n))
         (chars  (read-sequence string stream)))
    (subseq string 0 chars)))
