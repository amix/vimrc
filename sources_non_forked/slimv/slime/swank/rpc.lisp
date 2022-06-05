;;; -*- indent-tabs-mode: nil; coding: latin-1-unix -*-
;;;
;;; swank-rpc.lisp  -- Pass remote calls and responses between lisp systems.
;;;
;;; Created 2010, Terje Norderhaug <terje@in-progress.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package swank/rpc)


;;;;; Input

(define-condition swank-reader-error (reader-error)
  ((packet :type string :initarg :packet 
           :reader swank-reader-error.packet)
   (cause :type reader-error :initarg :cause 
          :reader swank-reader-error.cause)))

(defun read-message (stream package)
  (let ((packet (read-packet stream)))
    (handler-case (values (read-form packet package))
      (reader-error (c)
        (error 'swank-reader-error 
               :packet packet :cause c)))))

(defun read-packet (stream)
  (let* ((length (parse-header stream))
         (octets (read-chunk stream length)))
    (handler-case (swank/backend:utf8-to-string octets)
      (error (c) 
        (error 'swank-reader-error 
               :packet (asciify octets)
               :cause c)))))

(defun asciify (packet)
  (with-output-to-string (*standard-output*)
    (loop for code across (etypecase packet 
                            (string (map 'vector #'char-code packet))
                            (vector packet))
          do (cond ((<= code #x7f) (write-char (code-char code)))
                   (t (format t "\\x~x" code))))))

(defun parse-header (stream)
  (parse-integer (map 'string #'code-char (read-chunk stream 6))
                 :radix 16))

(defun read-chunk (stream length)
  (let* ((buffer (make-array length :element-type '(unsigned-byte 8)))
         (count (read-sequence buffer stream)))
    (cond ((= count length)
           buffer)
          ((zerop count)
           (error 'end-of-file :stream stream))
          (t
           (error "Short read: length=~D  count=~D" length count)))))

(defparameter *validate-input* nil
  "Set to true to require input that more strictly conforms to the protocol")

(defun read-form (string package)
  (with-standard-io-syntax
    (let ((*package* package))
      (if *validate-input*
          (validating-read string)
          (read-from-string string)))))

(defun validating-read (string)
  (with-input-from-string (*standard-input* string)
    (simple-read)))

(defun simple-read ()
   "Read a form that conforms to the protocol, otherwise signal an error."
   (let ((c (read-char)))
     (case c
       (#\( (loop collect (simple-read)
                  while (ecase (read-char)
                          (#\) nil)
                          (#\space t))))
       (#\' `(quote ,(simple-read)))
       (t
        (cond
          ((digit-char-p c)
           (parse-integer
            (map 'simple-string #'identity
                 (loop for ch = c then (read-char nil nil)
                       while (and ch (digit-char-p ch))
                       collect ch
                       finally (unread-char ch)))))
          ((or (member c '(#\: #\")) (alpha-char-p c))
           (unread-char c)
           (read-preserving-whitespace))
          (t (error "Invalid character ~:c" c)))))))


;;;;; Output

(defun write-message (message package stream)
  (let* ((string (prin1-to-string-for-emacs message package))
         (octets (handler-case (swank/backend:string-to-utf8 string)
                   (error (c) (encoding-error c string))))
         (length (length octets)))
    (write-header stream length)
    (write-sequence octets stream)
    (finish-output stream)))

;; FIXME: for now just tell emacs that we and an encoding problem.
(defun encoding-error (condition string)
  (swank/backend:string-to-utf8
   (prin1-to-string-for-emacs
    `(:reader-error
      ,(asciify string)
      ,(format nil "Error during string-to-utf8: ~a"
               (or (ignore-errors (asciify (princ-to-string condition)))
                   (asciify (princ-to-string (type-of condition))))))
    (find-package :cl))))

(defun write-header (stream length)
  (declare (type (unsigned-byte 24) length))
  ;;(format *trace-output* "length: ~d (#x~x)~%" length length)
  (loop for c across (format nil "~6,'0x" length)
        do (write-byte (char-code c) stream)))

(defun switch-to-double-floats (x)
  (typecase x
    (double-float x)
    (float (coerce x 'double-float))
    (null x)
    (list (loop for (x . cdr) on x
                collect (switch-to-double-floats x) into result
                until (atom cdr)
                finally (return (append result (switch-to-double-floats cdr)))))
    (t x)))

(defun prin1-to-string-for-emacs (object package)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package)
          ;; Emacs has only double floats.
          (*read-default-float-format* 'double-float))
      (prin1-to-string (switch-to-double-floats object)))))


#| TEST/DEMO:

(defparameter *transport*
  (with-output-to-string (out)
    (write-message '(:message (hello "world")) *package* out)
    (write-message '(:return 5) *package* out)
    (write-message '(:emacs-rex NIL) *package* out)))

*transport*
                 
(with-input-from-string (in *transport*)
  (loop while (peek-char T in NIL)
        collect (read-message in *package*)))

|#
