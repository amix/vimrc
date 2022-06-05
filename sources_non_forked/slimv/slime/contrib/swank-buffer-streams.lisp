;;; swank-buffer-streams.lisp --- Streams that output to a buffer
;;;
;;; Authors: Ed Langley  <el-github@elangley.org>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(in-package :swank)

(defpackage :swank-buffer-streams
  (:use :cl)
  (:import-from :swank
                defslimefun
                add-hook
                encode-message
                send-event
                find-thread
                dcase
                current-socket-io
                send-to-emacs
                current-thread-id
                wait-for-event

                *emacs-connection*
                *event-hook*)
  (:export make-buffer-output-stream))

(in-package :swank-buffer-streams)

(defun get-temporary-identifier ()
  (intern (symbol-name (gensym "BUFFER"))
          :keyword))

(defun make-buffer-output-stream (&optional (target-identifier (get-temporary-identifier)))
  (swank:ed-rpc '#:slime-make-buffer-stream-target (current-thread-id) target-identifier)
  (values (swank:make-output-stream-for-target *emacs-connection* target-identifier)
          target-identifier))

(provide :swank-buffer-streams)
