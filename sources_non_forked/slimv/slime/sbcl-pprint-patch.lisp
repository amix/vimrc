;; Pretty printer patch for SBCL, which adds the "annotations" feature
;; required for sending presentations through pretty-printing streams.
;;
;; The section marked "Changed functions" and the DEFSTRUCT
;; PRETTY-STREAM are based on SBCL's pprint.lisp.
;; 
;; Public domain.

(in-package "SB!PRETTY")

(defstruct (annotation (:include queued-op))
  (handler (constantly nil) :type function)
  (record))


(defstruct (pretty-stream (:include sb!kernel:ansi-stream
				    (out #'pretty-out)
				    (sout #'pretty-sout)
				    (misc #'pretty-misc))
			  (:constructor make-pretty-stream (target))
			  (:copier nil))
  ;; Where the output is going to finally go.
  (target (missing-arg) :type stream)
  ;; Line length we should format to. Cached here so we don't have to keep
  ;; extracting it from the target stream.
  (line-length (or *print-right-margin*
		   (sb!impl::line-length target)
		   default-line-length)
	       :type column)
  ;; A simple string holding all the text that has been output but not yet
  ;; printed.
  (buffer (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; The index into BUFFER where more text should be put.
  (buffer-fill-pointer 0 :type index)
  ;; Whenever we output stuff from the buffer, we shift the remaining noise
  ;; over. This makes it difficult to keep references to locations in
  ;; the buffer. Therefore, we have to keep track of the total amount of
  ;; stuff that has been shifted out of the buffer.
  (buffer-offset 0 :type posn)
  ;; The column the first character in the buffer will appear in. Normally
  ;; zero, but if we end up with a very long line with no breaks in it we
  ;; might have to output part of it. Then this will no longer be zero.
  (buffer-start-column (or (sb!impl::charpos target) 0) :type column)
  ;; The line number we are currently on. Used for *PRINT-LINES*
  ;; abbreviations and to tell when sections have been split across
  ;; multiple lines.
  (line-number 0 :type index)
  ;; the value of *PRINT-LINES* captured at object creation time. We
  ;; use this, instead of the dynamic *PRINT-LINES*, to avoid
  ;; weirdness like
  ;;   (let ((*print-lines* 50))
  ;;     (pprint-logical-block ..
  ;;       (dotimes (i 10)
  ;;         (let ((*print-lines* 8))
  ;;           (print (aref possiblybigthings i) prettystream)))))
  ;; terminating the output of the entire logical blockafter 8 lines.
  (print-lines *print-lines* :type (or index null) :read-only t)
  ;; Stack of logical blocks in effect at the buffer start.
  (blocks (list (make-logical-block)) :type list)
  ;; Buffer holding the per-line prefix active at the buffer start.
  ;; Indentation is included in this. The length of this is stored
  ;; in the logical block stack.
  (prefix (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; Buffer holding the total remaining suffix active at the buffer start.
  ;; The characters are right-justified in the buffer to make it easier
  ;; to output the buffer. The length is stored in the logical block
  ;; stack.
  (suffix (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; Queue of pending operations. When empty, HEAD=TAIL=NIL. Otherwise,
  ;; TAIL holds the first (oldest) cons and HEAD holds the last (newest)
  ;; cons. Adding things to the queue is basically (setf (cdr head) (list
  ;; new)) and removing them is basically (pop tail) [except that care must
  ;; be taken to handle the empty queue case correctly.]
  (queue-tail nil :type list)
  (queue-head nil :type list)
  ;; Block-start queue entries in effect at the queue head.
  (pending-blocks nil :type list)
  ;; Queue of annotations to the buffer
  (annotations-tail nil :type list)
  (annotations-head nil :type list))


(defmacro enqueue (stream type &rest args)
  (let ((constructor (intern (concatenate 'string
					  "MAKE-"
					  (symbol-name type))
			     "SB-PRETTY")))
    (once-only ((stream stream)
		(entry `(,constructor :posn
				      (index-posn
				       (pretty-stream-buffer-fill-pointer
					,stream)
				       ,stream)
				      ,@args))
		(op `(list ,entry))
		(head `(pretty-stream-queue-head ,stream)))
      `(progn
	 (if ,head
	     (setf (cdr ,head) ,op)
	     (setf (pretty-stream-queue-tail ,stream) ,op))
	 (setf (pretty-stream-queue-head ,stream) ,op)
	 ,entry))))

;;;
;;; New helper functions
;;;

(defun enqueue-annotation (stream handler record)
  (enqueue stream annotation :handler handler
	   :record record))

(defun re-enqueue-annotation (stream annotation)
  (let* ((annotation-cons (list annotation))
	 (head (pretty-stream-annotations-head stream)))
    (if head
	(setf (cdr head) annotation-cons)
	(setf (pretty-stream-annotations-tail stream) annotation-cons))
    (setf (pretty-stream-annotations-head stream) annotation-cons)
    nil))

(defun re-enqueue-annotations (stream end)
  (loop for tail = (pretty-stream-queue-tail stream) then (cdr tail)
     while (and tail (not (eql (car tail) end)))
     when (annotation-p (car tail)) 
     do (re-enqueue-annotation stream (car tail))))

(defun dequeue-annotation (stream &key end-posn)
  (let ((next-annotation (car (pretty-stream-annotations-tail stream))))
    (when next-annotation
      (when (or (not end-posn)
		(<= (annotation-posn next-annotation) end-posn))
	(pop (pretty-stream-annotations-tail stream))
	(unless (pretty-stream-annotations-tail stream)
	  (setf (pretty-stream-annotations-head stream) nil))
	next-annotation))))

(defun invoke-annotation (stream annotation truncatep)
  (let ((target (pretty-stream-target stream)))
    (funcall (annotation-handler annotation)
	     (annotation-record annotation)
	     target
	     truncatep)))

(defun output-buffer-with-annotations (stream end)
  (let ((target (pretty-stream-target stream))
	(buffer (pretty-stream-buffer stream))
	(end-posn (index-posn end stream))
	(start 0))
    (loop
       for annotation = (dequeue-annotation stream :end-posn end-posn)
       while annotation
       do
	 (let ((annotation-index (posn-index (annotation-posn annotation)
					     stream)))
	   (when (> annotation-index start)
	     (write-string buffer target :start start 
			   :end annotation-index)
	     (setf start annotation-index))
	   (invoke-annotation stream annotation nil)))
    (when (> end start)
      (write-string buffer target :start start :end end))))

(defun flush-annotations (stream end truncatep)
  (let ((end-posn (index-posn end stream)))
    (loop
       for annotation = (dequeue-annotation stream :end-posn end-posn)
       while annotation
       do (invoke-annotation stream annotation truncatep))))

;;;
;;; Changed functions
;;;

(defun maybe-output (stream force-newlines-p)
  (declare (type pretty-stream stream))
  (let ((tail (pretty-stream-queue-tail stream))
	(output-anything nil))
    (loop
      (unless tail
	(setf (pretty-stream-queue-head stream) nil)
	(return))
      (let ((next (pop tail)))
	(etypecase next
	  (newline
	   (when (ecase (newline-kind next)
		   ((:literal :mandatory :linear) t)
		   (:miser (misering-p stream))
		   (:fill
		    (or (misering-p stream)
			(> (pretty-stream-line-number stream)
			   (logical-block-section-start-line
			    (first (pretty-stream-blocks stream))))
			(ecase (fits-on-line-p stream
					       (newline-section-end next)
					       force-newlines-p)
			  ((t) nil)
			  ((nil) t)
			  (:dont-know
			   (return))))))
	     (setf output-anything t)
	     (output-line stream next)))
	  (indentation
	   (unless (misering-p stream)
	     (set-indentation stream
			      (+ (ecase (indentation-kind next)
				   (:block
				    (logical-block-start-column
				     (car (pretty-stream-blocks stream))))
				   (:current
				    (posn-column
				     (indentation-posn next)
				     stream)))
				 (indentation-amount next)))))
	  (block-start
	   (ecase (fits-on-line-p stream (block-start-section-end next)
				  force-newlines-p)
	     ((t)
	      ;; Just nuke the whole logical block and make it look like one
	      ;; nice long literal.  (But don't nuke annotations.)
	      (let ((end (block-start-block-end next)))
		(expand-tabs stream end)
		(re-enqueue-annotations stream end)
		(setf tail (cdr (member end tail)))))
	     ((nil)
	      (really-start-logical-block
	       stream
	       (posn-column (block-start-posn next) stream)
	       (block-start-prefix next)
	       (block-start-suffix next)))
	     (:dont-know
	      (return))))
	  (block-end
	   (really-end-logical-block stream))
	  (tab
	   (expand-tabs stream next))
	  (annotation
	   (re-enqueue-annotation stream next))))
      (setf (pretty-stream-queue-tail stream) tail))
    output-anything))

(defun output-line (stream until)
  (declare (type pretty-stream stream)
	   (type newline until))
  (let* ((target (pretty-stream-target stream))
	 (buffer (pretty-stream-buffer stream))
	 (kind (newline-kind until))
	 (literal-p (eq kind :literal))
	 (amount-to-consume (posn-index (newline-posn until) stream))
	 (amount-to-print
	  (if literal-p
	      amount-to-consume
	      (let ((last-non-blank
		     (position #\space buffer :end amount-to-consume
			       :from-end t :test #'char/=)))
		(if last-non-blank
		    (1+ last-non-blank)
		    0)))))
    (output-buffer-with-annotations stream amount-to-print)
    (flush-annotations stream amount-to-consume nil)
    (let ((line-number (pretty-stream-line-number stream)))
      (incf line-number)
      (when (and (not *print-readably*)
		 (pretty-stream-print-lines stream)
		 (>= line-number (pretty-stream-print-lines stream)))
	(write-string " .." target)
	(flush-annotations stream 
			   (pretty-stream-buffer-fill-pointer stream)
			   t)
	(let ((suffix-length (logical-block-suffix-length
			      (car (pretty-stream-blocks stream)))))
	  (unless (zerop suffix-length)
	    (let* ((suffix (pretty-stream-suffix stream))
		   (len (length suffix)))
	      (write-string suffix target
			    :start (- len suffix-length)
			    :end len))))
	(throw 'line-limit-abbreviation-happened t))
      (setf (pretty-stream-line-number stream) line-number)
      (write-char #\newline target)
      (setf (pretty-stream-buffer-start-column stream) 0)
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	     (block (first (pretty-stream-blocks stream)))
	     (prefix-len
	      (if literal-p
		  (logical-block-per-line-prefix-end block)
		  (logical-block-prefix-length block)))
	     (shift (- amount-to-consume prefix-len))
	     (new-fill-ptr (- fill-ptr shift))
	     (new-buffer buffer)
	     (buffer-length (length buffer)))
	(when (> new-fill-ptr buffer-length)
	  (setf new-buffer
		(make-string (max (* buffer-length 2)
				  (+ buffer-length
				     (floor (* (- new-fill-ptr buffer-length)
					       5)
					    4)))))
	  (setf (pretty-stream-buffer stream) new-buffer))
	(replace new-buffer buffer
		 :start1 prefix-len :start2 amount-to-consume :end2 fill-ptr)
	(replace new-buffer (pretty-stream-prefix stream)
		 :end1 prefix-len)
	(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
	(incf (pretty-stream-buffer-offset stream) shift)
	(unless literal-p
	  (setf (logical-block-section-column block) prefix-len)
	  (setf (logical-block-section-start-line block) line-number))))))

(defun output-partial-line (stream)
  (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	 (tail (pretty-stream-queue-tail stream))
	 (count
	  (if tail
	      (posn-index (queued-op-posn (car tail)) stream)
	      fill-ptr))
	 (new-fill-ptr (- fill-ptr count))
	 (buffer (pretty-stream-buffer stream)))
    (when (zerop count)
      (error "Output-partial-line called when nothing can be output."))
    (output-buffer-with-annotations stream count)
    (incf (pretty-stream-buffer-start-column stream) count)
    (replace buffer buffer :end1 new-fill-ptr :start2 count :end2 fill-ptr)
    (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
    (incf (pretty-stream-buffer-offset stream) count)))

(defun force-pretty-output (stream)
  (maybe-output stream nil)
  (expand-tabs stream nil)
  (re-enqueue-annotations stream nil)
  (output-buffer-with-annotations stream 
				  (pretty-stream-buffer-fill-pointer stream)))
	      