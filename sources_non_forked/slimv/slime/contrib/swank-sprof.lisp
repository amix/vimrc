;;; swank-sprof.lisp
;;
;; Authors: Juho Snellman
;;
;; License: MIT
;;

(in-package :swank)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

#+sbcl(progn

(defvar *call-graph* nil)
(defvar *node-numbers* nil)
(defvar *number-nodes* nil)

(defun frame-name (name)
  (if (consp name)
      (case (first name)
        ((sb-c::xep sb-c::tl-xep
                    sb-c::&more-processor
                    sb-c::top-level-form
                    sb-c::&optional-processor)
         (second name))
        (sb-pcl::fast-method
         (cdr name))
        ((flet labels lambda)
         (let* ((in (member :in name)))
           (if (stringp (cadr in))
               (append (ldiff name in) (cddr in))
               name)))
        (t
         name))
      name))

(defun pretty-name (name)
  (let ((*package* (find-package :common-lisp-user))
        (*print-right-margin* most-positive-fixnum))
    (format nil "~S" (frame-name name))))

(defun samples-percent (count)
  (sb-sprof::samples-percent *call-graph* count))

(defun node-values (node)
  (values (pretty-name (sb-sprof::node-name node))
          (samples-percent (sb-sprof::node-count node))
          (samples-percent (sb-sprof::node-accrued-count node))))

(defun filter-swank-nodes (nodes)
  (let ((swank-packages (load-time-value
                         (mapcar #'find-package
                                 '(swank swank/rpc swank/mop
                                   swank/match swank/backend)))))
    (remove-if (lambda (node)
                 (let ((name (sb-sprof::node-name node)))
                   (and (symbolp name)
                        (member (symbol-package name) swank-packages
                                :test #'eq))))
               nodes)))

(defun serialize-call-graph (&key exclude-swank)
  (let ((nodes (sb-sprof::call-graph-flat-nodes *call-graph*)))
    (when exclude-swank
      (setf nodes (filter-swank-nodes nodes)))
    (setf nodes (sort (copy-list nodes) #'>
                      ;; :key #'sb-sprof::node-count)))
                      :key #'sb-sprof::node-accrued-count))
    (setf *number-nodes* (make-hash-table))
    (setf *node-numbers* (make-hash-table))
    (loop for node in nodes
          for i from 1
          with total = 0
          collect (multiple-value-bind (name self cumulative)
                      (node-values node)
                    (setf (gethash node *node-numbers*) i
                          (gethash i *number-nodes*) node)
                    (incf total self)
                    (list i name self cumulative total)) into list
          finally (return
                    (let ((rest (- 100 total)))
                      (return (append list
                                      `((nil "Elsewhere" ,rest nil nil)))))))))

(defslimefun swank-sprof-get-call-graph (&key exclude-swank)
  (when (setf *call-graph* (sb-sprof:report :type nil))
    (serialize-call-graph :exclude-swank exclude-swank)))

(defslimefun swank-sprof-expand-node (index)
  (let* ((node (gethash index *number-nodes*)))
    (labels ((caller-count (v)
               (loop for e in (sb-sprof::vertex-edges v) do
                     (when (eq (sb-sprof::edge-vertex e) node)
                       (return-from caller-count (sb-sprof::call-count e))))
               0)
             (serialize-node (node count)
               (etypecase node
                 (sb-sprof::cycle
                  (list (sb-sprof::cycle-index node)
                        (sb-sprof::cycle-name node)
                        (samples-percent count)))
                 (sb-sprof::node
                  (let ((name (node-values node)))
                    (list (gethash node *node-numbers*)
                          name
                          (samples-percent count)))))))
      (list :callers (loop for node in
                           (sort (copy-list (sb-sprof::node-callers node)) #'>
                                 :key #'caller-count)
                           collect (serialize-node node
                                                   (caller-count node)))
            :calls (let ((edges (sort (copy-list (sb-sprof::vertex-edges node))
                                      #'>
                                      :key #'sb-sprof::call-count)))
                     (loop for edge in edges
                           collect
                           (serialize-node (sb-sprof::edge-vertex edge)
                                           (sb-sprof::call-count edge))))))))

(defslimefun swank-sprof-disassemble (index)
  (let* ((node (gethash index *number-nodes*))
         (debug-info (sb-sprof::node-debug-info node)))
    (with-output-to-string (s)
      (typecase debug-info
        (sb-impl::code-component
         (sb-disassem::disassemble-memory (sb-vm::code-instructions debug-info)
                                          (sb-vm::%code-code-size debug-info)
                                          :stream s))
        (sb-di::compiled-debug-fun
         (let ((component (sb-di::compiled-debug-fun-component debug-info)))
           (sb-disassem::disassemble-code-component component :stream s)))
        (t `(:error "No disassembly available"))))))

(defslimefun swank-sprof-source-location (index)
  (let* ((node (gethash index *number-nodes*))
         (debug-info (sb-sprof::node-debug-info node)))
    (or (when (typep debug-info 'sb-di::compiled-debug-fun)
          (let* ((component (sb-di::compiled-debug-fun-component debug-info))
                 (function (sb-kernel::%code-entry-points component)))
            (when function
              (find-source-location function))))
        `(:error "No source location available"))))

(defslimefun swank-sprof-start (&key (mode :cpu))
  (sb-sprof:start-profiling :mode mode))

(defslimefun swank-sprof-stop ()
  (sb-sprof:stop-profiling))

)

(provide :swank-sprof)
