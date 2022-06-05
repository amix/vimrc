;;
;;  SELECT-MATCH macro (and IN macro)
;;
;; Copyright 1990   Stephen Adams
;;
;; You are free to copy, distribute and make derivative works of this
;; source provided that this copyright notice is displayed near the
;; beginning of the file.  No liability is accepted for the
;; correctness or performance of the code.  If you modify the code
;; please indicate this fact both at the place of modification and in
;; this copyright message.
;;
;;   Stephen Adams
;;   Department of Electronics and Computer Science
;;   University of Southampton
;;   SO9 5NH, UK
;;
;; sra@ecs.soton.ac.uk
;;

;;
;;  Synopsis:
;;
;;  (select-match expression
;;      (pattern  action+)*)
;;
;;      --- or ---
;;
;;  (select-match expression
;;      pattern => expression
;;      pattern => expression
;;      ...)
;;
;;  pattern ->  constant		;egs  1, #\x, #c(1.0 1.1)
;;          |   symbol                  ;matches anything
;;          |   'anything               ;must be EQUAL
;;          |   (pattern = pattern)     ;both patterns must match
;;          |   (#'function pattern)    ;predicate test
;;          |   (pattern . pattern)	;cons cell
;;

;;  Example
;;
;;  (select-match item
;;      (('if e1 e2 e3) 'if-then-else)				;(1)
;;      ((#'oddp k)     'an-odd-integer)			;(2)
;;      (((#'treep tree) = (hd . tl))   'something-else)	;(3)
;;      (other          'anything-else))			;(4)
;;
;;  Notes
;;
;;  .   Each pattern is tested in turn.  The first match is taken.
;;
;;  .   If no pattern matches, an error is signalled.
;;
;;  .   Constant patterns (things X for which (CONSTANTP X) is true, i.e.
;;      numbers, strings, characters, etc.) match things which are EQUAL.
;;
;;  .   Quoted patterns (which are CONSTANTP) are constants.
;;
;;  .   Symbols match anything. The symbol is bound to the matched item
;;      for the execution of the actions.
;;      For example, (SELECT-MATCH '(1 2 3)
;;                      (1 . X) => X)
;;      returns (2 3) because X is bound to the cdr of the candidate.
;;
;;  .   The two pattern match (p1 = p2) can be used to name parts
;;      of the matched structure.  For example, (ALL = (HD . TL))
;;      matches a cons cell. ALL is bound to the cons cell, HD to its car
;;      and TL to its tail.
;;
;;  .   A predicate test applies the predicate to the item being matched.
;;      If the predicate returns NIL then the match fails.
;;      If it returns truth, then the nested pattern is matched.  This is
;;      often just a symbol like K in the example.
;;
;;  .   Care should be taken with the domain values for predicate matches.
;;      If, in the above eg, item is not an integer, an error would occur
;;      during the test.  A safer pattern would be
;;          (#'integerp (#'oddp k))
;;      This would only test for oddness of the item was an integer.
;;
;;  .   A single symbol will match anything so it can be used as a default
;;      case, like OTHER above.
;;

(in-package swank/match)

(defmacro match (expression &body patterns)
  `(select-match ,expression ,@patterns))

(defmacro select-match (expression &rest patterns)
  (let* ((do-let (not (atom expression)))
         (key    (if do-let (gensym) expression))
         (cbody  (expand-select-patterns key patterns))
         (cform  `(cond . ,cbody)))
    (if do-let
        `(let ((,key ,expression)) ,cform)
        cform)))

(defun expand-select-patterns (key patterns)
  (if (eq (second patterns) '=>)
      (expand-select-patterns-style-2 key patterns)
      (expand-select-patterns-style-1 key patterns)))

(defun expand-select-patterns-style-1 (key patterns)
  (if (null patterns)
      `((t (error "Case select pattern match failure on ~S" ,key)))
      (let* ((pattern  (caar patterns))
             (actions  (cdar patterns))
             (rest     (cdr patterns))
             (test     (compile-select-test key pattern))
             (bindings (compile-select-bindings key pattern actions)))
        `(,(if bindings `(,test (let ,bindings . ,actions))
               `(,test . ,actions))
           . ,(unless (eq test t)
                (expand-select-patterns-style-1 key rest))))))

(defun expand-select-patterns-style-2 (key patterns)
  (cond ((null patterns)
         `((t (error "Case select pattern match failure on ~S" ,key))))
        (t (when (or (< (length patterns) 3)
                     (not (eq (second patterns) '=>)))
             (error "Illegal patterns: ~S" patterns))
           (let* ((pattern  (first patterns))
                  (actions  (list (third patterns)))
                  (rest     (cdddr patterns))
                  (test     (compile-select-test key pattern))
                  (bindings (compile-select-bindings key pattern actions)))
             `(,(if bindings `(,test (let ,bindings . ,actions))
                    `(,test . ,actions))
                . ,(unless (eq test t)
                     (expand-select-patterns-style-2 key rest)))))))

(defun compile-select-test (key pattern)
  (let ((tests (remove t (compile-select-tests key pattern))))
    (cond
      ;; note AND does this anyway, but this allows us to tell if
      ;; the pattern will always match.
      ((null tests)         t)
      ((= (length tests) 1) (car tests))
      (t                    `(and . ,tests)))))

(defun compile-select-tests (key pattern)
  (cond ((constantp pattern)   `((,(cond ((numberp pattern) 'eql)
                                         ((symbolp pattern) 'eq)
                                         (t                'equal))
                                   ,key ,pattern)))
        ((symbolp pattern)      '(t))
        ((select-double-match? pattern)
         (append
          (compile-select-tests key (first pattern))
          (compile-select-tests key (third pattern))))
        ((select-predicate? pattern)
         (append
          `((,(second (first pattern)) ,key))
          (compile-select-tests key (second pattern))))
        ((consp pattern)
         (append
          `((consp ,key))
          (compile-select-tests (cs-car key) (car
                                               pattern))
          (compile-select-tests (cs-cdr key) (cdr
                                               pattern))))
        (t (error "Illegal select pattern: ~S" pattern))))


(defun compile-select-bindings (key pattern action)
  (cond ((constantp pattern) '())
        ((symbolp pattern)
         (if (select-in-tree pattern action)
             `((,pattern ,key))
             '()))
        ((select-double-match? pattern)
         (append
          (compile-select-bindings key (first pattern) action)
          (compile-select-bindings key (third pattern) action)))
        ((select-predicate? pattern)
         (compile-select-bindings key (second pattern) action))
        ((consp pattern)
         (append
          (compile-select-bindings (cs-car key) (car pattern)
                                   action)
          (compile-select-bindings (cs-cdr key) (cdr pattern)
                                   action)))))

(defun select-in-tree (atom tree)
  (or (eq atom tree)
      (if (consp tree)
          (or (select-in-tree atom (car tree))
              (select-in-tree atom (cdr tree))))))

(defun select-double-match? (pattern)
  ;;  (<pattern> = <pattern>)
  (and (consp pattern) (consp (cdr pattern)) (consp (cddr pattern))
       (null (cdddr pattern))
       (eq (second pattern) '=)))

(defun select-predicate? (pattern)
  ;; ((function <f>) <pattern>)
  (and (consp pattern)
       (consp (cdr pattern))
       (null (cddr pattern))
       (consp (first pattern))
       (consp (cdr (first pattern)))
       (null (cddr (first pattern)))
       (eq (caar pattern) 'function)))

(defun cs-car (exp)
  (cs-car/cdr 'car exp
               '((car . caar)     (cdr . cadr)    (caar . caaar) (cadr . caadr)
                 (cdar . cadar)   (cddr . caddr)
                 (caaar . caaaar) (caadr . caaadr) (cadar . caadar)
                 (caddr . caaddr) (cdaar . cadaar) (cdadr . cadadr)
                 (cddar . caddar) (cdddr . cadddr))))

(defun cs-cdr (exp)
  (cs-car/cdr 'cdr exp
               '((car . cdar)    (cdr . cddr)    (caar . cdaar)  (cadr . cdadr)
                 (cdar . cddar)  (cddr . cdddr)
                 (caaar . cdaaar)    (caadr . cdaadr)    (cadar . cdadar)
                 (caddr . cdaddr)    (cdaar . cddaar)    (cdadr . cddadr)
                 (cddar . cdddar)    (cdddr . cddddr))))

(defun cs-car/cdr (op exp table)
  (if (and (consp exp) (= (length exp) 2))
      (let ((replacement  (assoc (car exp) table)))
        (if replacement
            `(,(cdr replacement) ,(second exp))
            `(,op ,exp)))
      `(,op ,exp)))

;; (setf c1 '(select-match x (a 1) (b 2 3 4)))
;; (setf c2 '(select-match (car y)
;;             (1 (print 100) 101) (2 200) ("hello" 5) (:x 20) (else (1+
;;  else))))
;; (setf c3 '(select-match (caddr y)
;;             ((all = (x y)) (list x y all))
;;             ((a '= b)      (list 'assign a b))
;;             ((#'oddp k)     (1+ k)))))


