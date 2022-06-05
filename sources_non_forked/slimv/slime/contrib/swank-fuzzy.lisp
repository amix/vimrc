;;; swank-fuzzy.lisp --- fuzzy symbol completion
;;
;; Authors: Brian Downing <bdowning@lavos.net>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;;
;; License: Public Domain
;;


(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-util)
  (swank-require :swank-c-p-c))

(defvar *fuzzy-duplicate-symbol-filter* :nearest-package
  "Specifies how fuzzy-matching handles \"duplicate\" symbols.
Possible values are :NEAREST-PACKAGE, :HOME-PACKAGE, :ALL, or a custom
function. See Fuzzy Completion in the manual for details.")

(export '*fuzzy-duplicate-symbol-filter*)

;;; For nomenclature of the fuzzy completion section, please read
;;; through the following docstring.

(defslimefun fuzzy-completions (string default-package-name
                                &key limit time-limit-in-msec)
"Returns a list of two values:

  An (optionally limited to LIMIT best results) list of fuzzy
  completions for a symbol designator STRING. The list will be
  sorted by score, most likely match first.

  A flag that indicates whether or not TIME-LIMIT-IN-MSEC has
  been exhausted during computation. If that parameter's value is
  NIL or 0, no time limit is assumed.

The main result is a list of completion objects, where a completion
object is:

    (COMPLETED-STRING SCORE (&rest CHUNKS) CLASSIFICATION-STRING)

where a CHUNK is a description of a matched substring:

    (OFFSET SUBSTRING)

and FLAGS is short string describing properties of the symbol (see
SYMBOL-CLASSIFICATION-STRING).

E.g., completing \"mvb\" in a package that uses COMMON-LISP would
return something like:

    ((\"multiple-value-bind\" 26.588236 ((0 \"m\") (9 \"v\") (15 \"b\"))
     (:FBOUNDP :MACRO))
     ...)

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

Which symbols are candidates for matching depends on the symbol
designator's format. The cases are as follows:
  FOO      - Symbols accessible in the buffer package.
  PKG:FOO  - Symbols external in package PKG.
  PKG::FOO - Symbols accessible in package PKG."
  ;; For Emacs we allow both NIL and 0 as value of TIME-LIMIT-IN-MSEC
  ;; to denote an infinite time limit. Internally, we only use NIL for
  ;; that purpose, to be able to distinguish between "no time limit
  ;; alltogether" and "current time limit already exhausted." So we've
  ;; got to canonicalize its value at first:
  (let* ((no-time-limit-p (or (not time-limit-in-msec)
                              (zerop time-limit-in-msec)))
         (time-limit (if no-time-limit-p nil time-limit-in-msec)))
    (multiple-value-bind (completion-set interrupted-p)
        (fuzzy-completion-set string default-package-name :limit limit
                              :time-limit-in-msec time-limit)
      ;; We may send this as elisp [] arrays to spare a coerce here,
      ;; but then the network serialization were slower by handling arrays.
      ;; Instead we limit the number of completions that is transferred
      ;; (the limit is set from Emacs.)
      (list (coerce completion-set 'list) interrupted-p))))


;;; A Fuzzy Matching -- Not to be confused with a fuzzy completion
;;; object that will be sent back to Emacs, as described above.

(defstruct (fuzzy-matching (:conc-name fuzzy-matching.)
                           (:predicate fuzzy-matching-p)
                           (:constructor make-fuzzy-matching
                               (symbol package-name score package-chunks
                                symbol-chunks &key (symbol-p t))))
  symbol            ; The symbol that has been found to match.
  symbol-p          ; To deffirentiate between completeing
                    ; package: and package:nil
  package-name      ; The name of the package where SYMBOL was found in.
                    ;  (This is not necessarily the same as the home-package
                    ;   of SYMBOL, because the SYMBOL can be internal to
                    ;   lots of packages; also think of package nicknames.)
  score             ; The higher the better SYMBOL is a match.
  package-chunks    ; Chunks pertaining to the package identifier of SYMBOL.
  symbol-chunks)    ; Chunks pertaining to SYMBOL's name.

(defun %fuzzy-extract-matching-info (fuzzy-matching user-input-string)
  (multiple-value-bind (_ user-package-name __ input-internal-p)
      (parse-completion-arguments user-input-string nil)
    (declare (ignore _ __))
    (with-struct (fuzzy-matching. score symbol package-name package-chunks
                                  symbol-chunks symbol-p)
        fuzzy-matching
      (let (symbol-name real-package-name internal-p)
        (cond (symbol-p ; symbol fuzzy matching?
               (setf symbol-name (symbol-name symbol))
               (setf internal-p input-internal-p)
               (setf real-package-name (cond ((keywordp symbol)     "")
                                             ((not user-package-name) nil)
                                             (t package-name))))
              (t      ; package fuzzy matching?
               (setf symbol-name "")
               (setf real-package-name package-name)
               ;; If no explicit package name was given by the user
               ;; (e.g. input was "asdf"), we want to append only
               ;; one colon ":" to the package names.
               (setf internal-p (if user-package-name input-internal-p nil))))
        (values symbol-name
                real-package-name
                (if user-package-name internal-p nil)
                (completion-output-symbol-converter user-input-string)
                (completion-output-package-converter user-input-string))))))

(defun fuzzy-format-matching (fuzzy-matching user-input-string)
  "Returns the completion (\"foo:bar\") that's represented by FUZZY-MATCHING."
  (multiple-value-bind (symbol-name package-name internal-p
                        symbol-converter package-converter)
      (%fuzzy-extract-matching-info fuzzy-matching user-input-string)
    (setq symbol-name (and symbol-name
                           (funcall symbol-converter symbol-name)))
    (setq package-name (and package-name
                            (funcall package-converter package-name)))
    (let ((result (untokenize-symbol package-name internal-p symbol-name)))
      ;; We return the length of the possibly added prefix as second value.
      (values result (search symbol-name result)))))

(defun fuzzy-convert-matching-for-emacs (fuzzy-matching user-input-string)
  "Converts a result from the fuzzy completion core into something
that emacs is expecting.  Converts symbols to strings, fixes case
issues, and adds information (as a string) describing if the symbol is
bound, fbound, a class, a macro, a generic-function, a
special-operator, or a package."
  (with-struct (fuzzy-matching. symbol score package-chunks symbol-chunks
                                symbol-p)
               fuzzy-matching
    (multiple-value-bind (name added-length)
        (fuzzy-format-matching fuzzy-matching user-input-string)
      (list name
            (format nil "~,2f" score)
            (append package-chunks
                    (mapcar (lambda (chunk)
                              ;; Fix up chunk positions to account for possible
                              ;; added package identifier.
                              (let ((offset (first chunk))
                                    (string (second chunk)))
                                (list (+ added-length offset) string)))
                            symbol-chunks))
            (if symbol-p
                (symbol-classification-string symbol)
                "-------p")))))

(defun fuzzy-completion-set (string default-package-name
                             &key limit time-limit-in-msec)
  "Returns two values: an array of completion objects, sorted by
their score, that is how well they are a match for STRING
according to the fuzzy completion algorithm.  If LIMIT is set,
only the top LIMIT results will be returned. Additionally, a flag
is returned that indicates whether or not TIME-LIMIT-IN-MSEC was
exhausted."
  (check-type limit (or null (integer 0 #.(1- most-positive-fixnum))))
  (check-type time-limit-in-msec
              (or null (integer 0 #.(1- most-positive-fixnum))))
  (multiple-value-bind (matchings interrupted-p)
      (fuzzy-generate-matchings string default-package-name time-limit-in-msec)
    (when (and limit
               (> limit 0)
               (< limit (length matchings)))
      (if (array-has-fill-pointer-p matchings)
          (setf (fill-pointer matchings) limit)
          (setf matchings (make-array limit :displaced-to matchings))))
    (map-into matchings #'(lambda (m)
                            (fuzzy-convert-matching-for-emacs m string))
              matchings)
    (values matchings interrupted-p)))


(defun fuzzy-generate-matchings (string default-package-name
                                 time-limit-in-msec)
  "Does all the hard work for FUZZY-COMPLETION-SET. If
TIME-LIMIT-IN-MSEC is NIL, an infinite time limit is assumed."
  (multiple-value-bind (parsed-symbol-name parsed-package-name
                        package internal-p)
      (parse-completion-arguments string default-package-name)
    (flet ((fix-up (matchings parent-package-matching)
             ;; The components of each matching in MATCHINGS have been computed
             ;; relatively to PARENT-PACKAGE-MATCHING. Make them absolute.
             (let* ((p parent-package-matching)
                    (p.name   (fuzzy-matching.package-name p))
                    (p.score  (fuzzy-matching.score p))
                    (p.chunks (fuzzy-matching.package-chunks p)))
               (map-into
                matchings
                (lambda (m)
                  (let ((m.score (fuzzy-matching.score m)))
                    (setf (fuzzy-matching.package-name m) p.name)
                    (setf (fuzzy-matching.package-chunks m) p.chunks)
                    (setf (fuzzy-matching.score m)
                          (if (equal parsed-symbol-name "")
                              ;; Make package matchings be sorted before all
                              ;; the relative symbol matchings while preserving
                              ;; over all orderness.
                              (/ p.score 100)
                              (+ p.score m.score)))
                    m))
                matchings)))
           (find-symbols (designator package time-limit &optional filter)
             (fuzzy-find-matching-symbols designator package
                                          :time-limit-in-msec time-limit
                                          :external-only (not internal-p)
                                          :filter (or filter #'identity)))
           (find-packages (designator time-limit)
             (fuzzy-find-matching-packages designator
                                           :time-limit-in-msec time-limit))
           (maybe-find-local-package (name)
             (or (find-locally-nicknamed-package name *buffer-package*)
                 (find-package name))))
      (let ((time-limit time-limit-in-msec) (symbols) (packages) (results)
            (dedup-table (make-hash-table :test #'equal)))
        (cond ((not parsed-package-name) ; E.g. STRING = "asd"
               ;; We don't know if user is searching for a package or a symbol
               ;; within his current package. So we try to find either.
               (setf (values packages time-limit)
                     (find-packages parsed-symbol-name time-limit))
               (setf (values symbols  time-limit)
                     (find-symbols parsed-symbol-name package time-limit)))
              ((string= parsed-package-name "") ; E.g. STRING = ":" or ":foo"
               (setf (values symbols time-limit)
                     (find-symbols parsed-symbol-name package time-limit)))
              (t                   ; E.g. STRING = "asd:" or "asd:foo"
               ;; Find fuzzy matchings of the denoted package identifier part.
               ;; After that, find matchings for the denoted symbol identifier
               ;; relative to all the packages found.
               (multiple-value-bind (symbol-packages rest-time-limit)
                   (find-packages parsed-package-name time-limit-in-msec)
                 ;; We want to traverse the found packages in the order of
                 ;; their score, since those with higher score presumably
                 ;; represent better choices.  (This is important because some
                 ;; packages may never be looked at if time limit exhausts
                 ;; during traversal.)
                 (setf symbol-packages
                       (sort symbol-packages #'fuzzy-matching-greaterp))
                 (loop
                   for package-matching across symbol-packages
                   for package = (maybe-find-local-package
                                  (fuzzy-matching.package-name
                                   package-matching))
                   while (or (not time-limit) (> rest-time-limit 0)) do
                   (multiple-value-bind (matchings remaining-time)
                       ;; The duplication filter removes all those symbols
                       ;; which are present in more than one package
                       ;; match. See *FUZZY-DUPLICATE-SYMBOL-FILTER*
                       (find-symbols parsed-symbol-name package rest-time-limit
                                     (%make-duplicate-symbols-filter
                                      package-matching symbol-packages dedup-table))
                     (setf matchings (fix-up matchings package-matching))
                     (setf symbols   (concatenate 'vector symbols matchings))
                     (setf rest-time-limit remaining-time)
                     (let ((guessed-sort-duration
                             (%guess-sort-duration (length symbols))))
                       (when (and rest-time-limit
                                  (<= rest-time-limit guessed-sort-duration))
                         (decf rest-time-limit guessed-sort-duration)
                         (loop-finish))))
                   finally
                   (setf time-limit rest-time-limit)
                   (when (equal parsed-symbol-name "") ; E.g. STRING = "asd:"
                     (setf packages symbol-packages))))))
        ;; Sort by score; thing with equal score, sort alphabetically.
        ;; (Especially useful when PARSED-SYMBOL-NAME is empty, and all
        ;; possible completions are to be returned.)
        (setf results (concatenate 'vector symbols packages))
        (setf results (sort results #'fuzzy-matching-greaterp))
        (values results (and time-limit (<= time-limit 0)))))))

(defun %guess-sort-duration (length)
  ;; These numbers are pretty much arbitrary, except that they're
  ;; vaguely correct on my machine with SBCL. Yes, this is an ugly
  ;; kludge, but it's better than before (where this didn't exist at
  ;; all, which essentially meant, that this was taken to be 0.)
  (if (zerop length)
      0
      (let ((comparasions (* 3.8 (* length (log length 2)))))
        (* 1000 (* comparasions (expt 10 -7)))))) ; msecs

(defun %make-duplicate-symbols-filter (current-package-matching fuzzy-package-matchings dedup-table)
  ;; Returns a filter function based on *FUZZY-DUPLICATE-SYMBOL-FILTER*.
  (case *fuzzy-duplicate-symbol-filter*
    (:home-package
     ;; Return a filter function that takes a symbol, and which returns T
     ;; if and only if /no/ matching in FUZZY-PACKAGE-MATCHINGS represents
     ;; the home-package of the symbol passed.
     (let ((packages (mapcar #'(lambda (m)
                                 (find-package (fuzzy-matching.package-name m)))
                             (remove current-package-matching
                                     (coerce fuzzy-package-matchings 'list)))))
       #'(lambda (symbol)
           (not (member (symbol-package symbol) packages)))))
    (:nearest-package
     ;; Keep only the first occurence of the symbol.
     #'(lambda (symbol)
         (unless (gethash (symbol-name symbol) dedup-table)
           (setf (gethash (symbol-name symbol) dedup-table) t))))
    (:all
     ;; No filter
     #'identity)
    (t
     (typecase *fuzzy-duplicate-symbol-filter*
       (function
        ;; Custom filter
        (funcall *fuzzy-duplicate-symbol-filter*
                 (fuzzy-matching.package-name current-package-matching)
                 (map 'list #'fuzzy-matching.package-name fuzzy-package-matchings)
                 dedup-table))
       (t
        ;; Bad filter value
        (warn "bad *FUZZY-DUPLICATE-SYMBOL-FILTER* value: ~s"
              *fuzzy-duplicate-symbol-filter*)
        #'identity)))))

(defun fuzzy-matching-greaterp (m1 m2)
  "Returns T if fuzzy-matching M1 should be sorted before M2.
Basically just the scores of the two matchings are compared, and
the match with higher score wins. For the case that the score is
equal, the one which comes alphabetically first wins."
  (declare (type fuzzy-matching m1 m2))
  (let ((score1 (fuzzy-matching.score m1))
        (score2 (fuzzy-matching.score m2)))
    (cond ((> score1 score2) t)
          ((< score1 score2) nil)       ; total order
          (t
           (let ((name1 (symbol-name (fuzzy-matching.symbol m1)))
                 (name2 (symbol-name (fuzzy-matching.symbol m2))))
             (string< name1 name2))))))

(declaim (ftype (function () (integer 0)) get-real-time-msecs))
(defun get-real-time-in-msecs ()
  (let ((units-per-msec (max 1 (floor internal-time-units-per-second 1000))))
    (values (floor (get-internal-real-time) units-per-msec))))

(defun fuzzy-find-matching-symbols
    (string package &key (filter #'identity) external-only time-limit-in-msec)
  "Returns two values: a vector of fuzzy matchings for matching
symbols in PACKAGE, using the fuzzy completion algorithm, and the
remaining time limit.

Only those symbols are considered of which FILTER does return T.

If EXTERNAL-ONLY is true, only external symbols are considered. A
TIME-LIMIT-IN-MSEC of NIL is considered no limit; if it's zero or
negative, perform a NOP."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (package-name (package-name package))
        (count 0))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type (integer 0 #.(1- most-positive-fixnum)) count))

    (flet ((recompute-remaining-time (old-remaining-time)
             (cond ((not time-limit-p)
                    ;; propagate NIL back as infinite time limit
                    (values nil nil))
                   ((> count 0) ; ease up on getting internal time like crazy
                    (setf count (mod (1+ count) 128))
                    (values nil old-remaining-time))
                   (t (let* ((elapsed-time (- (get-real-time-in-msecs)
                                              rtime-at-start))
                             (remaining (- time-limit elapsed-time)))
                        (values (<= remaining 0) remaining)))))
           (perform-fuzzy-match (string symbol-name)
             (let* ((converter (completion-output-symbol-converter string))
                    (converted-symbol-name (funcall converter symbol-name)))
               (compute-highest-scoring-completion string
                                                   converted-symbol-name))))
      (let ((completions (make-array 256 :adjustable t :fill-pointer 0))
            (rest-time-limit time-limit))
        (do-symbols* (symbol package)
          (multiple-value-bind (exhausted? remaining-time)
              (recompute-remaining-time rest-time-limit)
            (setf rest-time-limit remaining-time)
            (cond (exhausted? (return))
                  ((not (and (or (not external-only)
                                 (symbol-external-p symbol package))
                             (funcall filter symbol))))
                  ((string= "" string) ; "" matches always
                   (vector-push-extend
                    (make-fuzzy-matching symbol package-name
                                         0.0 '() '())
                    completions))
                  (t
                   (multiple-value-bind (match-result score)
                       (perform-fuzzy-match string (symbol-name symbol))
                     (when match-result
                       (vector-push-extend
                        (make-fuzzy-matching symbol package-name score
                                             '() match-result)
                        completions)))))))
        (values completions rest-time-limit)))))

(defun fuzzy-find-matching-packages (name &key time-limit-in-msec)
  "Returns a vector of fuzzy matchings for each package that is
similiar to NAME, and the remaining time limit.
Cf. FUZZY-FIND-MATCHING-SYMBOLS."
  (let ((time-limit-p (and time-limit-in-msec t))
        (time-limit (or time-limit-in-msec 0))
        (rtime-at-start (get-real-time-in-msecs))
        (converter (completion-output-package-converter name))
        (completions (make-array 32 :adjustable t :fill-pointer 0)))
    (declare (type boolean time-limit-p))
    (declare (type integer time-limit rtime-at-start))
    (declare (type function converter))
    (flet ((match-package (names)
             (loop with max-pkg-name = ""
                   with max-result   = nil
                   with max-score    = 0
                   for package-name in names
                   for converted-name = (funcall converter package-name)
                   do
                   (multiple-value-bind (result score)
                       (compute-highest-scoring-completion name
                                                           converted-name)
                     (when (and result (> score max-score))
                       (setf max-pkg-name package-name)
                       (setf max-result   result)
                       (setf max-score    score)))
                   finally
                   (when max-result
                     (vector-push-extend
                      (make-fuzzy-matching nil max-pkg-name
                                           max-score max-result '()
                                           :symbol-p nil)
                      completions)))))
     (cond ((and time-limit-p (<= time-limit 0))
            (values #() time-limit))
           (t
            (loop for (nick) in (package-local-nicknames *buffer-package*)
                  do
                  (match-package (list nick)))
            (loop for package in (list-all-packages)
                  do
                  ;; Find best-matching package-nickname:
                  (match-package (package-names package))
                  finally
                  (return
                    (values completions
                            (and time-limit-p
                                 (let ((elapsed-time (- (get-real-time-in-msecs)
                                                        rtime-at-start)))
                                   (- time-limit elapsed-time)))))))))))


(defslimefun fuzzy-completion-selected (original-string completion)
  "This function is called by Slime when a fuzzy completion is
selected by the user.  It is for future expansion to make
testing, say, a machine learning algorithm for completion scoring
easier.

ORIGINAL-STRING is the string the user completed from, and
COMPLETION is the completion object (see docstring for
SWANK:FUZZY-COMPLETIONS) corresponding to the completion that the
user selected."
  (declare (ignore original-string completion))
  nil)


;;;;; Fuzzy completion core

(defparameter *fuzzy-recursion-soft-limit* 30
  "This is a soft limit for recursion in
RECURSIVELY-COMPUTE-MOST-COMPLETIONS.  Without this limit,
completing a string such as \"ZZZZZZ\" with a symbol named
\"ZZZZZZZZZZZZZZZZZZZZZZZ\" will result in explosive recursion to
find all the ways it can match.

Most natural language searches and symbols do not have this
problem -- this is only here as a safeguard.")
(declaim (fixnum *fuzzy-recursion-soft-limit*))

(defvar *all-chunks* '())
(declaim (type list *all-chunks*))

(defun compute-highest-scoring-completion (short full)
  "Finds the highest scoring way to complete the abbreviation
SHORT onto the string FULL, using CHAR= as a equality function for
letters.  Returns two values:  The first being the completion
chunks of the highest scorer, and the second being the score."
  (let* ((scored-results
          (mapcar #'(lambda (result)
                      (cons (score-completion result short full) result))
                  (compute-most-completions short full)))
         (winner (first (sort scored-results #'> :key #'first))))
    (values (rest winner) (first winner))))

(defun compute-most-completions (short full)
  "Finds most possible ways to complete FULL with the letters in SHORT.
Calls RECURSIVELY-COMPUTE-MOST-COMPLETIONS recursively.  Returns
a list of (&rest CHUNKS), where each CHUNKS is a description of
how a completion matches."
  (let ((*all-chunks* nil))
    (recursively-compute-most-completions short full 0 0 nil nil nil t)
    *all-chunks*))

(defun recursively-compute-most-completions
    (short full
     short-index initial-full-index
     chunks current-chunk current-chunk-pos
     recurse-p)
  "Recursively (if RECURSE-P is true) find /most/ possible ways
to fuzzily map the letters in SHORT onto FULL, using CHAR= to
determine if two letters match.

A chunk is a list of elements that have matched consecutively.
When consecutive matches stop, it is coerced into a string,
paired with the starting position of the chunk, and pushed onto
CHUNKS.

Whenever a letter matches, if RECURSE-P is true,
RECURSIVELY-COMPUTE-MOST-COMPLETIONS calls itself with a position
one index ahead, to find other possibly higher scoring
possibilities.  If there are less than
*FUZZY-RECURSION-SOFT-LIMIT* results in *ALL-CHUNKS* currently,
this call will also recurse.

Once a word has been completely matched, the chunks are pushed
onto the special variable *ALL-CHUNKS* and the function returns."
  (declare (optimize speed)
           (type fixnum short-index initial-full-index)
           (type list current-chunk)
           (simple-string short full))
  (flet ((short-cur ()
           "Returns the next letter from the abbreviation, or NIL
            if all have been used."
           (if (= short-index (length short))
               nil
               (aref short short-index)))
         (add-to-chunk (char pos)
           "Adds the CHAR at POS in FULL to the current chunk,
            marking the start position if it is empty."
           (unless current-chunk
             (setf current-chunk-pos pos))
           (push char current-chunk))
         (collect-chunk ()
           "Collects the current chunk to CHUNKS and prepares for
            a new chunk."
           (when current-chunk
             (let ((current-chunk-as-string
                     (nreverse
                      (make-array (length current-chunk)
                                  :element-type 'character
                                  :initial-contents current-chunk))))
               (push (list current-chunk-pos current-chunk-as-string) chunks)
               (setf current-chunk nil
                     current-chunk-pos nil)))))
    ;; If there's an outstanding chunk coming in collect it.  Since
    ;; we're recursively called on skipping an input character, the
    ;; chunk can't possibly continue on.
    (when current-chunk (collect-chunk))
    (do ((pos initial-full-index (1+ pos)))
        ((= pos (length full)))
      (let ((cur-char (aref full pos)))
        (if (and (short-cur)
                 (char= cur-char (short-cur)))
            (progn
              (when recurse-p
                ;; Try other possibilities, limiting insanely deep
                ;; recursion somewhat.
                (recursively-compute-most-completions
                 short full short-index (1+ pos)
                 chunks current-chunk current-chunk-pos
                 (not (> (length *all-chunks*)
                         *fuzzy-recursion-soft-limit*))))
              (incf short-index)
              (add-to-chunk cur-char pos))
            (collect-chunk))))
    (collect-chunk)
    ;; If we've exhausted the short characters we have a match.
    (if (short-cur)
        nil
        (let ((rev-chunks (reverse chunks)))
          (push rev-chunks *all-chunks*)
          rev-chunks))))


;;;;; Fuzzy completion scoring

(defvar *fuzzy-completion-symbol-prefixes* "*+-%&?<"
  "Letters that are likely to be at the beginning of a symbol.
Letters found after one of these prefixes will be scored as if
they were at the beginning of ths symbol.")
(defvar *fuzzy-completion-symbol-suffixes* "*+->"
  "Letters that are likely to be at the end of a symbol.
Letters found before one of these suffixes will be scored as if
they were at the end of the symbol.")
(defvar *fuzzy-completion-word-separators* "-/."
  "Letters that separate different words in symbols.  Letters
after one of these symbols will be scores more highly than other
letters.")

(defun score-completion (completion short full)
  "Scores the completion chunks COMPLETION as a completion from
the abbreviation SHORT to the full string FULL.  COMPLETION is a
list like:
    ((0 \"mul\") (9 \"v\") (15 \"b\"))
Which, if SHORT were \"mulvb\" and full were \"multiple-value-bind\",
would indicate that it completed as such (completed letters
capitalized):
    MULtiple-Value-Bind

Letters are given scores based on their position in the string.
Letters at the beginning of a string or after a prefix letter at
the beginning of a string are scored highest.  Letters after a
word separator such as #\- are scored next highest.  Letters at
the end of a string or before a suffix letter at the end of a
string are scored medium, and letters anywhere else are scored
low.

If a letter is directly after another matched letter, and its
intrinsic value in that position is less than a percentage of the
previous letter's value, it will use that percentage instead.

Finally, a small scaling factor is applied to favor shorter
matches, all other things being equal."
  (labels ((at-beginning-p (pos)
             (= pos 0))
           (after-prefix-p (pos)
             (and (= pos 1)
                  (find (aref full 0) *fuzzy-completion-symbol-prefixes*)))
           (word-separator-p (pos)
             (find (aref full pos) *fuzzy-completion-word-separators*))
           (after-word-separator-p (pos)
             (find (aref full (1- pos)) *fuzzy-completion-word-separators*))
           (at-end-p (pos)
             (= pos (1- (length full))))
           (before-suffix-p (pos)
             (and (= pos (- (length full) 2))
                  (find (aref full (1- (length full)))
                        *fuzzy-completion-symbol-suffixes*)))
           (score-or-percentage-of-previous (base-score pos chunk-pos)
             (if (zerop chunk-pos)
                 base-score
                 (max base-score
                      (+ (* (score-char (1- pos) (1- chunk-pos)) 0.85)
                         (expt 1.2 chunk-pos)))))
           (score-char (pos chunk-pos)
             (score-or-percentage-of-previous
              (cond ((at-beginning-p pos)         10)
                    ((after-prefix-p pos)         10)
                    ((word-separator-p pos)       1)
                    ((after-word-separator-p pos) 8)
                    ((at-end-p pos)               6)
                    ((before-suffix-p pos)        6)
                    (t                            1))
              pos chunk-pos))
           (score-chunk (chunk)
             (loop for chunk-pos below (length (second chunk))
                   for pos from (first chunk)
                   summing (score-char pos chunk-pos))))
    (let* ((chunk-scores (mapcar #'score-chunk completion))
           (length-score (/ 10.0 (1+ (- (length full) (length short))))))
      (values
       (+ (reduce #'+ chunk-scores) length-score)
       (list (mapcar #'list chunk-scores completion) length-score)))))

(defun highlight-completion (completion full)
  "Given a chunk definition COMPLETION and the string FULL,
HIGHLIGHT-COMPLETION will create a string that demonstrates where
the completion matched in the string.  Matches will be
capitalized, while the rest of the string will be lower-case."
  (let ((highlit (nstring-downcase (copy-seq full))))
    (dolist (chunk completion)
      (setf highlit (nstring-upcase highlit
                                    :start (first chunk)
                                    :end (+ (first chunk)
                                            (length (second chunk))))))
    highlit))

(defun format-fuzzy-completion-set (winners)
  "Given a list of completion objects such as on returned by
FUZZY-COMPLETION-SET, format the list into user-readable output
for interactive debugging purpose."
  (let ((max-len
         (loop for winner in winners maximizing (length (first winner)))))
    (loop for (sym score result) in winners do
          (format t "~&~VA  score ~8,2F  ~A"
                  max-len (highlight-completion result sym) score result))))

(provide :swank-fuzzy)
