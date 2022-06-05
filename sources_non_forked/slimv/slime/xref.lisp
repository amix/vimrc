;;; -*- Mode: LISP; Package: XREF; Syntax: Common-lisp;  -*- 
;;; Mon Jan 21 16:21:20 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; xref.lisp

;;; ****************************************************************
;;; List Callers: A Static Analysis Cross Referencing Tool for Lisp 
;;; ****************************************************************
;;; 
;;; The List Callers system is a portable Common Lisp cross referencing
;;; utility. It grovels over a set of files and compiles a database of the
;;; locations of all references for each symbol used in the files.
;;; List Callers is similar to the Symbolics Who-Calls and the
;;; Xerox Masterscope facilities.
;;;
;;; When you change a function or variable definition, it can be useful
;;; to know its callers, in order to update each of them to the new
;;; definition. Similarly, having a graphic display of the structure 
;;; (e.g., call graph) of a program can help make undocumented code more
;;; understandable. This static code analyzer facilitates both capabilities.
;;; The database compiled by xref is suitable for viewing by a graphical 
;;; browser. (Note: the reference graph is not necessarily a DAG. Since many
;;; graphical browsers assume a DAG, this will lead to infinite loops.
;;; Some code which is useful in working around this problem is included,
;;; as well as a sample text-indenting outliner and an interface to Bates'
;;; PSGraph Postscript Graphing facility.) 
;;;
;;; Written by Mark Kantrowitz, July 1990.
;;;
;;; Address: School of Computer Science
;;;          Carnegie Mellon University
;;;          Pittsburgh, PA 15213
;;;
;;; Copyright (c) 1990. All rights reserved.
;;;
;;; See general license below.
;;;

;;; ****************************************************************
;;; General License Agreement and Lack of Warranty *****************
;;; ****************************************************************
;;;
;;; This software is distributed in the hope that it will be useful (both
;;; in and of itself and as an example of lisp programming), but WITHOUT
;;; ANY WARRANTY. The author(s) do not accept responsibility to anyone for
;;; the consequences of using it or for whether it serves any particular
;;; purpose or works at all. No warranty is made about the software or its
;;; performance. 
;;; 
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;;	o  The copyright notice and this entire notice are included intact
;;;        and prominently carried on all copies and supporting documentation.
;;;	o  No fees or compensation are charged for use, copies, or
;;;	   access to this software. You may charge a nominal
;;;	   distribution fee for the physical act of transferring a
;;;	   copy, but you may not charge for the program itself. 
;;;	o  If you modify this software, you must cause the modified
;;;	   file(s) to carry prominent notices (a Change Log)
;;;	   describing the changes, who made the changes, and the date
;;;	   of those changes.
;;;	o  Any work distributed or published that in whole or in part
;;;	   contains or is a derivative of this software or any part 
;;;	   thereof is subject to the terms of this agreement. The 
;;;	   aggregation of another unrelated program with this software
;;;	   or its derivative on a volume of storage or distribution
;;;	   medium does not bring the other program under the scope
;;;	   of these terms.
;;;	o  Permission is granted to manufacturers and distributors of
;;;	   lisp compilers and interpreters to include this software
;;;	   with their distribution. 
;;;
;;; This software is made available AS IS, and is distributed without 
;;; warranty of any kind, either expressed or implied.
;;; 
;;; In no event will the author(s) or their institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of or in connection
;;; with the use or inability to use (including but not limited to loss of
;;; data or data being rendered inaccurate or losses sustained by third
;;; parties or a failure of the program to operate as documented) the 
;;; program, even if you have been advised of the possibility of such
;;; damanges, or for any claim by any other party, whether in an action of
;;; contract, negligence, or other tortious action.
;;; 
;;; The current version of this software and a variety of related utilities
;;; may be obtained by anonymous ftp from ftp.cs.cmu.edu in the directory
;;;    user/ai/lang/lisp/code/tools/xref/
;;; 
;;; Please send bug reports, comments, questions and suggestions to
;;; mkant@cs.cmu.edu. We would also appreciate receiving any changes
;;; or improvements you may make. 
;;; 
;;; If you wish to be added to the Lisp-Utilities@cs.cmu.edu mailing list, 
;;; send email to Lisp-Utilities-Request@cs.cmu.edu with your name, email
;;; address, and affiliation. This mailing list is primarily for
;;; notification about major updates, bug fixes, and additions to the lisp
;;; utilities collection. The mailing list is intended to have low traffic.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; 27-FEB-91 mk   Added insert arg to psgraph-xref to allow the postscript
;;;                graphs to be inserted in Scribe documents.
;;; 21-FEB-91 mk   Added warning if not compiled.
;;; 07-FEB-91 mk   Fixed bug in record-callers with regard to forms at 
;;;                toplevel.
;;; 21-JAN-91 mk   Added file xref-test.lisp to test xref.
;;; 16-JAN-91 mk   Added definition WHO-CALLS to parallel the Symbolics syntax.
;;; 16-JAN-91 mk   Added macroexpansion capability to record-callers. Also
;;;                added parameter *handle-macro-forms*, defaulting to T.
;;; 16-JAN-91 mk   Modified print-caller-tree and related functions
;;;                to allow the user to specify root nodes. If the user
;;;                doesn't specify them, it will default to all root
;;;                nodes, as before. 
;;; 16-JAN-91 mk   Added parameter *default-graphing-mode* to specify
;;;                the direction of the graphing. Either :call-graph,
;;;                where the children of a node are those functions called
;;;                by the node, or :caller-graph where the children of a
;;;                node are the callers of the node. :call-graph is the
;;;                default.
;;; 16-JAN-91 mk   Added parameter *indent-amount* to control the indentation
;;;                in print-indented-tree.
;;; 16-JUL-90 mk   Functions with argument lists of () were being ignored
;;;                because of a (when form) wrapped around the body of
;;;                record-callers. Then intent of (when form) was as an extra
;;;                safeguard against infinite looping. This wasn't really
;;;                necessary, so it has been removed.
;;; 16-JUL-90 mk   PSGraph-XREF now has keyword arguments, instead of
;;;                optionals.
;;; 16-JUL-90 mk   Added PRINT-CLASS-HIERARCHY to use psgraph to graph the
;;;                CLOS class hierarchy. This really doesn't belong here,
;;;                and should be moved to psgraph.lisp as an example of how
;;;                to use psgraph.
;;; 16-JUL-90 mk   Fixed several caller patterns. The pattern for member
;;;                had an error which caused many references to be missed.
;;; 16-JUL-90 mk   Added ability to save/load processed databases.
;;;  5-JUL-91 mk    Fixed warning of needing compilation to occur only when the
;;;                 source is loaded.
;;; 20-SEP-93 mk    Added fix from Peter Norvig to allow Xref to xref itself.
;;;                 The arg to macro-function must be a symbol.
;;;  7-APR-12 heller  Break lines at 80 columns.

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; Verify that:
;;;    o  null forms don't cause it to infinite loop.
;;;    o  nil matches against null argument lists.
;;;    o  declarations and doc are being ignored.
;;;
;;; Would be nice if in addition to showing callers of a function, it
;;; displayed the context of the calls to the function (e.g., the
;;; immediately surrounding form). This entails storing entries of
;;; the form (symbol context*) in the database and augmenting
;;; record-callers to keep the context around. The only drawbacks is
;;; that it would cons a fair bit. If we do this, we should store
;;; additional information as well in the database, such as the caller
;;; pattern type (e.g., variable vs. function).
;;;
;;; Write a translator from BNF (at least as much of BNF as is used
;;; in CLtL2), to the format used here.
;;;
;;; Should automatically add new patterns for new functions and macros
;;; based on their arglists. Probably requires much more than this
;;; simple code walker, so there isn't much we can do.
;;;
;;; Defmacro is a problem, because it often hides internal function
;;; calls within backquote and quote, which we normally ignore. If
;;; we redefine QUOTE's pattern so that it treats the arg like a FORM,
;;; we'll probably get them (though maybe the syntax will be mangled),
;;; but most likely a lot of spurious things as well. 
;;;
;;; Define an operation for Defsystem which will run XREF-FILE on the
;;; files of the system. Or yet simpler, when XREF sees a LOAD form
;;; for which the argument is a string, tries to recursively call
;;; XREF-FILE on the specified file. Then one could just XREF-FILE
;;; the file which loads the system. (This should be a program
;;; parameter.)
;;;
;;; Have special keywords which the user may place in a file to have
;;; XREF-FILE ignore a region.
;;;
;;; Should we distinguish flet and labels from defun? I.e., note that
;;; flet's definitions are locally defined, instead of just lumping
;;; them in with regular definitions.
;;;
;;; Add patterns for series, loop macro.
;;;
;;; Need to integrate the variable reference database with the other
;;; databases, yet maintain separation. So we can distinguish all
;;; the different types of variable and function references, without
;;; multiplying databases.
;;;
;;; Would pay to comment record-callers and record-callers* in more
;;; depth.
;;; 
;;; (&OPTIONAL &REST &KEY &AUX &BODY &WHOLE &ALLOW-OTHER-KEYS &ENVIRONMENT)

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    XREF has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;    
;;;    XREF has been tested (unsuccessfully) in the following lisps:
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;           - if interpreted, runs into stack overflow
;;;           - does not compile (tried ibcl on Suns, PMAXes and RTs)
;;;             seems to be due to a limitation in the c compiler.
;;;    
;;;    XREF needs to be tested in the following lisps:
;;;       Symbolics Common Lisp (8.0)
;;;       Lucid Common Lisp (3.0, 4.0)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       VAXLisp (2.0, 3.1)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp


;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; XREF analyzes a user's program, determining which functions call a
;;; given function, and the location of where variables are bound/assigned
;;; and used. The user may retrieve this information for either a single
;;; symbol, or display the call graph of portions of the program
;;; (including the entire program). This allows the programmer to debug
;;; and document the program's structure.
;;; 
;;; XREF is primarily intended for analyzing large programs, where it is
;;; difficult, if not impossible, for the programmer to grasp the structure
;;; of the whole program. Nothing precludes using XREF for smaller programs,
;;; where it can be useful for inspecting the relationships between pieces
;;; of the program and for documenting the program.
;;; 
;;; Two aspects of the Lisp programming language greatly simplify the
;;; analysis of Lisp programs:
;;; 	o  Lisp programs are naturally represented as data.
;;; 	   Successive definitions from a file are easily read in
;;; 	   as list structure.
;;; 	o  The basic syntax of Lisp is uniform. A list program
;;; 	   consists of a set of nested forms, where each form is
;;; 	   a list whose car is a tag (e.g., function name) that
;;; 	   specifies the structure of the rest of the form.
;;; Thus Lisp programs, when represented as data, can be considered to be
;;; parse trees. Given a grammar of syntax patterns for the language, XREF
;;; recursively descends the parse tree for a given definition, computing
;;; a set of relations that hold for the definition at each node in the
;;; tree. For example, one kind of relation is that the function defined
;;; by the definition calls the functions in its body. The relations are
;;; stored in a database for later examination by the user.
;;; 
;;; While XREF currently only works for programs written in Lisp, it could
;;; be extended to other programming languages by writing a function to
;;; generate parse trees for definitions in that language, and a core
;;; set of patterns for the language's syntax.
;;; 
;;; Since XREF normally does a static syntactic analysis of the program, 
;;; it does not detect references due to the expansion of a macro definition. 
;;; To do this in full generality XREF would have to have knowledge about the
;;; semantics of the program (e.g., macros which call other functions to
;;; do the expansion). This entails either modifying the compiler to
;;; record the relationships (e.g., Symbolics Who-Calls Database) or doing
;;; a walk of loaded code and macroexpanding as needed (PCL code walker).
;;; The former is not portable, while the latter requires that the code
;;; used by macros be loaded and in working order. On the other hand, then
;;; we would need no special knowledge about macros (excluding the 24 special
;;; forms of Lisp).
;;;
;;; Parameters may be set to enable macro expansion in XREF. Then XREF
;;; will expand any macros for which it does not have predefined patterns.
;;; (For example, most Lisps will implement dolist as a macro. Since XREF
;;; has a pattern defined for dolist, it will not call macroexpand-1 on
;;; a form whose car is dolist.) For this to work properly, the code must
;;; be loaded before being processed by XREF, and XREF's parameters should
;;; be set so that it processes forms in their proper packages. 
;;;
;;; If macro expansion is disabled, the default rules for handling macro
;;; references may not be sufficient for some user-defined macros, because
;;; macros allow a variety of non-standard syntactic extensions to the
;;; language. In this case, the user may specify additional templates in
;;; a manner similar to that in which the core Lisp grammar was specified.
;;;


;;; ********************************
;;; User Guide *********************
;;; ********************************
;;; -----
;;; The following functions are called to cross reference the source files.
;;;
;;; XREF-FILES (&rest files)                                      [FUNCTION]
;;;    Grovels over the lisp code located in source file FILES, using
;;;    xref-file.
;;;
;;; XREF-FILE (filename &optional clear-tables verbose)       [Function]
;;;    Cross references the function and variable calls in FILENAME by
;;;    walking over the source code located in the file. Defaults type of
;;;    filename to ".lisp". Chomps on the code using record-callers and
;;;    record-callers*. If CLEAR-TABLES is T (the default), it clears the
;;;    callers database before processing the file. Specify CLEAR-TABLES as
;;;    nil to append to the database. If VERBOSE is T (the default), prints
;;;    out the name of the file, one progress dot for each form processed,
;;;    and the total number of forms.
;;;
;;; -----
;;; The following functions display information about the uses of the 
;;; specified symbol as a function, variable, or constant.
;;;
;;; LIST-CALLERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which call SYMBOL as a function (function
;;;    invocation).
;;;
;;; LIST-READERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which refer to SYMBOL as a variable
;;;    (variable reference).
;;;
;;; LIST-SETTERS (symbol)                                         [FUNCTION]
;;;    Lists all functions which bind/set SYMBOL as a variable
;;;    (variable mutation).
;;;
;;; LIST-USERS (symbol)                                           [FUNCTION]
;;;    Lists all functions which use SYMBOL as a variable or function.
;;;
;;; WHO-CALLS (symbol &optional how)                              [FUNCTION]
;;;    Lists callers of symbol. HOW may be :function, :reader, :setter,
;;;    or :variable."
;;;
;;; WHAT-FILES-CALL (symbol)                                      [FUNCTION]
;;;    Lists names of files that contain uses of SYMBOL
;;;    as a function, variable, or constant.
;;;
;;; SOURCE-FILE (symbol)                                          [FUNCTION]
;;;    Lists the names of files in which SYMBOL is defined/used.
;;;
;;; LIST-CALLEES (symbol)                                         [FUNCTION]
;;;    Lists names of functions and variables called by SYMBOL.
;;;
;;; -----
;;; The following functions may be useful for viewing the database and
;;; debugging the calling patterns.
;;;
;;; *LAST-FORM* ()                                                [VARIABLE]
;;;    The last form read from the file. Useful for figuring out what went
;;;    wrong when xref-file drops into the debugger.
;;;
;;; *XREF-VERBOSE* t                                              [VARIABLE]
;;;    When T, xref-file(s) prints out the names of the files it looks at,
;;;    progress dots, and the number of forms read.
;;;
;;; *TYPES-TO-IGNORE* (quote (:lisp :lisp2))                      [VARIABLE]
;;;    Default set of caller types (as specified in the patterns) to ignore
;;;    in the database handling functions. :lisp is CLtL 1st edition,
;;;    :lisp2 is additional patterns from CLtL 2nd edition.
;;;
;;; *HANDLE-PACKAGE-FORMS* ()                                     [VARIABLE]
;;;    When non-NIL, and XREF-FILE sees a package-setting form like
;;;    IN-PACKAGE, sets the current package to the specified package by
;;;    evaluating the form. When done with the file, xref-file resets the
;;;    package to its original value. In some of the displaying functions,
;;;    when this variable is non-NIL one may specify that all symbols from a
;;;    particular set of packages be ignored. This is only useful if the
;;;    files use different packages with conflicting names.
;;;
;;; *HANDLE-FUNCTION-FORMS* t                                     [VARIABLE]
;;;    When T, XREF-FILE tries to be smart about forms which occur in
;;;    a function position, such as lambdas and arbitrary Lisp forms.
;;;    If so, it recursively calls record-callers with pattern 'FORM.
;;;    If the form is a lambda, makes the caller a caller of
;;;    :unnamed-lambda.
;;;
;;; *HANDLE-MACRO-FORMS* t                                        [VARIABLE]
;;;    When T, if the file was loaded before being processed by XREF, and
;;;    the car of a form is a macro, it notes that the parent calls the
;;;    macro, and then calls macroexpand-1 on the form.
;;;
;;; *DEFAULT-GRAPHING-MODE* :call-graph                           [VARIABLE]
;;;    Specifies whether we graph up or down. If :call-graph, the children
;;;    of a node are the functions it calls. If :caller-graph, the
;;;    children of a node are the functions that call it.
;;;
;;; *INDENT-AMOUNT* 3                                             [VARIABLE]
;;;    Number of spaces to indent successive levels in PRINT-INDENTED-TREE.
;;;
;;; DISPLAY-DATABASE (&optional database types-to-ignore)         [FUNCTION]
;;;    Prints out the name of each symbol and all its callers. Specify
;;;    database :callers (the default) to get function call references,
;;;    :file to the get files in which the symbol is called, :readers to get
;;;    variable references, and :setters to get variable binding and
;;;    assignments. Ignores functions of types listed in types-to-ignore.
;;;
;;; PRINT-CALLER-TREES (&key (mode *default-graphing-mode*)       [FUNCTION]
;;;                     (types-to-ignore *types-to-ignore*)
;;;                     compact root-nodes)
;;;    Prints the calling trees (which may actually be a full graph and not
;;;    necessarily a DAG) as indented text trees using
;;;    PRINT-INDENTED-TREE. MODE is :call-graph for trees where the children
;;;    of a node are the functions called by the node, or :caller-graph for
;;;    trees where the children of a node are the functions the node calls.
;;;    TYPES-TO-IGNORE is a list of funcall types (as specified in the
;;;    patterns) to ignore in printing out the database. For example,
;;;    '(:lisp) would ignore all calls to common lisp functions. COMPACT is
;;;    a flag to tell the program to try to compact the trees a bit by not
;;;    printing trees if they have already been seen. ROOT-NODES is a list
;;;    of root nodes of trees to display. If ROOT-NODES is nil, tries to
;;;    find all root nodes in the database.
;;;
;;; MAKE-CALLER-TREE (&optional (mode *default-graphing-mode*)    [FUNCTION]
;;;                   (types-to-ignore *types-to-ignore*)
;;;                   compact)
;;;    Outputs list structure of a tree which roughly represents the
;;;    possibly cyclical structure of the caller database.
;;;    If mode is :call-graph, the children of a node are the functions
;;;    it calls. If mode is :caller-graph, the children of a node are the
;;;    functions that call it.
;;;    If compact is T, tries to eliminate the already-seen nodes, so
;;;    that the graph for a node is printed at most once. Otherwise it will
;;;    duplicate the node's tree (except for cycles). This is usefull
;;;    because the call tree is actually a directed graph, so we can either
;;;    duplicate references or display only the first one.
;;;
;;; DETERMINE-FILE-DEPENDENCIES (&optional database)          [FUNCTION]
;;;    Makes a hash table of file dependencies for the references listed in
;;;    DATABASE. This function may be useful for automatically resolving
;;;    file references for automatic creation of a system definition
;;;    (defsystem).
;;;
;;; PRINT-FILE-DEPENDENCIES (&optional database)              [FUNCTION]
;;;    Prints a list of file dependencies for the references listed in
;;;    DATABASE. This function may be useful for automatically computing
;;;    file loading constraints for a system definition tool.
;;;
;;; WRITE-CALLERS-DATABASE-TO-FILE (filename)                     [FUNCTION]
;;;    Saves the contents of the current callers database to a file. This
;;;    file can be loaded to restore the previous contents of the
;;;    database. (For large systems it can take a long time to crunch
;;;    through the code, so this can save some time.)
;;;
;;; -----
;;; The following macros define new function and macro call patterns.
;;; They may be used to extend the static analysis tool to handle
;;; new def forms, extensions to Common Lisp, and program defs.
;;;
;;; DEFINE-PATTERN-SUBSTITUTION (name pattern)                    [MACRO]
;;;    Defines NAME to be equivalent to the specified pattern. Useful for
;;;    making patterns more readable. For example, the LAMBDA-LIST is
;;;    defined as a pattern substitution, making the definition of the
;;;    DEFUN caller-pattern simpler.
;;;
;;; DEFINE-CALLER-PATTERN (name pattern &optional caller-type)    [MACRO]
;;;    Defines NAME as a function/macro call with argument structure
;;;    described by PATTERN. CALLER-TYPE, if specified, assigns a type to
;;;    the pattern, which may be used to exclude references to NAME while
;;;    viewing the database. For example, all the Common Lisp definitions
;;;    have a caller-type of :lisp or :lisp2, so that you can exclude
;;;    references to common lisp functions from the calling tree.
;;;
;;; DEFINE-VARIABLE-PATTERN (name &optional caller-type)          [MACRO]
;;;    Defines NAME as a variable reference of type CALLER-TYPE. This is
;;;    mainly used to establish the caller-type of the variable.
;;;
;;; DEFINE-CALLER-PATTERN-SYNONYMS (source destinations)          [MACRO]
;;;    For defining function caller pattern syntax synonyms. For each name
;;;    in DESTINATIONS, defines its pattern as a copy of the definition
;;;    of SOURCE. Allows a large number of identical patterns to be defined
;;;    simultaneously. Must occur after the SOURCE has been defined.
;;;
;;; -----
;;; This system includes pattern definitions for the latest
;;; common lisp specification, as published in Guy Steele,
;;; Common Lisp: The Language, 2nd Edition.
;;;
;;; Patterns may be either structures to match, or a predicate
;;; like symbolp/numberp/stringp. The pattern specification language
;;; is similar to the notation used in CLtL2, but in a more lisp-like 
;;; form:
;;;    (:eq name)           The form element must be eq to the symbol NAME.
;;;    (:test test)         TEST must be true when applied to the form element.
;;;    (:typep type)        The form element must be of type TYPE.
;;;    (:or pat1 pat2 ...)  Tries each of the patterns in left-to-right order,
;;;                         until one succeeds.
;;;                         Equivalent to { pat1 | pat2 | ... }
;;;    (:rest pattern)      The remaining form elements are grouped into a
;;;                         list which is matched against PATTERN.
;;;    (:optional pat1 ...) The patterns may optionally match against the
;;;                         form element.
;;;                         Equivalent to [ pat1 ... ].
;;;    (:star pat1 ...)     The patterns may match against the patterns
;;;                         any number of times, including 0.
;;;                         Equivalent to { pat1 ... }*.
;;;    (:plus pat1 ...)     The patterns may match against the patterns
;;;                         any number of times, but at least once.
;;;                         Equivalent to { pat1 ... }+.
;;;    &optional, &key,     Similar in behavior to the corresponding
;;;    &rest                lambda-list keywords.
;;;    FORM                 A random lisp form. If a cons, assumes the
;;;                         car is a function or macro and tries to
;;;                         match the args against that symbol's pattern.
;;;                         If a symbol, assumes it's a variable reference.
;;;    :ignore              Ignores the corresponding form element.
;;;    NAME                 The corresponding form element should be
;;;                         the name of a new definition (e.g., the
;;;                         first arg in a defun pattern is NAME.
;;;    FUNCTION, MACRO      The corresponding form element should be
;;;                         a function reference not handled by FORM.
;;;                         Used in the definition of apply and funcall.
;;;    VAR                  The corresponding form element should be
;;;                         a variable definition or mutation. Used
;;;                         in the definition of let, let*, etc.
;;;    VARIABLE             The corresponding form element should be
;;;                         a variable reference. 
;;;
;;; In all other pattern symbols, it looks up the symbols pattern substitution
;;; and recursively matches against the pattern. Automatically destructures
;;; list structure that does not include consing dots.
;;;
;;; Among the pattern substitution names defined are:
;;;    STRING, SYMBOL, NUMBER    Appropriate :test patterns.
;;;    LAMBDA-LIST               Matches against a lambda list.
;;;    BODY                      Matches against a function body definition.
;;;    FN                        Matches against #'function, 'function,
;;;                              and lambdas. This is used in the definition
;;;                              of apply, funcall, and the mapping patterns.
;;;    and others...
;;;
;;; Here's some sample pattern definitions:
;;; (define-caller-pattern defun 
;;;   (name lambda-list
;;;	(:star (:or documentation-string declaration))
;;;	(:star form))
;;;  :lisp)
;;; (define-caller-pattern funcall (fn (:star form)) :lisp)
;;;
;;; In general, the system is intelligent enough to handle any sort of
;;; simple funcall. One only need specify the syntax for functions and
;;; macros which use optional arguments, keyword arguments, or some
;;; argument positions are special, such as in apply and funcall, or
;;; to indicate that the function is of the specified caller type.
;;;
;;;
;;; NOTES:
;;;
;;;    XRef assumes syntactically correct lisp code.
;;;
;;;    This is by no means perfect. For example, let and let* are treated
;;;    identically, instead of differentiating between serial and parallel
;;;    binding. But it's still a useful tool. It can be helpful in 
;;;    maintaining code, debugging problems with patch files, determining
;;;    whether functions are multiply defined, and help you remember where
;;;    a function is defined or called.
;;;
;;;    XREF runs best when compiled.

;;; ********************************
;;; References *********************
;;; ********************************
;;;
;;; Xerox Interlisp Masterscope Program:
;;;   Larry M Masinter, Global program analysis in an interactive environment
;;;   PhD Thesis, Stanford University, 1980. 
;;;
;;; Symbolics Who-Calls Database:
;;;   User's Guide to Symbolics Computers, Volume 1, Cambridge, MA, July 1986
;;;   Genera 7.0, pp 183-185.
;;;   

;;; ********************************
;;; Example ************************
;;; ********************************
;;; 
;;; Here is an example of running XREF on a short program.
;;; [In Scribe documentation, give a simple short program and resulting
;;;  XREF output, including postscript call graphs.]
#|
<cl> (xref:xref-file  "/afs/cs/user/mkant/Lisp/Graph-Dag/graph-dag.lisp")
Cross-referencing file /afs/cs/user/mkant/Lisp/Graph-Dag/graph-dag.lisp.
................................................
48 forms processed.
<cl> (xref:display-database :readers)

*DISPLAY-CUTOFF-DEPTH* is referenced by CALCULATE-LEVEL-POSITION
CALCULATE-LEVEL-POSITION-BEFORE CALCULATE-POSITION-IN-LEVEL.
*OFFSET-FROM-EDGE-OF-PANE* is referenced by CALCULATE-LEVEL-POSITION
CALCULATE-LEVEL-POSITION-BEFORE.
*WITHIN-LEVEL-SPACING* is referenced by BREADTH CALCULATE-POSITION-INFO.
*DIRECTION* is referenced by CREATE-POSITION-INFO.
*LINK-OFFSET* is referenced by OFFSET-OF-LINK-FROM-ATTACHMENT-POINT.
*ROOT-IS-SEQUENCE* is referenced by GRAPH.
*LEVEL-SPACING* is referenced by CALCULATE-LEVEL-POSITION
CALCULATE-LEVEL-POSITION-BEFORE.
*ORIENTATION* is referenced by BREADTH CALCULATE-LEVEL-POSITION
CALCULATE-LEVEL-POSITION-BEFORE CALCULATE-POSITION-IN-LEVEL.
*DEFAULT-GRAPH-POSITION* is referenced by CREATE-POSITION-INFO.
*GRAPHING-CUTOFF-DEPTH* is referenced by CREATE-NODE-STRUCTURE.
*LIST-OF-NODES* is referenced by CALCULATE-LEVEL-POSITION
CALCULATE-LEVEL-POSITION-BEFORE CREATE-NODE FIND-NODE.
*GRAPH-TYPE* is referenced by CREATE-NODE-STRUCTURE.
<cl> (xref:print-caller-trees :root-nodes '(display-graph))

Rooted calling trees:
  DISPLAY-GRAPH
     CREATE-POSITION-INFO
        CALCULATE-POSITION-INFO
           CALCULATE-POSITION
              NODE-POSITION-ALREADY-SET-FLAG
              NODE-LEVEL-ALREADY-SET-FLAG
              CALCULATE-POSITION-IN-LEVEL
                 NODE-CHILDREN
                 NODE-LEVEL
                 CALCULATE-POSITION
                 NEW-CALCULATE-BREADTH
                    NODE-CHILDREN
                    BREADTH
                       OPPOSITE-DIMENSION
                          NODE-HEIGHT
                          NODE-WIDTH
                    NEW-CALCULATE-BREADTH
                    NODE-PARENTS
                 OPPOSITE-DIMENSION
                    NODE-HEIGHT
                    NODE-WIDTH
                 OPPOSITE-POSITION
                    NODE-Y
                    NODE-X
        NODE-LEVEL
        CALCULATE-LEVEL-POSITION
           NODE-LEVEL
           NODE-POSITION
              NODE-X
              NODE-Y
           DIMENSION
              NODE-WIDTH
              NODE-HEIGHT
        CALCULATE-LEVEL-POSITION-BEFORE
           NODE-LEVEL
           NODE-POSITION
              NODE-X
              NODE-Y
           NODE-WIDTH
           NODE-HEIGHT
           DIMENSION
              NODE-WIDTH
              NODE-HEIGHT
|#

;;; ****************************************************************
;;; List Callers ***************************************************
;;; ****************************************************************

(defpackage :pxref
  (:use :common-lisp)
  (:export #:list-callers 
	   #:list-users 
	   #:list-readers 
	   #:list-setters
	   #:what-files-call
	   #:who-calls 
	   #:list-callees 
	   #:source-file 
	   #:clear-tables
	   #:define-pattern-substitution 
	   #:define-caller-pattern 
	   #:define-variable-pattern 
	   #:define-caller-pattern-synonyms
	   #:clear-patterns
	   #:*last-form* 
	   #:*xref-verbose* 
	   #:*handle-package-forms* 
	   #:*handle-function-forms*
	   #:*handle-macro-forms*
	   #:*types-to-ignore*
	   #:*last-caller-tree* 
	   #:*default-graphing-mode* 
	   #:*indent-amount*
	   #:xref-file 
	   #:xref-files
	   #:write-callers-database-to-file
	   #:display-database
	   #:print-caller-trees 
	   #:make-caller-tree 
	   #:print-indented-tree 
	   #:determine-file-dependencies 
	   #:print-file-dependencies
	   #:psgraph-xref
	   ))

(in-package "PXREF")

;;; Warn user if they're loading the source instead of compiling it first.
;(eval-when (compile load eval)
;  (defvar compiled-p nil))
;(eval-when (compile load)
;  (setq compiled-p t))
;(eval-when (load eval)
;  (unless compiled-p
;    (warn "This file should be compiled before loading for best results.")))
(eval-when (eval)
   (warn "This file should be compiled before loading for best results."))


;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun lookup (symbol environment)
  (dolist (frame environment)
    (when (member symbol frame)
      (return symbol))))

(defun car-eq (list item)
  (and (consp list)
       (eq (car list) item)))

;;; ********************************
;;; Callers Database ***************
;;; ********************************
(defvar *file-callers-database* (make-hash-table :test #'equal)
  "Contains name and list of file callers (files which call) for that name.")
(defvar *callers-database* (make-hash-table :test #'equal)
  "Contains name and list of callers (function invocation) for that name.")
(defvar *readers-database* (make-hash-table :test #'equal)
  "Contains name and list of readers (variable use) for that name.")
(defvar *setters-database* (make-hash-table :test #'equal)
  "Contains name and list of setters (variable mutation) for that name.")
(defvar *callees-database* (make-hash-table :test #'equal)
  "Contains name and list of functions and variables it calls.")
(defun callers-list (name &optional (database :callers))
  (case database
    (:file    (gethash name *file-callers-database*))
    (:callees (gethash name *callees-database*))
    (:callers (gethash name *callers-database*))
    (:readers (gethash name *readers-database*))
    (:setters (gethash name *setters-database*))))
(defsetf callers-list (name &optional (database :callers)) (caller)
  `(setf (gethash ,name (case ,database
			  (:file    *file-callers-database*)
			  (:callees *callees-database*)
			  (:callers *callers-database*)
			  (:readers *readers-database*)
			  (:setters *setters-database*)))
	 ,caller))

(defun list-callers (symbol)
  "Lists all functions which call SYMBOL as a function (function invocation)."
  (callers-list symbol :callers))
(defun list-readers (symbol)
  "Lists all functions which refer to SYMBOL as a variable 
   (variable reference)."
  (callers-list symbol :readers))
(defun list-setters (symbol)
  "Lists all functions which bind/set SYMBOL as a variable 
   (variable mutation)."
  (callers-list symbol :setters))
(defun list-users (symbol)
  "Lists all functions which use SYMBOL as a variable or function."
  (values (list-callers symbol)
	  (list-readers symbol)
	  (list-setters symbol)))
(defun who-calls (symbol &optional how)
  "Lists callers of symbol. HOW may be :function, :reader, :setter,
   or :variable."
  ;; would be nice to have :macro and distinguish variable
  ;; binding from assignment. (i.e., variable binding, assignment, and use)
  (case how
    (:function (list-callers symbol))
    (:reader   (list-readers symbol))
    (:setter   (list-setters symbol))
    (:variable (append (list-readers symbol) 
		       (list-setters symbol)))
    (otherwise (append (list-callers symbol)
		       (list-readers symbol)
		       (list-setters symbol)))))
(defun what-files-call (symbol)
  "Lists names of files that contain uses of SYMBOL 
   as a function, variable, or constant."
  (callers-list symbol :file))
(defun list-callees (symbol)
  "Lists names of functions and variables called by SYMBOL."
  (callers-list symbol :callees))

(defvar *source-file* (make-hash-table :test #'equal)
  "Contains function name and source file for that name.")
(defun source-file (symbol)
  "Lists the names of files in which SYMBOL is defined/used."
  (gethash symbol *source-file*))
(defsetf source-file (name) (value)
  `(setf (gethash ,name *source-file*) ,value))

(defun clear-tables ()
  (clrhash *file-callers-database*)
  (clrhash *callers-database*)
  (clrhash *callees-database*)
  (clrhash *readers-database*)
  (clrhash *setters-database*)
  (clrhash *source-file*))


;;; ********************************
;;; Pattern Database ***************
;;; ********************************
;;; Pattern Types
(defvar *pattern-caller-type* (make-hash-table :test #'equal))
(defun pattern-caller-type (name)
  (gethash name *pattern-caller-type*))
(defsetf pattern-caller-type (name) (value)
  `(setf (gethash ,name *pattern-caller-type*) ,value))

;;; Pattern Substitutions
(defvar *pattern-substitution-table* (make-hash-table :test #'equal)
  "Stores general patterns for function destructuring.")
(defun lookup-pattern-substitution (name)
  (gethash name *pattern-substitution-table*))
(defmacro define-pattern-substitution (name pattern)
  "Defines NAME to be equivalent to the specified pattern. Useful for
   making patterns more readable. For example, the LAMBDA-LIST is 
   defined as a pattern substitution, making the definition of the
   DEFUN caller-pattern simpler."
  `(setf (gethash ',name *pattern-substitution-table*)
	 ',pattern))

;;; Function/Macro caller patterns: 
;;; The car of the form is skipped, so we don't need to specify
;;; (:eq function-name) like we would for a substitution.
;;;
;;; Patterns must be defined in the XREF package because the pattern
;;; language is tested by comparing symbols (using #'equal) and not
;;; their printreps. This is fine for the lisp grammer, because the XREF
;;; package depends on the LISP package, so a symbol like 'xref::cons is
;;; translated automatically into 'lisp::cons. However, since
;;; (equal 'foo::bar 'baz::bar) returns nil unless both 'foo::bar and
;;; 'baz::bar are inherited from the same package (e.g., LISP), 
;;; if package handling is turned on the user must specify package 
;;; names in the caller pattern definitions for functions that occur
;;; in packages other than LISP, otherwise the symbols will not match.
;;; 
;;; Perhaps we should enforce the definition of caller patterns in the
;;; XREF package by wrapping the body of define-caller-pattern in
;;; the XREF package:
;;;    (defmacro define-caller-pattern (name value &optional caller-type)
;;;      (let ((old-package *package*))
;;;        (setf *package* (find-package "XREF"))
;;;        (prog1
;;;    	     `(progn
;;;    	        (when ',caller-type
;;;         	     (setf (pattern-caller-type ',name) ',caller-type))
;;;    	        (when ',value 
;;;    	          (setf (gethash ',name *caller-pattern-table*)
;;;    		        ',value)))
;;;          (setf *package* old-package)))) 
;;; Either that, or for the purpose of pattern testing we should compare
;;; printreps. [The latter makes the primitive patterns like VAR
;;; reserved words.]
(defvar *caller-pattern-table* (make-hash-table :test #'equal)
  "Stores patterns for function destructuring.")
(defun lookup-caller-pattern (name)
  (gethash name *caller-pattern-table*))
(defmacro define-caller-pattern (name pattern &optional caller-type)
  "Defines NAME as a function/macro call with argument structure
   described by PATTERN. CALLER-TYPE, if specified, assigns a type to
   the pattern, which may be used to exclude references to NAME while
   viewing the database. For example, all the Common Lisp definitions
   have a caller-type of :lisp or :lisp2, so that you can exclude 
   references to common lisp functions from the calling tree."
  `(progn
     (when ',caller-type
       (setf (pattern-caller-type ',name) ',caller-type))
     (when ',pattern 
       (setf (gethash ',name *caller-pattern-table*)
	     ',pattern))))

;;; For defining variables
(defmacro define-variable-pattern (name &optional caller-type)
  "Defines NAME as a variable reference of type CALLER-TYPE. This is
   mainly used to establish the caller-type of the variable."
  `(progn
     (when ',caller-type
       (setf (pattern-caller-type ',name) ',caller-type))))

;;; For defining synonyms. Means much less space taken up by the patterns.
(defmacro define-caller-pattern-synonyms (source destinations)
  "For defining function caller pattern syntax synonyms. For each name
   in DESTINATIONS, defines its pattern as a copy of the definition of SOURCE.
   Allows a large number of identical patterns to be defined simultaneously.
   Must occur after the SOURCE has been defined."
  `(let ((source-type (pattern-caller-type ',source))
	 (source-pattern (gethash ',source *caller-pattern-table*)))
     (when source-type
       (dolist (dest ',destinations)
	 (setf (pattern-caller-type dest) source-type)))
     (when source-pattern
       (dolist (dest ',destinations)
	 (setf (gethash dest *caller-pattern-table*)
	       source-pattern)))))

(defun clear-patterns ()
  (clrhash *pattern-substitution-table*)
  (clrhash *caller-pattern-table*)
  (clrhash *pattern-caller-type*))

;;; ********************************
;;; Cross Reference Files **********
;;; ********************************
(defvar *last-form* ()
  "The last form read from the file. Useful for figuring out what went wrong
   when xref-file drops into the debugger.")

(defvar *xref-verbose* t
  "When T, xref-file(s) prints out the names of the files it looks at,
   progress dots, and the number of forms read.")

;;; This needs to first clear the tables?
(defun xref-files (&rest files)
  "Grovels over the lisp code located in source file FILES, using xref-file."
  ;; If the arg is a list, use it.
  (when (listp (car files)) (setq files (car files)))
  (dolist (file files)
    (xref-file file nil))
  (values))

(defvar *handle-package-forms* nil	;'(lisp::in-package)
  "When non-NIL, and XREF-FILE sees a package-setting form like IN-PACKAGE,
   sets the current package to the specified package by evaluating the
   form. When done with the file, xref-file resets the package to its 
   original value. In some of the displaying functions, when this variable
   is non-NIL one may specify that all symbols from a particular set of
   packages be ignored. This is only useful if the files use different
   packages with conflicting names.")

(defvar *normal-readtable* (copy-readtable nil)
  "Normal, unadulterated CL readtable.")

(defun xref-file (filename &optional (clear-tables t) (verbose *xref-verbose*))
  "Cross references the function and variable calls in FILENAME by
   walking over the source code located in the file. Defaults type of
   filename to \".lisp\". Chomps on the code using record-callers and
   record-callers*. If CLEAR-TABLES is T (the default), it clears the callers
   database before processing the file. Specify CLEAR-TABLES as nil to
   append to the database. If VERBOSE is T (the default), prints out the
   name of the file, one progress dot for each form processed, and the
   total number of forms."
  ;; Default type to "lisp"
  (when (and (null (pathname-type filename))
	     (not  (probe-file filename)))
    (cond ((stringp filename)
	   (setf filename (concatenate 'string filename ".lisp")))
	  ((pathnamep filename)
	   (setf filename (merge-pathnames filename
					   (make-pathname :type "lisp"))))))
  (when clear-tables (clear-tables))
  (let ((count 0)
	(old-package *package*)
	(*readtable* *normal-readtable*))
    (when verbose
      (format t "~&Cross-referencing file ~A.~&" filename))
    (with-open-file (stream filename :direction :input)
      (do ((form (read stream nil :eof) (read stream nil :eof)))
	  ((eq form :eof))
	(incf count)
	(when verbose
	  (format *standard-output* ".")
	  (force-output *standard-output*))
	(setq *last-form* form)
	(record-callers filename form)
	;; Package Magic.
	(when (and *handle-package-forms*
		   (consp form)
		   (member (car form) *handle-package-forms*))
	  (eval form))))
    (when verbose 
      (format t "~&~D forms processed." count))
    (setq *package* old-package)
    (values)))

(defvar *handle-function-forms* t
  "When T, XREF-FILE tries to be smart about forms which occur in
   a function position, such as lambdas and arbitrary Lisp forms.
   If so, it recursively calls record-callers with pattern 'FORM.
   If the form is a lambda, makes the caller a caller of :unnamed-lambda.") 

(defvar *handle-macro-forms* t
  "When T, if the file was loaded before being processed by XREF, and the
   car of a form is a macro, it notes that the parent calls the macro,
   and then calls macroexpand-1 on the form.") 

(defvar *callees-database-includes-variables* nil)

(defun record-callers (filename form 
				&optional pattern parent (environment nil)
				funcall)
  "RECORD-CALLERS is the main routine used to walk down the code. It matches
   the PATTERN against the FORM, possibly adding statements to the database.
   PARENT is the name defined by the current outermost definition; it is
   the caller of the forms in the body (e.g., FORM). ENVIRONMENT is used
   to keep track of the scoping of variables. FUNCALL deals with the type
   of variable assignment and hence how the environment should be modified.
   RECORD-CALLERS handles atomic patterns and simple list-structure patterns.
   For complex list-structure pattern destructuring, it calls RECORD-CALLERS*."
;  (when form)
  (unless pattern (setq pattern 'FORM))
  (cond ((symbolp pattern)
	 (case pattern
	   (:IGNORE
	    ;; Ignores the rest of the form.
	    (values t parent environment))
	   (NAME    
	    ;; This is the name of a new definition.
	    (push filename (source-file form))
	    (values t form   environment))
	   ((FUNCTION MACRO)
	    ;; This is the name of a call.
	    (cond ((and *handle-function-forms* (consp form))
		   ;; If we're a cons and special handling is on,
		   (when (eq (car form) 'lambda)
		     (pushnew filename (callers-list :unnamed-lambda :file))
		     (when parent
		       (pushnew parent (callers-list :unnamed-lambda
						     :callers))
		       (pushnew :unnamed-lambda (callers-list parent
							      :callees))))
		   (record-callers filename form 'form parent environment))
		  (t 
		   ;; If we're just a regular function name call.
		   (pushnew filename (callers-list form :file))
		   (when parent
		     (pushnew parent (callers-list form :callers))
		     (pushnew form (callers-list parent :callees)))
		   (values t parent environment)))) 
	   (VAR     
	    ;; This is the name of a new variable definition.
	    ;; Includes arglist parameters.
	    (when (and (symbolp form) (not (keywordp form))
		       (not (member form lambda-list-keywords)))
	      (pushnew form (car environment))
	      (pushnew filename (callers-list form :file))
	      (when parent 
;		  (pushnew form (callers-list parent :callees))
		(pushnew parent (callers-list form :setters)))
	      (values t parent environment)))
	   (VARIABLE
	    ;; VAR reference
	    (pushnew filename (callers-list form :file))
	    (when (and parent (not (lookup form environment)))
	      (pushnew parent (callers-list form :readers))
	      (when *callees-database-includes-variables*
		(pushnew form (callers-list parent :callees))))
	    (values t parent environment))
	   (FORM    
	    ;; A random form (var or funcall).
	    (cond ((consp form)
		   ;; Get new pattern from TAG.
		   (let ((new-pattern (lookup-caller-pattern (car form))))
		     (pushnew filename (callers-list (car form) :file))
		     (when parent
		       (pushnew parent (callers-list (car form) :callers))
		       (pushnew (car form) (callers-list parent :callees)))
		     (cond ((and new-pattern (cdr form))
			    ;; Special Pattern and there's stuff left
			    ;; to be processed. Note that we check if
			    ;; a pattern is defined for the form before
			    ;; we check to see if we can macroexpand it.
			    (record-callers filename (cdr form) new-pattern
					    parent environment :funcall))
			   ((and *handle-macro-forms*
				 (symbolp (car form)) ; pnorvig 9/9/93
				 (macro-function (car form)))
			    ;; The car of the form is a macro and
			    ;; macro processing is turned on. Macroexpand-1
			    ;; the form and try again.
			    (record-callers filename 
					    (macroexpand-1 form)
					    'form parent environment 
					    :funcall))
			   ((null (cdr form))
			    ;; No more left to be processed. Note that
			    ;; this must occur after the macros clause,
			    ;; since macros can expand into more code.
			    (values t parent environment))
			   (t
			    ;; Random Form. We assume it is a function call.
			    (record-callers filename (cdr form)
					    '((:star FORM))
					    parent environment :funcall)))))
		  (t 
		   (when (and (not (lookup form environment))
			      (not (numberp form))
			      ;; the following line should probably be 
			      ;; commented out?
			      (not (keywordp form))
			      (not (stringp form))
			      (not (eq form t))
			      (not (eq form nil)))
		     (pushnew filename (callers-list form :file))
		     ;; ??? :callers
		     (when parent
		       (pushnew parent (callers-list form :readers))
		       (when *callees-database-includes-variables*
			 (pushnew form (callers-list parent :callees)))))
		   (values t parent environment))))
	   (otherwise 
	    ;; Pattern Substitution
	    (let ((new-pattern (lookup-pattern-substitution pattern)))
	      (if new-pattern
		  (record-callers filename form new-pattern 
				  parent environment)
		  (when (eq pattern form)
		    (values t parent environment)))))))
	((consp pattern)
	 (case (car pattern)
	   (:eq    (when (eq (second pattern) form)
		     (values t parent environment)))
	   (:test  (when (funcall (eval (second pattern)) form)
		     (values t parent environment)))
	   (:typep (when (typep form (second pattern))
		     (values t parent environment)))
	   (:or    (dolist (subpat (rest pattern))
		     (multiple-value-bind (processed parent environment)
			 (record-callers filename form subpat
					 parent environment)
		       (when processed
			 (return (values processed parent environment))))))
	   (:rest			; (:star :plus :optional :rest)
	    (record-callers filename form (second pattern)
			    parent environment))
	   (otherwise
	    (multiple-value-bind (d p env)
		(record-callers* filename form pattern 
				 parent (cons nil environment))
	      (values d p (if funcall environment env))))))))

(defun record-callers* (filename form pattern parent environment
				 &optional continuation 
				 in-optionals in-keywords)
  "RECORD-CALLERS* handles complex list-structure patterns, such as
   ordered lists of subpatterns, patterns involving :star, :plus,
   &optional, &key, &rest, and so on. CONTINUATION is a stack of
   unprocessed patterns, IN-OPTIONALS and IN-KEYWORDS are corresponding
   stacks which determine whether &rest or &key has been seen yet in
   the current pattern."   
  ;; form must be a cons or nil.
;  (when form)
  (if (null pattern)
      (if (null continuation)
	  (values t parent environment)
	  (record-callers* filename form (car continuation) parent environment
			   (cdr continuation) 
			   (cdr in-optionals)
			   (cdr in-keywords)))
      (let ((pattern-elt (car pattern)))
	(cond ((car-eq pattern-elt :optional)
	       (if (null form) 
		   (values t parent environment)
		   (multiple-value-bind (processed par env)
		       (record-callers* filename form (cdr pattern-elt)
					parent environment
					(cons (cdr pattern) continuation)
					(cons (car in-optionals) in-optionals)
					(cons (car in-keywords) in-keywords))
		     (if processed
			 (values processed par env)
			 (record-callers* filename form (cdr pattern)
					  parent environment continuation
					  in-optionals in-keywords)))))
	      ((car-eq pattern-elt :star)
	       (if (null form)
		   (values t parent environment)
		   (multiple-value-bind (processed par env)
		       (record-callers* filename form (cdr pattern-elt)
					parent environment
					(cons pattern continuation)
					(cons (car in-optionals) in-optionals)
					(cons (car in-keywords) in-keywords))
		     (if processed
			 (values processed par env)
			 (record-callers* filename form (cdr pattern)
					  parent environment continuation
					  in-optionals in-keywords)))))
	      ((car-eq pattern-elt :plus)
	       (record-callers* filename form (cdr pattern-elt)
				parent environment
				(cons (cons (cons :star (cdr pattern-elt))
					    (cdr pattern))
				      continuation)
				(cons (car in-optionals) in-optionals)
				(cons (car in-keywords) in-keywords)))
	      ((car-eq pattern-elt :rest)
	       (record-callers filename form pattern-elt parent environment))
	      ((eq pattern-elt '&optional)
	       (record-callers* filename form (cdr pattern)
				parent environment continuation
				(cons t in-optionals)
				(cons (car in-keywords) in-keywords)))
	      ((eq pattern-elt '&rest)
	       (record-callers filename form (second pattern)
			       parent environment))
	      ((eq pattern-elt '&key)
	       (record-callers* filename form (cdr pattern)
				parent environment continuation
				(cons (car in-optionals) in-optionals)
				(cons t in-keywords)))
	      ((null form)
	       (when (or (car in-keywords) (car in-optionals))
		 (values t parent environment)))
	      ((consp form)
	       (multiple-value-bind (processed parent environment)
		   (record-callers filename (if (car in-keywords)
						(cadr form)
						(car form))
				   pattern-elt
				   parent environment)
		 (cond (processed
			(record-callers* filename (if (car in-keywords)
						      (cddr form)
						      (cdr form))
					 (cdr pattern)
					 parent environment
					 continuation
					 in-optionals in-keywords))
		       ((or (car in-keywords)
			    (car in-optionals))
			(values t parent environment)))))))))


;;; ********************************
;;; Misc Utilities *****************
;;; ********************************
(defvar *types-to-ignore*
  '(:lisp			; CLtL 1st Edition
    :lisp2			; CLtL 2nd Edition additional patterns
    )
  "Default set of caller types (as specified in the patterns) to ignore
   in the database handling functions. :lisp is CLtL 1st edition,
   :lisp2 is additional patterns from CLtL 2nd edition.")

(defun display-database (&optional (database :callers) 
				   (types-to-ignore *types-to-ignore*))
  "Prints out the name of each symbol and all its callers. Specify database
   :callers (the default) to get function call references, :fill to the get
   files in which the symbol is called, :readers to get variable references,
   and :setters to get variable binding and assignments. Ignores functions
   of types listed in types-to-ignore."
  (maphash #'(lambda (name callers)
	       (unless (or (member (pattern-caller-type name)
				   types-to-ignore)
			   ;; When we're doing fancy package crap,
			   ;; allow us to ignore symbols based on their
			   ;; packages.
			   (when *handle-package-forms*
			     (member (symbol-package name)
				     types-to-ignore
				     :key #'find-package)))
		 (format t "~&~S is referenced by~{ ~S~}."
			 name callers)))
	   (ecase database
	     (:file    *file-callers-database*)
	     (:callers *callers-database*)
	     (:readers *readers-database*)
	     (:setters *setters-database*))))

(defun write-callers-database-to-file (filename)
  "Saves the contents of the current callers database to a file. This
   file can be loaded to restore the previous contents of the
   database. (For large systems it can take a long time to crunch
   through the code, so this can save some time.)"
  (with-open-file (stream filename :direction :output)
    (format stream "~&(clear-tables)")
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (source-file '~S) '~S)"
			 x y))
	     *source-file*)
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (callers-list '~S :file) '~S)"
			 x y))
	     *file-callers-database*)
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (callers-list '~S :callers) '~S)"
			 x y))
	     *callers-database*)
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (callers-list '~S :callees) '~S)"
			 x y))
	     *callees-database*)
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (callers-list '~S :readers) '~S)"
			 x y))
	     *readers-database*)
    (maphash #'(lambda (x y) 
		 (format stream "~&(setf (callers-list '~S :setters) '~S)"
			 x y))
	     *setters-database*)))


;;; ********************************
;;; Print Caller Trees *************
;;; ********************************
;;; The following function is useful for reversing a caller table into
;;; a callee table. Possibly later we'll extend xref to create two 
;;; such database hash tables. Needs to include vars as well.
(defun invert-hash-table (table &optional (types-to-ignore *types-to-ignore*))
  "Makes a copy of the hash table in which (name value*) pairs
   are inverted to (value name*) pairs."
  (let ((target (make-hash-table :test #'equal)))
    (maphash #'(lambda (key values)
		 (dolist (value values)
		   (unless (member (pattern-caller-type key) 
				   types-to-ignore)
		     (pushnew key (gethash value target)))))
	     table)
    target))

;;; Resolve file references for automatic creation of a defsystem file.
(defun determine-file-dependencies (&optional (database *callers-database*))
  "Makes a hash table of file dependencies for the references listed in
   DATABASE. This function may be useful for automatically resolving
   file references for automatic creation of a system definition (defsystem)."
  (let ((file-ref-ht  (make-hash-table :test #'equal)))
    (maphash #'(lambda (key values)
		 (let ((key-file (source-file key)))
		   (when key
		     (dolist (value values)
		       (let ((value-file (source-file value)))
			 (when value-file
			   (dolist (s key-file)
			     (dolist (d value-file)
			       (pushnew d (gethash s file-ref-ht))))))))))
	     database)
    file-ref-ht))

(defun print-file-dependencies (&optional (database *callers-database*))
  "Prints a list of file dependencies for the references listed in DATABASE.
   This function may be useful for automatically computing file loading
   constraints for a system definition tool."
  (maphash #'(lambda (key value) (format t "~&~S --> ~S" key value))
	   (determine-file-dependencies database)))

;;; The following functions demonstrate a possible way to interface
;;; xref to a graphical browser such as psgraph to mimic the capabilities
;;; of Masterscope's graphical browser. 

(defvar *last-caller-tree* nil)

(defvar *default-graphing-mode* :call-graph
  "Specifies whether we graph up or down. If :call-graph, the children
   of a node are the functions it calls. If :caller-graph, the children
   of a node are the functions that call it.") 

(defun gather-tree (parents &optional already-seen 
			    (mode *default-graphing-mode*)
			    (types-to-ignore *types-to-ignore*) compact)
  "Extends the tree, copying it into list structure, until it repeats
   a reference (hits a cycle)."
  (let ((*already-seen* nil)
	(database (case mode
		    (:call-graph   *callees-database*)
		    (:caller-graph *callers-database*))))
    (declare (special *already-seen*))
    (labels 
	((amass-tree
	  (parents &optional already-seen)
	  (let (result this-item)
	    (dolist (parent parents)
	      (unless (member (pattern-caller-type parent)
			      types-to-ignore)
		(pushnew parent *already-seen*)
		(if (member parent already-seen)
		    (setq this-item nil) ; :ignore
		    (if compact 
			(multiple-value-setq (this-item already-seen)
			    (amass-tree (gethash parent database)
					(cons parent already-seen)))
			(setq this-item
			      (amass-tree (gethash parent database)
					  (cons parent already-seen)))))
		(setq parent (format nil "~S" parent))
		(when (consp parent) (setq parent (cons :xref-list parent)))
		(unless (eq this-item :ignore)
		  (push (if this-item
			    (list parent this-item)
			    parent) 
			result))))
	    (values result		;(reverse result)
		    already-seen))))
      (values (amass-tree parents already-seen)
	      *already-seen*))))

(defun find-roots-and-cycles (&optional (mode *default-graphing-mode*)
					(types-to-ignore *types-to-ignore*))
  "Returns a list of uncalled callers (roots) and called callers (potential
   cycles)."
  (let ((uncalled-callers nil)
	(called-callers nil)
	(database (ecase mode
		    (:call-graph   *callers-database*)
		    (:caller-graph *callees-database*)))
	(other-database (ecase mode
			  (:call-graph   *callees-database*)
			  (:caller-graph *callers-database*))))
    (maphash #'(lambda (name value)
		 (declare (ignore value))
		 (unless (member (pattern-caller-type name) 
				 types-to-ignore)
		   (if (gethash name database)
		       (push name called-callers)
		       (push name uncalled-callers))))
	     other-database)
    (values uncalled-callers called-callers)))

(defun make-caller-tree (&optional (mode *default-graphing-mode*)
				   (types-to-ignore *types-to-ignore*) compact)
  "Outputs list structure of a tree which roughly represents the possibly
   cyclical structure of the caller database.
   If mode is :call-graph, the children of a node are the functions it calls.
   If mode is :caller-graph, the children of a node are the functions that
   call it.
   If compact is T, tries to eliminate the already-seen nodes, so that
   the graph for a node is printed at most once. Otherwise it will duplicate
   the node's tree (except for cycles). This is usefull because the call tree
   is actually a directed graph, so we can either duplicate references or
   display only the first one."
  ;; Would be nice to print out line numbers and whenever we skip a duplicated
  ;; reference, print the line number of the full reference after the node.
  (multiple-value-bind (uncalled-callers called-callers)
      (find-roots-and-cycles mode types-to-ignore)
    (multiple-value-bind (trees already-seen)
	(gather-tree uncalled-callers nil mode types-to-ignore compact)
      (setq *last-caller-tree* trees)
      (let ((more-trees (gather-tree (set-difference called-callers
						     already-seen)
				     already-seen 
				     mode types-to-ignore compact)))
	(values trees more-trees)))))

(defvar *indent-amount* 3
  "Number of spaces to indent successive levels in PRINT-INDENTED-TREE.")

(defun print-indented-tree (trees &optional (indent 0))
  "Simple code to print out a list-structure tree (such as those created
   by make-caller-tree) as indented text."
  (when trees
    (dolist (tree trees)
      (cond ((and (listp tree) (eq (car tree) :xref-list))
	     (format t "~&~VT~A" indent (cdr tree)))
	    ((listp tree)
	     (format t "~&~VT~A" indent (car tree))
	     (print-indented-tree (cadr tree) (+ indent *indent-amount*)))
	    (t
	     (format t "~&~VT~A" indent tree))))))

(defun print-caller-trees (&key (mode *default-graphing-mode*)
				(types-to-ignore *types-to-ignore*)
				compact
				root-nodes)
  "Prints the calling trees (which may actually be a full graph and not
   necessarily a DAG) as indented text trees using PRINT-INDENTED-TREE.
   MODE is :call-graph for trees where the children of a node are the
   functions called by the node, or :caller-graph for trees where the
   children of a node are the functions the node calls. TYPES-TO-IGNORE
   is a list of funcall types (as specified in the patterns) to ignore
   in printing out the database. For example, '(:lisp) would ignore all
   calls to common lisp functions. COMPACT is a flag to tell the program
   to try to compact the trees a bit by not printing trees if they have
   already been seen. ROOT-NODES is a list of root nodes of trees to 
   display. If ROOT-NODES is nil, tries to find all root nodes in the
   database."
  (multiple-value-bind (rooted cycles)
      (if root-nodes
	  (values (gather-tree root-nodes nil mode types-to-ignore compact))
	  (make-caller-tree mode types-to-ignore compact))
    (when rooted
      (format t "~&Rooted calling trees:")
      (print-indented-tree rooted 2))
    (when cycles
      (when rooted      
	(format t "~2%"))
      (format t "~&Cyclic calling trees:")
      (print-indented-tree cycles 2))))


;;; ********************************
;;; Interface to PSGraph ***********
;;; ********************************
#|
;;; Interface to Bates' PostScript Graphing Utility
(load "/afs/cs/user/mkant/Lisp/PSGraph/psgraph")

(defparameter *postscript-output-directory* "")
(defun psgraph-xref (&key (mode *default-graphing-mode*)
			  (output-directory *postscript-output-directory*)
			  (types-to-ignore *types-to-ignore*)
			  (compact t)
			  (shrink t)
			  root-nodes
			  insert)
  ;; If root-nodes is a non-nil list, uses that list as the starting
  ;; position. Otherwise tries to find all roots in the database.
  (multiple-value-bind (rooted cycles)
      (if root-nodes
	  (values (gather-tree root-nodes nil mode types-to-ignore compact))
	  (make-caller-tree mode types-to-ignore compact))
    (psgraph-output (append rooted cycles) output-directory shrink insert)))

(defun psgraph-output (list-of-trees directory shrink &optional insert)
  (let ((psgraph:*fontsize* 9)
	(psgraph:*second-fontsize* 7)
;	(psgraph:*boxkind* "fill")
	(psgraph:*boxgray* "0") ; .8
	(psgraph:*edgewidth* "1")
	(psgraph:*edgegray* "0"))
    (labels ((stringify (thing)
		(cond ((stringp thing) (string-downcase thing))
		      ((symbolp thing) (string-downcase (symbol-name thing)))
		      ((and (listp thing) (eq (car thing) :xref-list))
		       (stringify (cdr thing)))
		      ((listp thing) (stringify (car thing)))
		      (t (string thing)))))
      (dolist (item list-of-trees)
	(let* ((fname (stringify item))
	       (filename (concatenate 'string directory
				      (string-trim '(#\: #\|) fname)
				      ".ps")))
	  (format t "~&Creating PostScript file ~S." filename)
	  (with-open-file (*standard-output* filename
					     :direction :output
					     :if-does-not-exist :create
					     :if-exists :supersede)
	    ;; Note that the #'eq prints the DAG as a tree. If
	    ;; you replace it with #'equal, it will print it as
	    ;; a DAG, which I think is slightly ugly.
	    (psgraph:psgraph item
			     #'caller-tree-children #'caller-info shrink
			     insert #'eq)))))))

(defun caller-tree-children (tree)
  (when (and tree (listp tree) (not (eq (car tree) :xref-list)))
    (cadr tree)))

(defun caller-tree-node (tree)
  (when tree
    (cond ((and (listp tree) (eq (car tree) :xref-list))
	   (cdr tree))
	  ((listp tree)
	   (car tree))
	  (t
	   tree))))

(defun caller-info (tree)
  (let ((node (caller-tree-node tree)))
    (list node)))
|#
#|
;;; Code to print out graphical trees of CLOS class hierarchies.
(defun print-class-hierarchy (&optional (start-class 'anything) 
					(file "classes.ps"))
  (let ((start (find-class start-class)))
    (when start
      (with-open-file (*standard-output* file :direction :output)
	(psgraph:psgraph start 
			 #'clos::class-direct-subclasses
			 #'(lambda (x) 
			     (list (format nil "~A" (clos::class-name x))))
			 t nil #'eq)))))

|#


;;; ****************************************************************
;;; Cross Referencing Patterns for Common Lisp *********************
;;; ****************************************************************
(clear-patterns)

;;; ********************************
;;; Pattern Substitutions **********
;;; ********************************
(define-pattern-substitution integer (:test #'integerp))
(define-pattern-substitution rational (:test #'rationalp))
(define-pattern-substitution symbol  (:test #'symbolp))
(define-pattern-substitution string  (:test #'stringp))
(define-pattern-substitution number  (:test #'numberp))
(define-pattern-substitution lambda-list
  ((:star var)
   (:optional (:eq &optional)
	      (:star (:or var
			  (var (:optional form (:optional var))))))
   (:optional (:eq &rest) var)
   (:optional (:eq &key) (:star (:or var
			       ((:or var
				     (keyword var))
				(:optional form (:optional var)))))
	      (:optional &allow-other-keys))
   (:optional (:eq &aux)
	      (:star (:or var
			  (var (:optional form)))))))
(define-pattern-substitution test form)
(define-pattern-substitution body
  ((:star (:or declaration documentation-string))
   (:star form)))
(define-pattern-substitution documentation-string string)
(define-pattern-substitution initial-value form)
(define-pattern-substitution tag symbol)
(define-pattern-substitution declaration ((:eq declare)(:rest :ignore)))
(define-pattern-substitution destination form)
(define-pattern-substitution control-string string)
(define-pattern-substitution format-arguments 
  ((:star form)))
(define-pattern-substitution fn
  (:or ((:eq quote) function) 
       ((:eq function) function)
       function))

;;; ********************************
;;; Caller Patterns ****************
;;; ********************************

;;; Types Related
(define-caller-pattern coerce (form :ignore) :lisp)
(define-caller-pattern type-of (form) :lisp)
(define-caller-pattern upgraded-array-element-type (:ignore) :lisp2)
(define-caller-pattern upgraded-complex-part-type (:ignore) :lisp2)

;;; Lambdas and Definitions
(define-variable-pattern lambda-list-keywords :lisp)
(define-variable-pattern lambda-parameters-limit :lisp)
(define-caller-pattern lambda (lambda-list (:rest body)) :lisp)

(define-caller-pattern defun 
  (name lambda-list
	(:star (:or documentation-string declaration))
	(:star form))
  :lisp)

;;; perhaps this should use VAR, instead of NAME
(define-caller-pattern defvar 
  (var (:optional initial-value (:optional documentation-string)))
  :lisp)
(define-caller-pattern defparameter
  (var initial-value (:optional documentation-string))
  :lisp)
(define-caller-pattern defconstant
  (var initial-value (:optional documentation-string))
  :lisp)

(define-caller-pattern eval-when
  (:ignore				; the situations
   (:star form))
  :lisp)

;;; Logical Values
(define-variable-pattern nil :lisp)
(define-variable-pattern t :lisp)

;;; Predicates
(define-caller-pattern typep (form form) :lisp)
(define-caller-pattern subtypep (form form) :lisp)

(define-caller-pattern null (form) :lisp)
(define-caller-pattern symbolp (form) :lisp)
(define-caller-pattern atom (form) :lisp)
(define-caller-pattern consp (form) :lisp)
(define-caller-pattern listp (form) :lisp)
(define-caller-pattern numberp (form) :lisp)
(define-caller-pattern integerp (form) :lisp)
(define-caller-pattern rationalp (form) :lisp)
(define-caller-pattern floatp (form) :lisp)
(define-caller-pattern realp (form) :lisp2)
(define-caller-pattern complexp (form) :lisp)
(define-caller-pattern characterp (form) :lisp)
(define-caller-pattern stringp (form) :lisp)
(define-caller-pattern bit-vector-p (form) :lisp)
(define-caller-pattern vectorp (form) :lisp)
(define-caller-pattern simple-vector-p (form) :lisp)
(define-caller-pattern simple-string-p (form) :lisp)
(define-caller-pattern simple-bit-vector-p (form) :lisp)
(define-caller-pattern arrayp (form) :lisp)
(define-caller-pattern packagep (form) :lisp)
(define-caller-pattern functionp (form) :lisp)
(define-caller-pattern compiled-function-p (form) :lisp)
(define-caller-pattern commonp (form) :lisp)

;;; Equality Predicates
(define-caller-pattern eq (form form) :lisp)
(define-caller-pattern eql (form form) :lisp)
(define-caller-pattern equal (form form) :lisp)
(define-caller-pattern equalp (form form) :lisp)

;;; Logical Operators
(define-caller-pattern not (form) :lisp)
(define-caller-pattern or ((:star form)) :lisp)
(define-caller-pattern and ((:star form)) :lisp)

;;; Reference

;;; Quote is a problem. In Defmacro & friends, we'd like to actually
;;; look at the argument, 'cause it hides internal function calls
;;; of the defmacro. 
(define-caller-pattern quote (:ignore) :lisp)

(define-caller-pattern function ((:or fn form)) :lisp)
(define-caller-pattern symbol-value (form) :lisp)
(define-caller-pattern symbol-function (form) :lisp)
(define-caller-pattern fdefinition (form) :lisp2)
(define-caller-pattern boundp (form) :lisp)
(define-caller-pattern fboundp (form) :lisp)
(define-caller-pattern special-form-p (form) :lisp)

;;; Assignment
(define-caller-pattern setq ((:star var form)) :lisp)
(define-caller-pattern psetq ((:star var form)) :lisp)
(define-caller-pattern set (form form) :lisp)
(define-caller-pattern makunbound (form) :lisp)
(define-caller-pattern fmakunbound (form) :lisp)

;;; Generalized Variables
(define-caller-pattern setf ((:star form form)) :lisp)
(define-caller-pattern psetf ((:star form form)) :lisp)
(define-caller-pattern shiftf ((:plus form) form) :lisp)
(define-caller-pattern rotatef ((:star form)) :lisp)
(define-caller-pattern define-modify-macro 
  (name
   lambda-list
   fn
   (:optional documentation-string))
  :lisp)
(define-caller-pattern defsetf 
  (:or (name name (:optional documentation-string))
       (name lambda-list (var)
	(:star (:or declaration documentation-string))
	(:star form)))
  :lisp)
(define-caller-pattern define-setf-method
  (name lambda-list
   (:star (:or declaration documentation-string))
   (:star form))
  :lisp)
(define-caller-pattern get-setf-method (form) :lisp)
(define-caller-pattern get-setf-method-multiple-value (form) :lisp)


;;; Function invocation
(define-caller-pattern apply (fn form (:star form)) :lisp)
(define-caller-pattern funcall (fn (:star form)) :lisp)


;;; Simple sequencing
(define-caller-pattern progn ((:star form)) :lisp)
(define-caller-pattern prog1 (form (:star form)) :lisp)
(define-caller-pattern prog2 (form form (:star form)) :lisp)

;;; Variable bindings
(define-caller-pattern let
  (((:star (:or var (var &optional form))))
   (:star declaration)
   (:star form))
  :lisp)
(define-caller-pattern let*
  (((:star (:or var (var &optional form))))
    (:star declaration)
    (:star form))
  :lisp)
(define-caller-pattern compiler-let
  (((:star (:or var (var form))))
    (:star form))
  :lisp)
(define-caller-pattern progv
  (form form (:star form)) :lisp)
(define-caller-pattern flet
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(define-caller-pattern labels
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(define-caller-pattern macrolet
  (((:star (name lambda-list 
		 (:star (:or declaration
			     documentation-string))
		 (:star form))))
   (:star form))
  :lisp)
(define-caller-pattern symbol-macrolet
  (((:star (var form))) (:star declaration) (:star form))
  :lisp2)

;;; Conditionals
(define-caller-pattern if (test form (:optional form)) :lisp)
(define-caller-pattern when (test (:star form)) :lisp)
(define-caller-pattern unless (test (:star form)) :lisp)
(define-caller-pattern cond ((:star (test (:star form)))) :lisp)
(define-caller-pattern case
  (form
   (:star ((:or symbol
		((:star symbol)))
	   (:star form)))) 
  :lisp)
(define-caller-pattern typecase (form (:star (symbol (:star form)))) 
  :lisp)

;;; Blocks and Exits
(define-caller-pattern block (name (:star form)) :lisp)
(define-caller-pattern return-from (function (:optional form)) :lisp)
(define-caller-pattern return ((:optional form)) :lisp)

;;; Iteration
(define-caller-pattern loop ((:star form)) :lisp)
(define-caller-pattern do
  (((:star (:or var
		(var (:optional form (:optional form)))))) ; init step
   (form (:star form)) ; end-test result
   (:star declaration)
   (:star (:or tag form)))		; statement
  :lisp)
(define-caller-pattern do*
  (((:star (:or var
		(var (:optional form (:optional form)))))) 
   (form (:star form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(define-caller-pattern dolist
  ((var form (:optional form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(define-caller-pattern dotimes
  ((var form (:optional form))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)

;;; Mapping
(define-caller-pattern mapcar (fn form (:star form)) :lisp)
(define-caller-pattern maplist (fn form (:star form)) :lisp)
(define-caller-pattern mapc (fn form (:star form)) :lisp)
(define-caller-pattern mapl (fn form (:star form)) :lisp)
(define-caller-pattern mapcan (fn form (:star form)) :lisp)
(define-caller-pattern mapcon (fn form (:star form)) :lisp)

;;; The "Program Feature"
(define-caller-pattern tagbody ((:star (:or tag form))) :lisp)
(define-caller-pattern prog
  (((:star (:or var (var (:optional form)))))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(define-caller-pattern prog*    
  (((:star (:or var (var (:optional form)))))
   (:star declaration)
   (:star (:or tag form)))
  :lisp)
(define-caller-pattern go (tag) :lisp)

;;; Multiple Values
(define-caller-pattern values ((:star form)) :lisp)
(define-variable-pattern multiple-values-limit :lisp)
(define-caller-pattern values-list (form) :lisp)
(define-caller-pattern multiple-value-list (form) :lisp)
(define-caller-pattern multiple-value-call (fn (:star form)) :lisp)
(define-caller-pattern multiple-value-prog1 (form (:star form)) :lisp)
(define-caller-pattern multiple-value-bind
  (((:star var)) form
   (:star declaration)
   (:star form))
  :lisp)
(define-caller-pattern multiple-value-setq (((:star var)) form) :lisp)
(define-caller-pattern nth-value (form form) :lisp2)

;;; Dynamic Non-Local Exits
(define-caller-pattern catch (tag (:star form)) :lisp)
(define-caller-pattern throw (tag form) :lisp)
(define-caller-pattern unwind-protect (form (:star form)) :lisp)

;;; Macros
(define-caller-pattern macro-function (form) :lisp)
(define-caller-pattern defmacro
  (name
   lambda-list
   (:star (:or declaration documentation-string))
   (:star form))
  :lisp)
(define-caller-pattern macroexpand (form (:optional :ignore)) :lisp)
(define-caller-pattern macroexpand-1 (form (:optional :ignore)) :lisp)
(define-variable-pattern *macroexpand-hook* :lisp)

;;; Destructuring
(define-caller-pattern destructuring-bind 
  (lambda-list form
	       (:star declaration)
	       (:star form))
  :lisp2)

;;; Compiler Macros
(define-caller-pattern define-compiler-macro
  (name lambda-list
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(define-caller-pattern compiler-macro-function (form) :lisp2)
(define-caller-pattern compiler-macroexpand (form (:optional :ignore)) :lisp2)
(define-caller-pattern compiler-macroexpand-1 (form (:optional :ignore))
  :lisp2)

;;; Environments
(define-caller-pattern variable-information (form &optional :ignore) 
  :lisp2)
(define-caller-pattern function-information (fn &optional :ignore) :lisp2)
(define-caller-pattern declaration-information (form &optional :ignore) :lisp2)
(define-caller-pattern augment-environment (form &key (:star :ignore)) :lisp2)
(define-caller-pattern define-declaration 
  (name
   lambda-list
   (:star form)) 
  :lisp2)
(define-caller-pattern parse-macro (name lambda-list form) :lisp2)
(define-caller-pattern enclose (form &optional :ignore) :lisp2)


;;; Declarations
(define-caller-pattern declare ((:rest :ignore)) :lisp)
(define-caller-pattern proclaim ((:rest :ignore)) :lisp)
(define-caller-pattern locally ((:star declaration) (:star form)) :lisp)
(define-caller-pattern declaim ((:rest :ignore)) :lisp2)
(define-caller-pattern the (form form) :lisp)

;;; Symbols
(define-caller-pattern get (form form (:optional form)) :lisp)
(define-caller-pattern remprop (form form) :lisp)
(define-caller-pattern symbol-plist (form) :lisp)
(define-caller-pattern getf (form form (:optional form)) :lisp)
(define-caller-pattern remf (form form) :lisp)
(define-caller-pattern get-properties (form form) :lisp)

(define-caller-pattern symbol-name (form) :lisp)
(define-caller-pattern make-symbol (form) :lisp)
(define-caller-pattern copy-symbol (form (:optional :ignore)) :lisp)
(define-caller-pattern gensym ((:optional :ignore)) :lisp)
(define-variable-pattern *gensym-counter* :lisp2)
(define-caller-pattern gentemp ((:optional :ignore :ignore)) :lisp)
(define-caller-pattern symbol-package (form) :lisp)
(define-caller-pattern keywordp (form) :lisp)

;;; Packages
(define-variable-pattern *package* :lisp)
(define-caller-pattern make-package ((:rest :ignore)) :lisp)
(define-caller-pattern in-package ((:rest :ignore)) :lisp)
(define-caller-pattern find-package ((:rest :ignore)) :lisp)
(define-caller-pattern package-name ((:rest :ignore)) :lisp)
(define-caller-pattern package-nicknames ((:rest :ignore)) :lisp)
(define-caller-pattern rename-package ((:rest :ignore)) :lisp)
(define-caller-pattern package-use-list ((:rest :ignore)) :lisp)
(define-caller-pattern package-used-by-list ((:rest :ignore)) :lisp)
(define-caller-pattern package-shadowing-symbols ((:rest :ignore)) :lisp)
(define-caller-pattern list-all-packages () :lisp)
(define-caller-pattern delete-package ((:rest :ignore)) :lisp2)
(define-caller-pattern intern (form &optional :ignore) :lisp)
(define-caller-pattern find-symbol (form &optional :ignore) :lisp)
(define-caller-pattern unintern (form &optional :ignore) :lisp)

(define-caller-pattern export ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(define-caller-pattern unexport ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(define-caller-pattern import ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(define-caller-pattern shadowing-import ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)
(define-caller-pattern shadow ((:or symbol ((:star symbol)))
			       &optional :ignore) :lisp)

(define-caller-pattern use-package ((:rest :ignore)) :lisp)
(define-caller-pattern unuse-package ((:rest :ignore)) :lisp)
(define-caller-pattern defpackage (name (:rest :ignore)) :lisp2)
(define-caller-pattern find-all-symbols (form) :lisp)
(define-caller-pattern do-symbols 
  ((var (:optional form (:optional form)))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(define-caller-pattern do-external-symbols 
  ((var (:optional form (:optional form)))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(define-caller-pattern do-all-symbols 
  ((var (:optional form))
   (:star declaration) 
   (:star (:or tag form))) 
  :lisp)
(define-caller-pattern with-package-iterator
  ((name form (:plus :ignore))
   (:star form))
  :lisp2)

;;; Modules
(define-variable-pattern *modules* :lisp)
(define-caller-pattern provide (form) :lisp)
(define-caller-pattern require (form &optional :ignore) :lisp)


;;; Numbers
(define-caller-pattern zerop (form) :lisp)
(define-caller-pattern plusp (form) :lisp)
(define-caller-pattern minusp (form) :lisp)
(define-caller-pattern oddp (form) :lisp)
(define-caller-pattern evenp (form) :lisp)

(define-caller-pattern = (form (:star form)) :lisp)
(define-caller-pattern /= (form (:star form)) :lisp)
(define-caller-pattern > (form (:star form)) :lisp)
(define-caller-pattern < (form (:star form)) :lisp)
(define-caller-pattern <= (form (:star form)) :lisp)
(define-caller-pattern >= (form (:star form)) :lisp)

(define-caller-pattern max (form (:star form)) :lisp)
(define-caller-pattern min (form (:star form)) :lisp)

(define-caller-pattern - (form (:star form)) :lisp)
(define-caller-pattern + (form (:star form)) :lisp)
(define-caller-pattern * (form (:star form)) :lisp)
(define-caller-pattern / (form (:star form)) :lisp)
(define-caller-pattern 1+ (form) :lisp)
(define-caller-pattern 1- (form) :lisp)

(define-caller-pattern incf (form form) :lisp)
(define-caller-pattern decf (form form) :lisp)

(define-caller-pattern conjugate (form) :lisp)

(define-caller-pattern gcd ((:star form)) :lisp)
(define-caller-pattern lcm ((:star form)) :lisp)

(define-caller-pattern exp (form) :lisp)
(define-caller-pattern expt (form form) :lisp)
(define-caller-pattern log (form (:optional form)) :lisp)
(define-caller-pattern sqrt (form) :lisp)
(define-caller-pattern isqrt (form) :lisp)

(define-caller-pattern abs (form) :lisp)
(define-caller-pattern phase (form) :lisp)
(define-caller-pattern signum (form) :lisp)
(define-caller-pattern sin (form) :lisp)
(define-caller-pattern cos (form) :lisp)
(define-caller-pattern tan (form) :lisp)
(define-caller-pattern cis (form) :lisp)
(define-caller-pattern asin (form) :lisp)
(define-caller-pattern acos (form) :lisp)
(define-caller-pattern atan (form &optional form) :lisp)
(define-variable-pattern pi :lisp)

(define-caller-pattern sinh (form) :lisp)
(define-caller-pattern cosh (form) :lisp)
(define-caller-pattern tanh (form) :lisp)
(define-caller-pattern asinh (form) :lisp)
(define-caller-pattern acosh (form) :lisp)
(define-caller-pattern atanh (form) :lisp)

;;; Type Conversions and Extractions
(define-caller-pattern float (form (:optional form)) :lisp)
(define-caller-pattern rational (form) :lisp)
(define-caller-pattern rationalize (form) :lisp)
(define-caller-pattern numerator (form) :lisp)
(define-caller-pattern denominator (form) :lisp)

(define-caller-pattern floor (form (:optional form)) :lisp)
(define-caller-pattern ceiling (form (:optional form)) :lisp)
(define-caller-pattern truncate (form (:optional form)) :lisp)
(define-caller-pattern round (form (:optional form)) :lisp)

(define-caller-pattern mod (form form) :lisp)
(define-caller-pattern rem (form form) :lisp)

(define-caller-pattern ffloor (form (:optional form)) :lisp)
(define-caller-pattern fceiling (form (:optional form)) :lisp)
(define-caller-pattern ftruncate (form (:optional form)) :lisp)
(define-caller-pattern fround (form (:optional form)) :lisp)

(define-caller-pattern decode-float (form) :lisp)
(define-caller-pattern scale-float (form form) :lisp)
(define-caller-pattern float-radix (form) :lisp)
(define-caller-pattern float-sign (form (:optional form)) :lisp)
(define-caller-pattern float-digits (form) :lisp)
(define-caller-pattern float-precision (form) :lisp)
(define-caller-pattern integer-decode-float (form) :lisp)

(define-caller-pattern complex (form (:optional form)) :lisp)
(define-caller-pattern realpart (form) :lisp)
(define-caller-pattern imagpart (form) :lisp)

(define-caller-pattern logior ((:star form)) :lisp)
(define-caller-pattern logxor ((:star form)) :lisp)
(define-caller-pattern logand ((:star form)) :lisp)
(define-caller-pattern logeqv ((:star form)) :lisp)

(define-caller-pattern lognand (form form) :lisp)
(define-caller-pattern lognor (form form) :lisp)
(define-caller-pattern logandc1 (form form) :lisp)
(define-caller-pattern logandc2 (form form) :lisp)
(define-caller-pattern logorc1 (form form) :lisp)
(define-caller-pattern logorc2 (form form) :lisp)

(define-caller-pattern boole (form form form) :lisp)
(define-variable-pattern boole-clr :lisp)
(define-variable-pattern boole-set :lisp)
(define-variable-pattern boole-1 :lisp)
(define-variable-pattern boole-2 :lisp)
(define-variable-pattern boole-c1 :lisp)
(define-variable-pattern boole-c2 :lisp)
(define-variable-pattern boole-and :lisp)
(define-variable-pattern boole-ior :lisp)
(define-variable-pattern boole-xor :lisp)
(define-variable-pattern boole-eqv :lisp)
(define-variable-pattern boole-nand :lisp)
(define-variable-pattern boole-nor :lisp)
(define-variable-pattern boole-andc1 :lisp)
(define-variable-pattern boole-andc2 :lisp)
(define-variable-pattern boole-orc1 :lisp)
(define-variable-pattern boole-orc2 :lisp)

(define-caller-pattern lognot (form) :lisp)
(define-caller-pattern logtest (form form) :lisp)
(define-caller-pattern logbitp (form form) :lisp)
(define-caller-pattern ash (form form) :lisp)
(define-caller-pattern logcount (form) :lisp)
(define-caller-pattern integer-length (form) :lisp)

(define-caller-pattern byte (form form) :lisp)
(define-caller-pattern byte-size (form) :lisp)
(define-caller-pattern byte-position (form) :lisp)
(define-caller-pattern ldb (form form) :lisp)
(define-caller-pattern ldb-test (form form) :lisp)
(define-caller-pattern mask-field (form form) :lisp)
(define-caller-pattern dpb (form form form) :lisp)
(define-caller-pattern deposit-field (form form form) :lisp)

;;; Random Numbers
(define-caller-pattern random (form (:optional form)) :lisp)
(define-variable-pattern *random-state* :lisp)
(define-caller-pattern make-random-state ((:optional form)) :lisp)
(define-caller-pattern random-state-p (form) :lisp)

;;; Implementation Parameters
(define-variable-pattern most-positive-fixnum :lisp)
(define-variable-pattern most-negative-fixnum :lisp)
(define-variable-pattern most-positive-short-float :lisp)
(define-variable-pattern least-positive-short-float :lisp)
(define-variable-pattern least-negative-short-float :lisp)
(define-variable-pattern most-negative-short-float :lisp)
(define-variable-pattern most-positive-single-float :lisp)
(define-variable-pattern least-positive-single-float :lisp)
(define-variable-pattern least-negative-single-float :lisp)
(define-variable-pattern most-negative-single-float :lisp)
(define-variable-pattern most-positive-double-float :lisp)
(define-variable-pattern least-positive-double-float :lisp)
(define-variable-pattern least-negative-double-float :lisp)
(define-variable-pattern most-negative-double-float :lisp)
(define-variable-pattern most-positive-long-float :lisp)
(define-variable-pattern least-positive-long-float :lisp)
(define-variable-pattern least-negative-long-float :lisp)
(define-variable-pattern most-negative-long-float :lisp)
(define-variable-pattern least-positive-normalized-short-float :lisp2)
(define-variable-pattern least-negative-normalized-short-float :lisp2)
(define-variable-pattern least-positive-normalized-single-float :lisp2)
(define-variable-pattern least-negative-normalized-single-float :lisp2)
(define-variable-pattern least-positive-normalized-double-float :lisp2)
(define-variable-pattern least-negative-normalized-double-float :lisp2)
(define-variable-pattern least-positive-normalized-long-float :lisp2)
(define-variable-pattern least-negative-normalized-long-float :lisp2)
(define-variable-pattern short-float-epsilon :lisp)
(define-variable-pattern single-float-epsilon :lisp)
(define-variable-pattern double-float-epsilon :lisp)
(define-variable-pattern long-float-epsilon :lisp)
(define-variable-pattern short-float-negative-epsilon :lisp)
(define-variable-pattern single-float-negative-epsilon :lisp)
(define-variable-pattern double-float-negative-epsilon :lisp)
(define-variable-pattern long-float-negative-epsilon :lisp)

;;; Characters 
(define-variable-pattern char-code-limit :lisp)
(define-variable-pattern char-font-limit :lisp)
(define-variable-pattern char-bits-limit :lisp)
(define-caller-pattern standard-char-p (form) :lisp)
(define-caller-pattern graphic-char-p (form) :lisp)
(define-caller-pattern string-char-p (form) :lisp)
(define-caller-pattern alpha-char-p (form) :lisp)
(define-caller-pattern upper-case-p (form) :lisp)
(define-caller-pattern lower-case-p (form) :lisp)
(define-caller-pattern both-case-p (form) :lisp)
(define-caller-pattern digit-char-p (form (:optional form)) :lisp)
(define-caller-pattern alphanumericp (form) :lisp)

(define-caller-pattern char= ((:star form)) :lisp)
(define-caller-pattern char/= ((:star form)) :lisp)
(define-caller-pattern char< ((:star form)) :lisp)
(define-caller-pattern char> ((:star form)) :lisp)
(define-caller-pattern char<= ((:star form)) :lisp)
(define-caller-pattern char>= ((:star form)) :lisp)

(define-caller-pattern char-equal ((:star form)) :lisp)
(define-caller-pattern char-not-equal ((:star form)) :lisp)
(define-caller-pattern char-lessp ((:star form)) :lisp)
(define-caller-pattern char-greaterp ((:star form)) :lisp)
(define-caller-pattern char-not-greaterp ((:star form)) :lisp)
(define-caller-pattern char-not-lessp ((:star form)) :lisp)

(define-caller-pattern char-code (form) :lisp)
(define-caller-pattern char-bits (form) :lisp)
(define-caller-pattern char-font (form) :lisp)
(define-caller-pattern code-char (form (:optional form form)) :lisp)
(define-caller-pattern make-char (form (:optional form form)) :lisp)
(define-caller-pattern characterp (form) :lisp)
(define-caller-pattern char-upcase (form) :lisp)
(define-caller-pattern char-downcase (form) :lisp)
(define-caller-pattern digit-char (form (:optional form form)) :lisp)
(define-caller-pattern char-int (form) :lisp)
(define-caller-pattern int-char (form) :lisp)
(define-caller-pattern char-name (form) :lisp)
(define-caller-pattern name-char (form) :lisp)
(define-variable-pattern char-control-bit :lisp)
(define-variable-pattern char-meta-bit :lisp)
(define-variable-pattern char-super-bit :lisp)
(define-variable-pattern char-hyper-bit :lisp)
(define-caller-pattern char-bit (form form) :lisp)
(define-caller-pattern set-char-bit (form form form) :lisp)

;;; Sequences
(define-caller-pattern complement (fn) :lisp2)
(define-caller-pattern elt (form form) :lisp)
(define-caller-pattern subseq (form form &optional form) :lisp)
(define-caller-pattern copy-seq (form) :lisp)
(define-caller-pattern length (form) :lisp)
(define-caller-pattern reverse (form) :lisp)
(define-caller-pattern nreverse (form) :lisp)
(define-caller-pattern make-sequence (form form &key form) :lisp)

(define-caller-pattern concatenate (form (:star form)) :lisp)
(define-caller-pattern map (form fn form (:star form)) :lisp)
(define-caller-pattern map-into (form fn (:star form)) :lisp2)

(define-caller-pattern some (fn form (:star form)) :lisp)
(define-caller-pattern every (fn form (:star form)) :lisp)
(define-caller-pattern notany (fn form (:star form)) :lisp)
(define-caller-pattern notevery (fn form (:star form)) :lisp)

(define-caller-pattern reduce (fn form &key (:star form)) :lisp)
(define-caller-pattern fill (form form &key (:star form)) :lisp)
(define-caller-pattern replace (form form &key (:star form)) :lisp)
(define-caller-pattern remove (form form &key (:star form)) :lisp)
(define-caller-pattern remove-if (fn form &key (:star form)) :lisp)
(define-caller-pattern remove-if-not (fn form &key (:star form)) :lisp)
(define-caller-pattern delete (form form &key (:star form)) :lisp)
(define-caller-pattern delete-if (fn form &key (:star form)) :lisp)
(define-caller-pattern delete-if-not (fn form &key (:star form)) :lisp)
(define-caller-pattern remove-duplicates (form &key (:star form)) :lisp)
(define-caller-pattern delete-duplicates (form &key (:star form)) :lisp)
(define-caller-pattern substitute (form form form &key (:star form)) :lisp)
(define-caller-pattern substitute-if (form fn form &key (:star form)) :lisp)
(define-caller-pattern substitute-if-not (form fn form &key (:star form))
  :lisp)
(define-caller-pattern nsubstitute (form form form &key (:star form)) :lisp)
(define-caller-pattern nsubstitute-if (form fn form &key (:star form)) :lisp)
(define-caller-pattern nsubstitute-if-not (form fn form &key (:star form))
  :lisp)
(define-caller-pattern find (form form &key (:star form)) :lisp)
(define-caller-pattern find-if (fn form &key (:star form)) :lisp)
(define-caller-pattern find-if-not (fn form &key (:star form)) :lisp)
(define-caller-pattern position (form form &key (:star form)) :lisp)
(define-caller-pattern position-if (fn form &key (:star form)) :lisp)
(define-caller-pattern position-if-not (fn form &key (:star form)) :lisp)
(define-caller-pattern count (form form &key (:star form)) :lisp)
(define-caller-pattern count-if (fn form &key (:star form)) :lisp)
(define-caller-pattern count-if-not (fn form &key (:star form)) :lisp)
(define-caller-pattern mismatch (form form &key (:star form)) :lisp)
(define-caller-pattern search (form form &key (:star form)) :lisp)
(define-caller-pattern sort (form fn &key (:star form)) :lisp)
(define-caller-pattern stable-sort (form fn &key (:star form)) :lisp)
(define-caller-pattern merge (form form form fn &key (:star form)) :lisp)

;;; Lists
(define-caller-pattern car (form) :lisp)
(define-caller-pattern cdr (form) :lisp)
(define-caller-pattern caar (form) :lisp)
(define-caller-pattern cadr (form) :lisp)
(define-caller-pattern cdar (form) :lisp)
(define-caller-pattern cddr (form) :lisp)
(define-caller-pattern caaar (form) :lisp)
(define-caller-pattern caadr (form) :lisp)
(define-caller-pattern cadar (form) :lisp)
(define-caller-pattern caddr (form) :lisp)
(define-caller-pattern cdaar (form) :lisp)
(define-caller-pattern cdadr (form) :lisp)
(define-caller-pattern cddar (form) :lisp)
(define-caller-pattern cdddr (form) :lisp)
(define-caller-pattern caaaar (form) :lisp)
(define-caller-pattern caaadr (form) :lisp)
(define-caller-pattern caadar (form) :lisp)
(define-caller-pattern caaddr (form) :lisp)
(define-caller-pattern cadaar (form) :lisp)
(define-caller-pattern cadadr (form) :lisp)
(define-caller-pattern caddar (form) :lisp)
(define-caller-pattern cadddr (form) :lisp)
(define-caller-pattern cdaaar (form) :lisp)
(define-caller-pattern cdaadr (form) :lisp)
(define-caller-pattern cdadar (form) :lisp)
(define-caller-pattern cdaddr (form) :lisp)
(define-caller-pattern cddaar (form) :lisp)
(define-caller-pattern cddadr (form) :lisp)
(define-caller-pattern cdddar (form) :lisp)
(define-caller-pattern cddddr (form) :lisp)

(define-caller-pattern cons (form form) :lisp)
(define-caller-pattern tree-equal (form form &key (:star fn)) :lisp)
(define-caller-pattern endp (form) :lisp)
(define-caller-pattern list-length (form) :lisp)
(define-caller-pattern nth (form form) :lisp)

(define-caller-pattern first (form) :lisp)
(define-caller-pattern second (form) :lisp)
(define-caller-pattern third (form) :lisp)
(define-caller-pattern fourth (form) :lisp)
(define-caller-pattern fifth (form) :lisp)
(define-caller-pattern sixth (form) :lisp)
(define-caller-pattern seventh (form) :lisp)
(define-caller-pattern eighth (form) :lisp)
(define-caller-pattern ninth (form) :lisp)
(define-caller-pattern tenth (form) :lisp)

(define-caller-pattern rest (form) :lisp)
(define-caller-pattern nthcdr (form form) :lisp)
(define-caller-pattern last (form (:optional form)) :lisp)
(define-caller-pattern list ((:star form)) :lisp)
(define-caller-pattern list* ((:star form)) :lisp)
(define-caller-pattern make-list (form &key (:star form)) :lisp)
(define-caller-pattern append ((:star form)) :lisp)
(define-caller-pattern copy-list (form) :lisp)
(define-caller-pattern copy-alist (form) :lisp)
(define-caller-pattern copy-tree (form) :lisp)
(define-caller-pattern revappend (form form) :lisp)
(define-caller-pattern nconc ((:star form)) :lisp)
(define-caller-pattern nreconc (form form) :lisp)
(define-caller-pattern push (form form) :lisp)
(define-caller-pattern pushnew (form form &key (:star form)) :lisp)
(define-caller-pattern pop (form) :lisp)
(define-caller-pattern butlast (form (:optional form)) :lisp)
(define-caller-pattern nbutlast (form (:optional form)) :lisp)
(define-caller-pattern ldiff (form form) :lisp)
(define-caller-pattern rplaca (form form) :lisp)
(define-caller-pattern rplacd (form form) :lisp)

(define-caller-pattern subst (form form form &key (:star form)) :lisp)
(define-caller-pattern subst-if (form fn form &key (:star form)) :lisp)
(define-caller-pattern subst-if-not (form fn form &key (:star form)) :lisp)
(define-caller-pattern nsubst (form form form &key (:star form)) :lisp)
(define-caller-pattern nsubst-if (form fn form &key (:star form)) :lisp)
(define-caller-pattern nsubst-if-not (form fn form &key (:star form)) :lisp)
(define-caller-pattern sublis (form form &key (:star form)) :lisp)
(define-caller-pattern nsublis (form form &key (:star form)) :lisp)
(define-caller-pattern member (form form &key (:star form)) :lisp)
(define-caller-pattern member-if (fn form &key (:star form)) :lisp)
(define-caller-pattern member-if-not (fn form &key (:star form)) :lisp)

(define-caller-pattern tailp (form form) :lisp)
(define-caller-pattern adjoin (form form &key (:star form)) :lisp)
(define-caller-pattern union (form form &key (:star form)) :lisp)
(define-caller-pattern nunion (form form &key (:star form)) :lisp)
(define-caller-pattern intersection (form form &key (:star form)) :lisp)
(define-caller-pattern nintersection (form form &key (:star form)) :lisp)
(define-caller-pattern set-difference (form form &key (:star form)) :lisp)
(define-caller-pattern nset-difference (form form &key (:star form)) :lisp)
(define-caller-pattern set-exclusive-or (form form &key (:star form)) :lisp)
(define-caller-pattern nset-exclusive-or (form form &key (:star form)) :lisp)
(define-caller-pattern subsetp (form form &key (:star form)) :lisp)

(define-caller-pattern acons (form form form) :lisp)
(define-caller-pattern pairlis (form form (:optional form)) :lisp)
(define-caller-pattern assoc (form form &key (:star form)) :lisp)
(define-caller-pattern assoc-if (fn form) :lisp)
(define-caller-pattern assoc-if-not (fn form) :lisp)
(define-caller-pattern rassoc (form form &key (:star form)) :lisp)
(define-caller-pattern rassoc-if (fn form &key (:star form)) :lisp)
(define-caller-pattern rassoc-if-not (fn form &key (:star form)) :lisp)

;;; Hash Tables
(define-caller-pattern make-hash-table (&key (:star form)) :lisp)
(define-caller-pattern hash-table-p (form) :lisp)
(define-caller-pattern gethash (form form (:optional form)) :lisp)
(define-caller-pattern remhash (form form) :lisp)
(define-caller-pattern maphash (fn form) :lisp)
(define-caller-pattern clrhash (form) :lisp)
(define-caller-pattern hash-table-count (form) :lisp)
(define-caller-pattern with-hash-table-iterator
  ((name form) (:star form)) :lisp2)
(define-caller-pattern hash-table-rehash-size (form) :lisp2)
(define-caller-pattern hash-table-rehash-threshold (form) :lisp2)
(define-caller-pattern hash-table-size (form) :lisp2)
(define-caller-pattern hash-table-test (form) :lisp2)
(define-caller-pattern sxhash (form) :lisp)

;;; Arrays
(define-caller-pattern make-array (form &key (:star form)) :lisp)
(define-variable-pattern array-rank-limit :lisp)
(define-variable-pattern array-dimension-limit :lisp)
(define-variable-pattern array-total-size-limit :lisp)
(define-caller-pattern vector ((:star form)) :lisp)
(define-caller-pattern aref (form (:star form)) :lisp)
(define-caller-pattern svref (form form) :lisp)
(define-caller-pattern array-element-type (form) :lisp)
(define-caller-pattern array-rank (form) :lisp)
(define-caller-pattern array-dimension (form form) :lisp)
(define-caller-pattern array-dimensions (form) :lisp)
(define-caller-pattern array-total-size (form) :lisp)
(define-caller-pattern array-in-bounds-p (form (:star form)) :lisp)
(define-caller-pattern array-row-major-index (form (:star form)) :lisp)
(define-caller-pattern row-major-aref (form form) :lisp2)
(define-caller-pattern adjustable-array-p (form) :lisp)

(define-caller-pattern bit (form (:star form)) :lisp)
(define-caller-pattern sbit (form (:star form)) :lisp)

(define-caller-pattern bit-and (form form (:optional form)) :lisp)
(define-caller-pattern bit-ior (form form (:optional form)) :lisp)
(define-caller-pattern bit-xor (form form (:optional form)) :lisp)
(define-caller-pattern bit-eqv (form form (:optional form)) :lisp)
(define-caller-pattern bit-nand (form form (:optional form)) :lisp)
(define-caller-pattern bit-nor (form form (:optional form)) :lisp)
(define-caller-pattern bit-andc1 (form form (:optional form)) :lisp)
(define-caller-pattern bit-andc2 (form form (:optional form)) :lisp)
(define-caller-pattern bit-orc1 (form form (:optional form)) :lisp)
(define-caller-pattern bit-orc2 (form form (:optional form)) :lisp)
(define-caller-pattern bit-not (form (:optional form)) :lisp)

(define-caller-pattern array-has-fill-pointer-p (form) :lisp)
(define-caller-pattern fill-pointer (form) :lisp)
(define-caller-pattern vector-push (form form) :lisp)
(define-caller-pattern vector-push-extend (form form (:optional form)) :lisp)
(define-caller-pattern vector-pop (form) :lisp)
(define-caller-pattern adjust-array (form form &key (:star form)) :lisp)

;;; Strings
(define-caller-pattern char (form form) :lisp)
(define-caller-pattern schar (form form) :lisp)
(define-caller-pattern string= (form form &key (:star form)) :lisp)
(define-caller-pattern string-equal (form form &key (:star form)) :lisp)
(define-caller-pattern string< (form form &key (:star form)) :lisp)
(define-caller-pattern string> (form form &key (:star form)) :lisp)
(define-caller-pattern string<= (form form &key (:star form)) :lisp)
(define-caller-pattern string>= (form form &key (:star form)) :lisp)
(define-caller-pattern string/= (form form &key (:star form)) :lisp)
(define-caller-pattern string-lessp (form form &key (:star form)) :lisp)
(define-caller-pattern string-greaterp (form form &key (:star form)) :lisp)
(define-caller-pattern string-not-greaterp (form form &key (:star form)) :lisp)
(define-caller-pattern string-not-lessp (form form &key (:star form)) :lisp)
(define-caller-pattern string-not-equal (form form &key (:star form)) :lisp)

(define-caller-pattern make-string (form &key (:star form)) :lisp)
(define-caller-pattern string-trim (form form) :lisp)
(define-caller-pattern string-left-trim (form form) :lisp)
(define-caller-pattern string-right-trim (form form) :lisp)
(define-caller-pattern string-upcase (form &key (:star form)) :lisp)
(define-caller-pattern string-downcase (form &key (:star form)) :lisp)
(define-caller-pattern string-capitalize (form &key (:star form)) :lisp)
(define-caller-pattern nstring-upcase (form &key (:star form)) :lisp)
(define-caller-pattern nstring-downcase (form &key (:star form)) :lisp)
(define-caller-pattern nstring-capitalize (form &key (:star form)) :lisp)
(define-caller-pattern string (form) :lisp)

;;; Structures
(define-caller-pattern defstruct 
  ((:or name (name (:rest :ignore)))
   (:optional documentation-string)
   (:plus :ignore))
  :lisp)

;;; The Evaluator
(define-caller-pattern eval (form) :lisp)
(define-variable-pattern *evalhook* :lisp)
(define-variable-pattern *applyhook* :lisp)
(define-caller-pattern evalhook (form fn fn &optional :ignore) :lisp)
(define-caller-pattern applyhook (fn form fn fn &optional :ignore) :lisp)
(define-caller-pattern constantp (form) :lisp)

;;; Streams
(define-variable-pattern *standard-input* :lisp)
(define-variable-pattern *standard-output* :lisp)
(define-variable-pattern *error-output* :lisp)
(define-variable-pattern *query-io* :lisp)
(define-variable-pattern *debug-io* :lisp)
(define-variable-pattern *terminal-io* :lisp)
(define-variable-pattern *trace-output* :lisp)
(define-caller-pattern make-synonym-stream (symbol) :lisp)
(define-caller-pattern make-broadcast-stream ((:star form)) :lisp)
(define-caller-pattern make-concatenated-stream ((:star form)) :lisp)
(define-caller-pattern make-two-way-stream (form form) :lisp)
(define-caller-pattern make-echo-stream (form form) :lisp)
(define-caller-pattern make-string-input-stream (form &optional form form)
  :lisp)
(define-caller-pattern make-string-output-stream (&key (:star form)) :lisp)
(define-caller-pattern get-output-stream-string (form) :lisp)

(define-caller-pattern with-open-stream
  ((var form)
   (:star declaration)
   (:star form))
  :lisp)

(define-caller-pattern with-input-from-string
  ((var form &key (:star form))
   (:star declaration)
   (:star form))
  :lisp)

(define-caller-pattern with-output-to-string
  ((var (:optional form))
   (:star declaration)
   (:star form))
  :lisp)
(define-caller-pattern streamp (form) :lisp)
(define-caller-pattern open-stream-p (form) :lisp2)
(define-caller-pattern input-stream-p (form) :lisp)
(define-caller-pattern output-stream-p (form) :lisp)
(define-caller-pattern stream-element-type (form) :lisp)
(define-caller-pattern close (form (:rest :ignore)) :lisp)
(define-caller-pattern broadcast-stream-streams (form) :lisp2)
(define-caller-pattern concatenated-stream-streams (form) :lisp2)
(define-caller-pattern echo-stream-input-stream (form) :lisp2)
(define-caller-pattern echo-stream-output-stream (form) :lisp2)
(define-caller-pattern synonym-stream-symbol (form) :lisp2)
(define-caller-pattern two-way-stream-input-stream (form) :lisp2)
(define-caller-pattern two-way-stream-output-stream (form) :lisp2)
(define-caller-pattern interactive-stream-p (form) :lisp2)
(define-caller-pattern stream-external-format (form) :lisp2)

;;; Reader
(define-variable-pattern *read-base* :lisp)
(define-variable-pattern *read-suppress* :lisp)
(define-variable-pattern *read-eval* :lisp2)
(define-variable-pattern *readtable* :lisp)
(define-caller-pattern copy-readtable (&optional form form) :lisp)
(define-caller-pattern readtablep (form) :lisp)
(define-caller-pattern set-syntax-from-char (form form &optional form form)
  :lisp)
(define-caller-pattern set-macro-character (form fn &optional form) :lisp)
(define-caller-pattern get-macro-character (form (:optional form)) :lisp)
(define-caller-pattern make-dispatch-macro-character (form &optional form form)
  :lisp)
(define-caller-pattern set-dispatch-macro-character
  (form form fn (:optional form)) :lisp)
(define-caller-pattern get-dispatch-macro-character
  (form form (:optional form)) :lisp)
(define-caller-pattern readtable-case (form) :lisp2)
(define-variable-pattern *print-readably* :lisp2)
(define-variable-pattern *print-escape* :lisp)
(define-variable-pattern *print-pretty* :lisp)
(define-variable-pattern *print-circle* :lisp)
(define-variable-pattern *print-base* :lisp)
(define-variable-pattern *print-radix* :lisp)
(define-variable-pattern *print-case* :lisp)
(define-variable-pattern *print-gensym* :lisp)
(define-variable-pattern *print-level* :lisp)
(define-variable-pattern *print-length* :lisp)
(define-variable-pattern *print-array* :lisp)
(define-caller-pattern with-standard-io-syntax 
  ((:star declaration)
   (:star form))
  :lisp2)

(define-caller-pattern read (&optional form form form form) :lisp)
(define-variable-pattern *read-default-float-format* :lisp)
(define-caller-pattern read-preserving-whitespace
  (&optional form form form form) :lisp)
(define-caller-pattern read-delimited-list (form &optional form form) :lisp)
(define-caller-pattern read-line (&optional form form form form) :lisp)
(define-caller-pattern read-char (&optional form form form form) :lisp)
(define-caller-pattern unread-char (form (:optional form)) :lisp)
(define-caller-pattern peek-char (&optional form form form form) :lisp)
(define-caller-pattern listen ((:optional form)) :lisp)
(define-caller-pattern read-char-no-hang ((:star form)) :lisp)
(define-caller-pattern clear-input ((:optional form)) :lisp)
(define-caller-pattern read-from-string (form (:star form)) :lisp)
(define-caller-pattern parse-integer (form &rest :ignore) :lisp)
(define-caller-pattern read-byte ((:star form)) :lisp)

(define-caller-pattern write (form &key (:star form)) :lisp)
(define-caller-pattern prin1 (form (:optional form)) :lisp)
(define-caller-pattern print (form (:optional form)) :lisp)
(define-caller-pattern pprint (form (:optional form)) :lisp)
(define-caller-pattern princ (form (:optional form)) :lisp)
(define-caller-pattern write-to-string (form &key (:star form)) :lisp)
(define-caller-pattern prin1-to-string (form) :lisp)
(define-caller-pattern princ-to-string (form) :lisp)
(define-caller-pattern write-char (form (:optional form)) :lisp)
(define-caller-pattern write-string (form &optional form &key (:star form))
  :lisp)
(define-caller-pattern write-line (form &optional form &key (:star form))
  :lisp)
(define-caller-pattern terpri ((:optional form)) :lisp)
(define-caller-pattern fresh-line ((:optional form)) :lisp)
(define-caller-pattern finish-output ((:optional form)) :lisp)
(define-caller-pattern force-output ((:optional form)) :lisp)
(define-caller-pattern clear-output ((:optional form)) :lisp)
(define-caller-pattern print-unreadable-object 
  ((form form &key (:star form))
   (:star declaration)
   (:star form))
  :lisp2)
(define-caller-pattern write-byte (form form) :lisp)
(define-caller-pattern format
  (destination
   control-string
   (:rest format-arguments))
  :lisp)

(define-caller-pattern y-or-n-p (control-string (:star form)) :lisp)
(define-caller-pattern yes-or-no-p (control-string (:star form)) :lisp)

;;; Pathnames
(define-caller-pattern wild-pathname-p (form &optional form) :lisp2)
(define-caller-pattern pathname-match-p (form form) :lisp2)
(define-caller-pattern translate-pathname (form form form &key (:star form))
  :lisp2)

(define-caller-pattern logical-pathname (form) :lisp2)
(define-caller-pattern translate-logical-pathname (form &key (:star form))
  :lisp2)
(define-caller-pattern logical-pathname-translations (form) :lisp2)
(define-caller-pattern load-logical-pathname-translations (form) :lisp2)
(define-caller-pattern compile-file-pathname (form &key form) :lisp2)

(define-caller-pattern pathname (form) :lisp)
(define-caller-pattern truename (form) :lisp)
(define-caller-pattern parse-namestring ((:star form)) :lisp)
(define-caller-pattern merge-pathnames ((:star form)) :lisp)
(define-variable-pattern *default-pathname-defaults* :lisp)
(define-caller-pattern make-pathname ((:star form)) :lisp)
(define-caller-pattern pathnamep (form) :lisp)
(define-caller-pattern pathname-host (form) :lisp)
(define-caller-pattern pathname-device (form) :lisp)
(define-caller-pattern pathname-directory (form) :lisp)
(define-caller-pattern pathname-name (form) :lisp)
(define-caller-pattern pathname-type (form) :lisp)
(define-caller-pattern pathname-version (form) :lisp)
(define-caller-pattern namestring (form) :lisp)
(define-caller-pattern file-namestring (form) :lisp)
(define-caller-pattern directory-namestring (form) :lisp)
(define-caller-pattern host-namestring (form) :lisp)
(define-caller-pattern enough-namestring (form (:optional form)) :lisp)
(define-caller-pattern user-homedir-pathname (&optional form) :lisp)
(define-caller-pattern open (form &key (:star form)) :lisp)
(define-caller-pattern with-open-file
  ((var form (:rest :ignore))
   (:star declaration)
   (:star form))
 :lisp)

(define-caller-pattern rename-file (form form) :lisp)
(define-caller-pattern delete-file (form) :lisp)
(define-caller-pattern probe-file (form) :lisp)
(define-caller-pattern file-write-date (form) :lisp)
(define-caller-pattern file-author (form) :lisp)
(define-caller-pattern file-position (form (:optional form)) :lisp)
(define-caller-pattern file-length (form) :lisp)
(define-caller-pattern file-string-length (form form) :lisp2)
(define-caller-pattern load (form &key (:star form)) :lisp)
(define-variable-pattern *load-verbose* :lisp)
(define-variable-pattern *load-print* :lisp2)
(define-variable-pattern *load-pathname* :lisp2)
(define-variable-pattern *load-truename* :lisp2)
(define-caller-pattern make-load-form (form) :lisp2)
(define-caller-pattern make-load-form-saving-slots (form &optional form)
  :lisp2)
(define-caller-pattern directory (form &key (:star form)) :lisp)

;;; Errors
(define-caller-pattern error (form (:star form)) :lisp)
(define-caller-pattern cerror (form form (:star form)) :lisp)
(define-caller-pattern warn (form (:star form)) :lisp)
(define-variable-pattern *break-on-warnings* :lisp)
(define-caller-pattern break (&optional form (:star form)) :lisp)
(define-caller-pattern check-type (form form (:optional form)) :lisp)
(define-caller-pattern assert 
  (form
   (:optional ((:star var))
	      (:optional form (:star form)))) 
  :lisp)
(define-caller-pattern etypecase (form (:star (symbol (:star form)))) :lisp)
(define-caller-pattern ctypecase (form (:star (symbol (:star form)))) :lisp)
(define-caller-pattern ecase
  (form
   (:star ((:or symbol ((:star symbol)))
	   (:star form))))
  :lisp)
(define-caller-pattern ccase 
  (form
   (:star ((:or symbol ((:star symbol)))
	   (:star form))))
  :lisp)

;;; The Compiler
(define-caller-pattern compile (form (:optional form)) :lisp)
(define-caller-pattern compile-file (form &key (:star form)) :lisp)
(define-variable-pattern *compile-verbose* :lisp2)
(define-variable-pattern *compile-print* :lisp2)
(define-variable-pattern *compile-file-pathname* :lisp2)
(define-variable-pattern *compile-file-truename* :lisp2)
(define-caller-pattern load-time-value (form (:optional form)) :lisp2)
(define-caller-pattern disassemble (form) :lisp)
(define-caller-pattern function-lambda-expression (fn) :lisp2)
(define-caller-pattern with-compilation-unit (((:star :ignore)) (:star form)) 
  :lisp2)

;;; Documentation
(define-caller-pattern documentation (form form) :lisp)
(define-caller-pattern trace ((:star form)) :lisp)
(define-caller-pattern untrace ((:star form)) :lisp)
(define-caller-pattern step (form) :lisp)
(define-caller-pattern time (form) :lisp)
(define-caller-pattern describe (form &optional form) :lisp)
(define-caller-pattern describe-object (form &optional form) :lisp2)
(define-caller-pattern inspect (form) :lisp)
(define-caller-pattern room ((:optional form)) :lisp)
(define-caller-pattern ed ((:optional form)) :lisp)
(define-caller-pattern dribble ((:optional form)) :lisp)
(define-caller-pattern apropos (form (:optional form)) :lisp)
(define-caller-pattern apropos-list (form (:optional form)) :lisp)
(define-caller-pattern get-decoded-time () :lisp)
(define-caller-pattern get-universal-time () :lisp)
(define-caller-pattern decode-universal-time (form &optional form) :lisp)
(define-caller-pattern encode-universal-time 
  (form form form form form form &optional form) :lisp)
(define-caller-pattern get-internal-run-time () :lisp)
(define-caller-pattern get-internal-real-time () :lisp)
(define-caller-pattern sleep (form) :lisp)

(define-caller-pattern lisp-implementation-type () :lisp)
(define-caller-pattern lisp-implementation-version () :lisp)
(define-caller-pattern machine-type () :lisp)
(define-caller-pattern machine-version () :lisp)
(define-caller-pattern machine-instance () :lisp)
(define-caller-pattern software-type () :lisp)
(define-caller-pattern software-version () :lisp)
(define-caller-pattern short-site-name () :lisp)
(define-caller-pattern long-site-name () :lisp)
(define-variable-pattern *features* :lisp)

(define-caller-pattern identity (form) :lisp)

;;; Pretty Printing
(define-variable-pattern *print-pprint-dispatch* :lisp2)
(define-variable-pattern *print-right-margin* :lisp2)
(define-variable-pattern *print-miser-width* :lisp2)
(define-variable-pattern *print-lines* :lisp2)
(define-caller-pattern pprint-newline (form &optional form) :lisp2)
(define-caller-pattern pprint-logical-block
  ((var form &key (:star form))
   (:star form))
  :lisp2)
(define-caller-pattern pprint-exit-if-list-exhausted () :lisp2)
(define-caller-pattern pprint-pop () :lisp2)
(define-caller-pattern pprint-indent (form form &optional form) :lisp2)
(define-caller-pattern pprint-tab (form form form &optional form) :lisp2)
(define-caller-pattern pprint-fill (form form &optional form form) :lisp2)
(define-caller-pattern pprint-linear (form form &optional form form) :lisp2)
(define-caller-pattern pprint-tabular (form form &optional form form form)
  :lisp2)
(define-caller-pattern formatter (control-string) :lisp2)
(define-caller-pattern copy-pprint-dispatch (&optional form) :lisp2)
(define-caller-pattern pprint-dispatch (form &optional form) :lisp2)
(define-caller-pattern set-pprint-dispatch (form form &optional form form)
  :lisp2)

;;; CLOS
(define-caller-pattern add-method (fn form) :lisp2)
(define-caller-pattern call-method (form form) :lisp2)
(define-caller-pattern call-next-method ((:star form)) :lisp2)
(define-caller-pattern change-class (form form) :lisp2)
(define-caller-pattern class-name (form) :lisp2)
(define-caller-pattern class-of (form) :lisp2)
(define-caller-pattern compute-applicable-methods (fn (:star form)) :lisp2)
(define-caller-pattern defclass (name &rest :ignore) :lisp2)
(define-caller-pattern defgeneric (name lambda-list &rest :ignore) :lisp2)
(define-caller-pattern define-method-combination 
  (name lambda-list ((:star :ignore))
	(:optional ((:eq :arguments) :ignore))
	(:optional ((:eq :generic-function) :ignore))
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(define-caller-pattern defmethod 
  (name (:star symbol) lambda-list
	(:star (:or declaration documentation-string))
	(:star form))
  :lisp2)
(define-caller-pattern ensure-generic-function (name &key (:star form)) :lisp2)
(define-caller-pattern find-class (form &optional form form) :lisp2)
(define-caller-pattern find-method (fn &rest :ignore) :lisp2)
(define-caller-pattern function-keywords (&rest :ignore) :lisp2)
(define-caller-pattern generic-flet (((:star (name lambda-list))) (:star form))
  :lisp2)
(define-caller-pattern generic-labels 
  (((:star (name lambda-list))) (:star form))
  :lisp2)
(define-caller-pattern generic-function (lambda-list) :lisp2)
(define-caller-pattern initialize-instance (form &key (:star form)) :lisp2)
(define-caller-pattern invalid-method-error (fn form (:star form)) :lisp2)
(define-caller-pattern make-instance (fn (:star form)) :lisp2)
(define-caller-pattern make-instances-obsolete (fn) :lisp2)
(define-caller-pattern method-combination-error (form (:star form)) :lisp2)
(define-caller-pattern method-qualifiers (fn) :lisp2)
(define-caller-pattern next-method-p () :lisp2)
(define-caller-pattern no-applicable-method (fn (:star form)) :lisp2)
(define-caller-pattern no-next-method (fn (:star form)) :lisp2)
(define-caller-pattern print-object (form form) :lisp2)
(define-caller-pattern reinitialize-instance (form (:star form)) :lisp2)
(define-caller-pattern remove-method (fn form) :lisp2)
(define-caller-pattern shared-initialize (form form (:star form)) :lisp2)
(define-caller-pattern slot-boundp (form form) :lisp2)
(define-caller-pattern slot-exists-p (form form) :lisp2)
(define-caller-pattern slot-makeunbound (form form) :lisp2)
(define-caller-pattern slot-missing (fn form form form &optional form) :lisp2)
(define-caller-pattern slot-unbound (fn form form) :lisp2)
(define-caller-pattern slot-value (form form) :lisp2)
(define-caller-pattern update-instance-for-different-class 
  (form form (:star form)) :lisp2)
(define-caller-pattern update-instance-for-redefined-class 
  (form form (:star form)) :lisp2)
(define-caller-pattern with-accessors
  (((:star :ignore)) form
   (:star declaration)
   (:star form))
  :lisp2)
(define-caller-pattern with-added-methods
  ((name lambda-list) form
   (:star form))
  :lisp2)
(define-caller-pattern with-slots
  (((:star :ignore)) form
   (:star declaration)
   (:star form))
  :lisp2)

;;; Conditions
(define-caller-pattern signal (form (:star form)) :lisp2)
(define-variable-pattern *break-on-signals* :lisp2)
(define-caller-pattern handler-case (form (:star (form ((:optional var))
						       (:star form))))
  :lisp2)
(define-caller-pattern ignore-errors ((:star form)) :lisp2)
(define-caller-pattern handler-bind (((:star (form form)))
				     (:star form))
  :lisp2)
(define-caller-pattern define-condition (name &rest :ignore) :lisp2)
(define-caller-pattern make-condition (form &rest :ignore) :lisp2)
(define-caller-pattern with-simple-restart
  ((name form (:star form)) (:star form)) :lisp2)
(define-caller-pattern restart-case 
  (form
   (:star (form form (:star form))))
  :lisp2)
(define-caller-pattern restart-bind
  (((:star (name fn &key (:star form))))
   (:star form))
  :lisp2)
(define-caller-pattern with-condition-restarts
  (form form
	(:star declaration)
	(:star form))
  :lisp2)
(define-caller-pattern compute-restarts (&optional form) :lisp2)
(define-caller-pattern restart-name (form) :lisp2)
(define-caller-pattern find-restart (form &optional form) :lisp2)
(define-caller-pattern invoke-restart (form (:star form)) :lisp2)
(define-caller-pattern invoke-restart-interactively (form) :lisp2)
(define-caller-pattern abort (&optional form) :lisp2)
(define-caller-pattern continue (&optional form) :lisp2)
(define-caller-pattern muffle-warning (&optional form) :lisp2)
(define-caller-pattern store-value (form &optional form) :lisp2)
(define-caller-pattern use-value (form &optional form) :lisp2)
(define-caller-pattern invoke-debugger (form) :lisp2)
(define-variable-pattern *debugger-hook* :lisp2)
(define-caller-pattern simple-condition-format-string (form) :lisp2)
(define-caller-pattern simple-condition-format-arguments (form) :lisp2)
(define-caller-pattern type-error-datum (form) :lisp2)
(define-caller-pattern type-error-expected-type (form) :lisp2)
(define-caller-pattern package-error-package (form) :lisp2)
(define-caller-pattern stream-error-stream (form) :lisp2)
(define-caller-pattern file-error-pathname (form) :lisp2)
(define-caller-pattern cell-error-name (form) :lisp2)
(define-caller-pattern arithmetic-error-operation (form) :lisp2)
(define-caller-pattern arithmetic-error-operands (form) :lisp2)

;;; For ZetaLisp Flavors
(define-caller-pattern send (form fn (:star form)) :flavors)
