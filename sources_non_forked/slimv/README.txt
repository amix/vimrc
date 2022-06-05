--------------------------------------------------------------------------------
slimv.vim
--------------------------------------------------------------------------------

Superior Lisp Interaction Mode for Vim ("SLIME for Vim")

Vim script

created by
Tamas Kovacs
 
--------------------------------------------------------------------------------
Description
--------------------------------------------------------------------------------

Slimv is a SWANK client for Vim, similarly to SLIME for Emacs. SWANK is a TCP server for Emacs, which runs a Common Lisp, Clojure or Scheme REPL and provides a socket interface for evaluating, compiling, debugging, profiling lisp code. The SWANK server is embedded in Slimv, but you can also use your own SWANK installation.

Slimv opens the lisp REPL (Read-Eval-Print Loop) inside a Vim buffer. Lisp commands may be entered and executed in the REPL buffer, just as in a regular REPL.

Slimv supports SLIME's debugger, inspector, profiler, cross reference, arglist, indentation, symbol name completion functions. The script also has a Common Lisp Hyperspec lookup feature and it is able to lookup symbols in the Clojure API, as well as in JavaDoc.

Slimv comes with Paredit Mode, which is similar to the functionality of paredit.el in Emacs. Paredit Mode tries to maintain the balanced state of matched characters (parenthesis marks, square and curly braces, double quotes). Matched characters are inserted and removed in pairs, also when working with a block of text (well, mostly). Slimv also implements many paredit.el s-expression handling functions, like Split/Join/Wrap/Splice/Raise. Slurpage and Barfage known from Emacs is also possible but in a different fashion: you don't move the list element in or out of the list, rather you move the opening or closing parenthesis over the element or sub-list.

Please visit the Slimv Tutorial for a more complete introduction:
https://kovisoft.github.io/slimv-tutorial/tutorial.html

Please find the most recent development version in the repository:
https://github.com/kovisoft/slimv

Here follows a list of Slimv commands, any similarity with SLIME's menu is not coincidental. :)

Edit commands:
    *  Close Form
    *  Complete Symbol
    *  Function Arglist
    *  Paredit Toggle

Evaluation commands:
    *  Eval Defun
    *  Eval Current Expression
    *  Eval Region
    *  Eval Buffer
    *  Interactive Eval
    *  Undefine Function

Debug commands:
    *  Macroexpand-1
    *  Macroexpand All
    *  Toggle Trace
    *  Untrace All
    *  Disassemble
    *  Set Breakpoint
    *  Break on Exception
    *  Inspect
    *  Abort
    *  Quit to Toplevel
    *  Continue
    *  Restart Frame
    *  List Threads
    *  Kill Thread
    *  Debug Thread

Compile commands:
    *  Compile Defun
    *  Compile and Load File
    *  Compile File
    *  Compile Region

Cross Reference commands
    *  Who Calls
    *  Who References
    *  Who Sets
    *  Who Binds
    *  Who Macroexpands
    *  Who Specializes
    *  List Callers
    *  List Callees

Profile commands:
    *  Toggle Profile
    *  Profile by Substring
    *  Unprofile All
    *  Show Profiled
    *  Profile Report
    *  Profile Reset

Documentation commands:
    *  Describe Symbol
    *  Apropos
    *  Hyperspec
    *  Generate Tags

REPL commands:
    *  Connect to Server
    *  Interrupt Lisp Process
    *  Send Input
    *  Close and Send Input
    *  Set Package
    *  Previous Input
    *  Next Input
    *  Clear REPL

For more information see the included documentation.
 
---------------------------------------------------------------------------------------------
Installation details
---------------------------------------------------------------------------------------------

Extract the zip archive into your vimfiles or runtime directory.

Slimv works on Windows, Linux and Mac OS X (via Terminal.app), Cygwin is supported. The script requires the following programs installed on your system:
    *  Vim with Python feature enabled
    *  Python (must be the same Python version that was Vim compiled against)
    *  Lisp (any Common Lisp with SLIME support) or Clojure or MIT Scheme (Linux only)

Vim's Python version can be identified with the :ver command, look for the -DDYNAMIC_PYTHON_DLL=\"pythonXX\" string (if you have it). Another way of determining Vim's Python version:

:execute (has('python3') ? "python3" : "python") . " import sys; print(sys.version)"

Slimv tries to autodetect your Lisp/Clojure/Slime installation directories. If it fails to determine the correct directories, then you need to enter the command to start the SWANK server into your vimrc file.

Linux example:
    let g:slimv_swank_cmd = '! xterm -e sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &'

Windows example:
    let g:slimv_swank_cmd = '!start "c:/Program Files/Lisp Cabinet/bin/ccl/wx86cl.exe" -l "c:/Program Files/Lisp Cabinet/site/lisp/slime/start-swank.lisp"'

Mac OS X example:
    let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"sbcl --load ~/.vim/slime/start-swank.lisp\""'

For Clojure use the g:slimv_swank_clojure option, e.g.:
    let g:slimv_swank_clojure = '! xterm -e lein swank &' 


- For pure text-based console without XTerm

If you only have `SSH` and can not use `XTerm`, you can use `tmux` or `screen` instead.

Linux example with `tmux`:
    let g:slimv_swank_cmd = '! tmux new-window -d -n REPL-SBCL "sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp"'

Linux example with `screen`:
    let g:slimv_swank_cmd = '! screen -d -m -t REPL-SBCL sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp'

Mac OS X example with `tmux`:
    let g:slimv_swank_cmd = '!osascript -e "! tmux new-window -d -n REPL-SBCL "sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp"'


See the included documentation for more complete installation and customization instructions.


vim:et:wrap:
