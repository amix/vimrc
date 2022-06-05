[![Build Status](https://github.com/slime/slime/workflows/CI/badge.svg)](https://github.com/slime/slime/actions)
[![MELPA](http://melpa.org/packages/slime-badge.svg?)](http://melpa.org/#/slime) [![MELPA Stable](http://stable.melpa.org/packages/slime-badge.svg?)](http://stable.melpa.org/#/slime)

Overview
--------

SLIME is the Superior Lisp Interaction Mode for Emacs.

SLIME extends Emacs with support for interactive programming in Common
Lisp. The features are centered around slime-mode, an Emacs minor-mode that
complements the standard lisp-mode. While lisp-mode supports editing Lisp
source files, slime-mode adds support for interacting with a running Common
Lisp process for compilation, debugging, documentation lookup, and so on.

For much more information, consult [the manual][1].


Quick setup instructions
------------------------

  1. [Set up the MELPA repository][2], if you haven't already, and install
     SLIME using `M-x package-install RET slime RET`.

  2. In your `~/.emacs` file, point the `inferior-lisp-program`
     variable to your favourite Common Lisp implementation:

     ```el
     (setq inferior-lisp-program "sbcl")
     ```

  3. Use `M-x slime` to fire up and connect to an inferior Lisp. SLIME will
     now automatically be available in your Lisp source buffers.

If you'd like to contribute to SLIME, you will want to instead follow
the manual's instructions on [how to install SLIME via Git][7].


License
-------

SLIME is free software. All files, unless explicitly stated otherwise, are
public domain.


Contact
-------

If you have problems, first have a look at the list of
[known issues and workarounds][6]. 

Questions and comments are best directed to the mailing list at
`slime-devel@common-lisp.net`, but you have to [subscribe][3] first.

See the [CONTRIBUTING.md][5] file for instructions on how to contribute.




[1]: http://common-lisp.net/project/slime/doc/html/
[2]: http://melpa.org/#/getting-started
[3]: http://www.common-lisp.net/project/slime/#mailinglist
[5]: https://github.com/slime/slime/blob/master/CONTRIBUTING.md
[6]: https://github.com/slime/slime/issues?labels=workaround&state=closed
[7]: http://common-lisp.net/project/slime/doc/html/Installation.html#Installing-from-Git
