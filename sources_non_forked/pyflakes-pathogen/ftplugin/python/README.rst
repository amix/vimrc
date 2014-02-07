pyflakes-vim
============

A Vim plugin for checking Python code on the fly.

PyFlakes catches common Python errors like mistyping a variable name or
accessing a local before it is bound, and also gives warnings for things like
unused imports.

pyflakes-vim uses the output from PyFlakes to highlight errors in your code.
To locate errors quickly, use quickfix_ commands like :cc.

Make sure to check vim.org_ for the latest updates.

.. _pyflakes.vim: http://www.vim.org/scripts/script.php?script_id=2441
.. _vim.org: http://www.vim.org/scripts/script.php?script_id=2441
.. _quickfix: http://vimdoc.sourceforge.net/htmldoc/quickfix.html#quickfix

Quick Installation
------------------

1. Make sure your ``.vimrc`` has::
 
    filetype on            " enables filetype detection
    filetype plugin on     " enables filetype specific plugins

2. Download the latest release_.

3. Unzip ``pyflakes.vim`` and the ``pyflakes`` directory into
   ``~/.vim/ftplugin/python`` (or somewhere similar on your
   `runtime path`_ that will be sourced for Python files).

.. _release: http://www.vim.org/scripts/script.php?script_id=2441
.. _runtime path: http://vimdoc.sourceforge.net/htmldoc/options.html#'runtimepath' 

Installation
------------

If you downloaded this from vim.org_, then just drop the contents of the zip
file into ``~/.vim/ftplugin/python``.

Otherwise, if you're running "from source," you'll need PyFlakes on your
PYTHONPATH somewhere.  I recommend getting my PyFlakes_ fork, which retains
column number information and has therfore has more specific error locations.

.. _vim.org: http://www.vim.org/scripts/script.php?script_id=2441
.. _PyFlakes: http://github.com/kevinw/pyflakes

Hacking
-------

::

  git clone git://github.com/kevinw/pyflakes-vim.git
  cd pyflakes-vim
  git clone git://github.com/kevinw/pyflakes.git

Options
-------

Set this option to you vimrc file to disable quickfix support::
    
    let g:pyflakes_use_quickfix = 0

The value is set to 1 by default.

TODO
----
 * signs_ support (show warning and error icons to left of the buffer area)
 * configuration variables
 * parse or intercept useful output from the warnings module

.. _signs: http://www.vim.org/htmldoc/sign.html

Changelog
---------

Please see http://www.vim.org/scripts/script.php?script_id=2441 for a history of
all changes.

