vim-python-pep8-indent
======================

.. image:: https://circleci.com/gh/Vimjas/vim-python-pep8-indent.svg?style=svg
  :target: https://circleci.com/gh/Vimjas/vim-python-pep8-indent
.. image:: https://codecov.io/gh/Vimjas/vim-python-pep8-indent/branch/master/graph/badge.svg
  :target: https://codecov.io/gh/Vimjas/vim-python-pep8-indent

This small script modifies Vim_’s indentation behavior to comply with PEP8_ and my aesthetic preferences.
Most importantly::

   foobar(foo,
          bar)

and::

   foobar(
      foo,
      bar
   )


Installation
------------

Install the plugin using your favorite plugin manager / method, a few examples
follow:

Pathogen
^^^^^^^^

Follow the instructions on installing Pathogen_ and then:

.. code-block:: shell-session

   $ cd ~/.vim/bundle
   $ git clone https://github.com/Vimjas/vim-python-pep8-indent.git


Vundle
^^^^^^

Follow the instructions on installing Vundle_ and add the appropriate plugin line into your ``.vimrc``:

.. code-block:: vim

   Plugin 'Vimjas/vim-python-pep8-indent'


NeoBundle
^^^^^^^^^

Follow the instructions on installing NeoBundle_ and add the appropriate NeoBundle line into your ``.vimrc``:

.. code-block:: vim

   NeoBundle 'Vimjas/vim-python-pep8-indent'


Configuration
-------------

g:python_pep8_indent_multiline_string
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can configure the initial indentation of multiline strings using ``g:python_pep8_indent_multiline_string`` (which can also be set per buffer).
This defaults to ``0``, which means that multiline strings are not indented.
``-1`` and positive values will be used as-is, where ``-1`` is a special value for Vim's ``indentexpr``, and will keep the existing indent (using Vim's ``autoindent`` setting).
``-2`` is meant to be used for strings that are wrapped with ``textwrap.dedent`` etc.  It will add a level of indentation if the multiline string started in the previous line, without any content in it already::

   testdir.makeconftest("""
       _

With content already, it will be aligned to the opening parenthesis::

   testdir.makeconftest("""def pytest_addoption(parser):
                        _

Existing indentation (including ``0``) in multiline strings will be kept, so this setting only applies to the indentation of new/empty lines.

g:python_pep8_indent_hang_closing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Control closing bracket indentation with ``python_pep8_indent_hang_closing``, set globally or per buffer.

By default (set to ``0``), closing brackets line up with the opening line::

   my_list = [
       1, 2, 3,
       4, 5, 6,
   ]
   result = some_function_that_takes_arguments(
       'a', 'b', 'c',
       'd', 'e', 'f',
   )

With ``python_pep8_indent_hang_closing = 1``, closing brackets line up with the items::

   my_list = [
       1, 2, 3,
       4, 5, 6,
       ]
   result = some_function_that_takes_arguments(
       'a', 'b', 'c',
       'd', 'e', 'f',
       )


Troubleshooting
---------------

In case it is not working, please make sure your Vim is configured to load
indent files (``filetype indent on``).
This is typically the case when using a plugin manager, but check its docs.

Check ``:verbose set indentexpr?`` in a Python file, which should show
something like the following:

  indentexpr=GetPythonPEPIndent(v:lnum)
        Last set from ~/…/plugged/vim-python-pep8-indent/indent/python.vim


Notes
-----

Please note that Kirill Klenov’s python-mode_ ships its own version of this bundle.
Therefore, if you want to use this version specifically, you’ll have to disable python-mode’s using:

.. code-block:: vim

   let g:pymode_indent = 0


License and Authorship
----------------------

This script is based on one from Vim’s official `script repo`_  that was *not* originally written by me.
Unfortunately the indentation was off by one character in one case and the script hasn’t been updated since 2005.

Even more unfortunately, I wasn’t able to reach any of the original authors/maintainers:
**David Bustos** and **Eric Mc Sween**.

So I fixed the annoyance with the help of `Steve Losh`_ and am putting it out here so you don’t have to patch the original yourself.
The original patch is still available here_.

Over the time a lot more improvements have been contributed_ by `generous people`_.

I’d like to thank the original authors here for their work and release it hereby to the *Public Domain* (using the CC0_ licence) since I hope that would be in their spirit.
If anyone with a say in this objects, please let me_ know immediately.
Also, if someone is in contact with one of them, I would appreciate being introduced.

While my Vimscript_ skills are still feeble, I intend to maintain it for now.
This mainly means that I’ll triage through bugs and pull requests but won’t be fixing much myself.


.. _Vim: http://www.vim.org/
.. _PEP8: http://www.python.org/dev/peps/pep-0008/
.. _`script repo`: http://www.vim.org/scripts/script.php?script_id=974
.. _`Steve Losh`: http://stevelosh.com/
.. _here: https://gist.github.com/2965846
.. _Neobundle: https://github.com/Shougo/neobundle.vim
.. _Pathogen: https://github.com/tpope/vim-pathogen
.. _python-mode: https://github.com/klen/python-mode
.. _`Vimscript`: http://learnvimscriptthehardway.stevelosh.com/
.. _vundle: https://github.com/gmarik/Vundle.vim
.. _me: https://hynek.me/
.. _CC0: http://creativecommons.org/publicdomain/zero/1.0/
.. _contributed: https://github.com/hynek/vim-python-pep8-indent/blob/master/CONTRIBUTING.rst
.. _`generous people`: https://github.com/hynek/vim-python-pep8-indent/graphs/contributors
