pyflakes
========

This version of PyFlakes_ has been improved to use Python's newer ``ast``
module, instead of ``compiler``. So code checking happens faster, and will stay
up to date with new language changes.

.. _PyFlakes: http://http://www.divmod.org/trac/wiki/DivmodPyflakes

TODO
----

Importing several modules from the same package results in unnecessary warnings:

::

    import a.b
    import a.c # Redefinition of unused "a" from line 1

The following construct for defining a function differently depending on some
condition results in a redefinition warning:

::

    if some_condition:
        def foo(): do_foo()
    else:
        def foo(): do_bar() # redefinition of function 'foo' from line 2

IDE Integration
---------------

* vim: pyflakes-vim_

.. _pyflakes-vim: http://github.com/kevinw/pyflakes-vim

