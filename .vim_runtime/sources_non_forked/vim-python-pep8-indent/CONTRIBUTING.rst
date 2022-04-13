How To Contribute
=================

``vim-python-pep8-indent`` is always open for suggestions and contributions by generous developers.
I’ve collected a few tips to get you started.

Please:

- *Always* add tests for your code.
- Write `good commit messages`_.


Running Tests
-------------

- They are written in Ruby_ (sorry :() using vimrunner_ which requires rspec_.
- The tests go into ``spec/indent/indent_spec.rb``.
  Look at the ``describe`` blocks to get the hang of it.
- Run the tests with the command::

   $ rspec spec
- Alternatively you can use Docker::

   $ make test_docker

- You can select tests based on line numbers, e.g.::

   $ rspec ./spec/indent/indent_spec.rb:385
   $ make test_docker RSPEC_ARGS=./spec/indent/indent_spec.rb:385

Thank you for considering to contribute!


.. _Ruby: https://www.ruby-lang.org/
.. _`good commit messages`: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
.. _vimrunner: https://github.com/AndrewRadev/vimrunner
.. _rspec: https://github.com/rspec/rspec
