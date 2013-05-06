=================
Powerline for vim
=================

:Author: Kim Silkebækken (kim.silkebaekken+vim@gmail.com)
:Source: https://github.com/Lokaltog/vim-powerline
:Version: β

Introduction
------------

Powerline is a utility plugin which allows you to create better-looking, 
more functional vim statuslines. See the screenshots below for 
a demonstration of the plugin's capabilities.

It's recommended that you install the plugin using Pathogen_ or Vundle_.  
After the plugin is installed update your help tags and see ``:help 
Powerline`` for instructions on how to enable and configure the plugin.

See the `Troubleshooting`_ section below if you're having any issues with 
the plugin or the font patcher.

**Note:** You need a patched font to be able to use the symbols in the 
statusline. An experimental Python/fontforge-based font patcher is included 
in the ``fontpatcher`` directory. See ``fontpatcher/README.rst`` for usage 
instructions.

.. _Pathogen: https://github.com/tpope/vim-pathogen
.. _Vundle: https://github.com/gmarik/vundle

Screenshots
-----------

.. image:: http://i.imgur.com/MsuIB.png

Troubleshooting
---------------

I can't see the fancy symbols, what's wrong?
    Make sure that you have ``let g:Powerline_symbols = 'fancy'`` in your 
    ``vimrc`` file. The settings may be loaded too late if you have this in 
    ``gvimrc``, so always put this in your ``vimrc``.

    Clear the cache using ``:PowerlineClearCache`` and restart vim.

    Make sure that you've configured gvim or your terminal emulator to use 
    a patched font.

    Make sure that vim is compiled with the ``--with-features=big`` flag.

The fancy symbols look a bit blurry or "off"!
    Make sure that you have patched all variants of your font (i.e. both the 
    regular and the bold font files).

I'm unable to patch my font, what should I do?
    Font patching is only known to work on most Linux and OS X machines. If 
    you have followed the instructions in the fontpatcher README and still 
    have problems, please submit an issue on GitHub.

    You can download some community-contributed patched fonts from the 
    `Powerline wiki`_ if you don't want to mess around with the font 
    patcher.

The Syntastic/Fugitive statusline flags don't work!
    These flags should work without any configuration. If you installed 
    either plugin after Powerline, you'll have to clear the cache using 
    ``:PowerlineClearCache`` and restart vim.

The colors are weird in the default OS X Terminal app!
    The default OS X Terminal app is known to have some issues with the 
    Powerline colors. Please use another terminal emulator. iTerm2 should 
    work fine.

    The arrows may have the wrong colors if you have changed the "minimum 
    contrast" slider in the color tab of  your OS X settings.

The statusline has strange characters like ``^B`` in it!
    Please add ``set encoding=utf-8`` to your ``vimrc``.

    You may also need to set your ``LANG`` and ``LC_*`` environment 
    variables to a UTF-8 locale (e.g. ``LANG=en_US.utf8``). Consult your 
    Linux distro's documentation for information about setting these 
    variables correctly.

The statusline has a lot of ``^`` or underline characters in it!
    You need to configure the ``fillchars`` setting to disable statusline 
    fillchars (see ``:h fillchars`` for details). Add this to your 
    ``vimrc`` to solve this issue::

        set fillchars+=stl:\ ,stlnc:\ 

The statusline is hidden/only appears in split windows!
    Make sure that you have ``set laststatus=2`` in your ``vimrc``.

I'm using tmux and Powerline looks like crap, what's wrong?
    You need to tell tmux that it has 256-color capabilities. Add this to 
    your ``.tmux.conf`` to solve this issue::

        set -g default-terminal "screen-256color"

    If you use iTerm2, make sure that you have enabled the setting 'Set 
    locale variables automatically' in Profiles > Terminal > Environment.

If you have any other issues and you can't find the answer in the docs, 
please submit an issue on GitHub.

.. _`Powerline wiki`: https://github.com/Lokaltog/vim-powerline/wiki/Patched-fonts
