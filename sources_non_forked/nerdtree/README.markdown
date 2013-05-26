The NERD Tree
=============

Intro
-----

The NERD tree allows you to explore your filesystem and to open files and
directories. It presents the filesystem to you in the form of a tree which you
manipulate with the keyboard and/or mouse. It also allows you to perform
simple filesystem operations.

The following features and functionality are provided by the NERD tree:

  * Files and directories are displayed in a hierarchical tree structure
  * Different highlighting is provided for the following types of nodes:
    * files
    * directories
    * sym-links
    * windows .lnk files
    * read-only files
    * executable files
  * Many (customisable) mappings are provided to manipulate the tree:
    * Mappings to open/close/explore directory nodes
    * Mappings to open files in new/existing windows/tabs
    * Mappings to change the current root of the tree
    * Mappings to navigate around the tree
    * ...
  * Directories and files can be bookmarked.
  * Most NERD tree navigation can also be done with the mouse
  * Filtering of tree content (can be toggled at runtime)
    * custom file filters to prevent e.g. vim backup files being displayed
    * optional displaying of hidden files (. files)
    * files can be "turned off" so that only directories are displayed
  * The position and size of the NERD tree window can be customised
  * The order in which the nodes in the tree are listed can be customised.
  * A model of your filesystem is created/maintained as you explore it. This
    has several advantages:
    * All filesystem information is cached and is only re-read on demand
    * If you revisit a part of the tree that you left earlier in your
      session, the directory nodes will be opened/closed as you left them
  * The script remembers the cursor position and window position in the NERD
    tree so you can toggle it off (or just close the tree window) and then
    reopen it (with NERDTreeToggle) the NERD tree window will appear exactly
    as you left it
  * You can have a separate NERD tree for each tab, share trees across tabs,
    or a mix of both.
  * By default the script overrides the default file browser (netrw), so if
    you :edit a directory a (slightly modified) NERD tree will appear in the
    current window
  * A programmable menu system is provided (simulates right clicking on a node)
    * one default menu plugin is provided to perform basic filesystem
      operations (create/delete/move/copy files/directories)
  * There's an API for adding your own keymappings

Installation
------------

[pathogen.vim](https://github.com/tpope/vim-pathogen) is the recommended way to install nerdtree.

    cd ~/.vim/bundle
    git clone https://github.com/scrooloose/nerdtree.git

Then reload vim, run `:helptags`, and check out `:help NERD_tree.txt`.


Faq
---

__Q. Can I have the nerdtree on every tab automatically?__

A. Nope. If this is something you want then chances are you aren't using tabs
   and buffers as they were intended to be used. Read this
   http://stackoverflow.com/questions/102384/using-vims-tabs-like-buffers

   If you are interested in this behaviour then consider [vim-nerdtree-tabs](https://github.com/jistr/vim-nerdtree-tabs)

__Q. How can I open a NERDTree automatically when vim starts up?__

A. Stick this in your vimrc: `autocmd vimenter * NERDTree`

__Q. How can I open a NERDTree automatically when vim starts up if no files were specified?__

A. Stick this in your vimrc `autocmd vimenter * if !argc() | NERDTree | endif`

__Q. How can I map a specific key or shortcut to open NERDTree?__

A. Stick this in your vimrc to open NERDTree with `Ctrl+n` (you can set whatever key you want): `map <C-n> :NERDTreeToggle<CR>`

__Q. How can I close vim if the only window left open is a NERDTree?__

A. Stick this in your vimrc:

   `autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif`


Changelog
---------

4.2.0 (2011-12-28)

 * Add NERDTreeDirArrows option to make the UI use pretty arrow chars instead of the old +~| chars to define the tree structure (sickill)
 * shift the syntax highlighting out into its own syntax file (gnap) * add some mac specific options to the filesystem menu - for macvim only (andersonfreitas)
 * Add NERDTreeMinimalUI option to remove some non functional parts of the nerdtree ui (camthompson)
 * tweak the behaviour of :NERDTreeFind - see :help :NERDTreeFind for the new behaviour (benjamingeiger)
 * if no name is given to :Bookmark, make it default to the name of the target file/dir (minyoung)
 * use 'file' completion when doing copying, create, and move operations (EvanDotPro)
 * lots of misc bug fixes (paddyoloughlin, sdewald, camthompson, Vitaly Bogdanov, AndrewRadev, mathias, scottstvnsn, kml, wycats, me RAWR!)

