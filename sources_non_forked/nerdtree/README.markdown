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

#### [pathogen.vim](https://github.com/tpope/vim-pathogen)

    git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree

Then reload vim, run `:helptags ~/.vim/bundle/nerdtree/doc/`, and check out `:help NERD_tree.txt`.


#### [apt-vim](https://github.com/egalpin/apt-vim)

    apt-vim install -y https://github.com/scrooloose/nerdtree.git



Faq
---

> Is there any support for `git` flags?

Yes, install [nerdtree-git-plugin](https://github.com/Xuyuanp/nerdtree-git-plugin).

---

> Can I have the nerdtree on every tab automatically?

Nope. If this is something you want then chances are you aren't using tabs and
buffers as they were intended to be used. Read this
http://stackoverflow.com/questions/102384/using-vims-tabs-like-buffers

If you are interested in this behaviour then consider [vim-nerdtree-tabs](https://github.com/jistr/vim-nerdtree-tabs)

---
> How can I open a NERDTree automatically when vim starts up?

Stick this in your vimrc: `autocmd vimenter * NERDTree`

---
> How can I open a NERDTree automatically when vim starts up if no files were specified?

Stick this in your vimrc:

    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

Note: Now start vim with plain `vim`, not `vim .`

---
> How can I open NERDTree automatically when vim starts up on opening a directory?

    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

This window is tab-specific, meaning it's used by all windows in the tab. This trick also prevents NERDTree from hiding when first selecting a file.

---
> How can I map a specific key or shortcut to open NERDTree?

Stick this in your vimrc to open NERDTree with `Ctrl+n` (you can set whatever key you want):

    map <C-n> :NERDTreeToggle<CR>

---
> How can I close vim if the only window left open is a NERDTree?

Stick this in your vimrc:

    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

---
> Can I have different highlighting for different file extensions?

See here: https://github.com/scrooloose/nerdtree/issues/433#issuecomment-92590696

---
> How can I change default arrows?

Use these variables in your vimrc. Note that below are default arrow symbols

    let g:NERDTreeDirArrowExpandable = '▸'
    let g:NERDTreeDirArrowCollapsible = '▾'
