The NERDTree
=============

Introduction
------------

The NERDTree is a file system explorer for the Vim editor. Using this plugin,
users can visually browse complex directory hierarchies, quickly open files for
reading or editing, and perform basic file system operations.

This plugin can also be extended with custom mappings using a special API. The
details of this API and of other NERDTree features are described in the
included documentation.

![NERDTree Screenshot](https://github.com/scrooloose/nerdtree/raw/master/screenshot.png)

Installation
------------

#### [pathogen.vim](https://github.com/tpope/vim-pathogen)

    git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree

Then reload Vim, run `:helptags ~/.vim/bundle/nerdtree/doc/` or `:Helptags`, and check out `:help NERDTree.txt`.


#### [apt-vim](https://github.com/egalpin/apt-vim)

    apt-vim install -y https://github.com/scrooloose/nerdtree.git

F.A.Q.
------

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
