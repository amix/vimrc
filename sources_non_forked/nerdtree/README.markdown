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

Below are just some of the methods for installing NERDTree. Do not follow all of these instructions; just pick your favorite one. Other plugin managers exist, and NERDTree should install just fine with any of them.

#### Vim 8+ packages

If you are using VIM version 8 or higher you can use its built-in package management; see `:help packages` for more information. Just run these commands in your terminal:

```bash
git clone https://github.com/scrooloose/nerdtree.git ~/.vim/pack/vendor/start/nerdtree
vim -u NONE -c "helptags ~/.vim/pack/vendor/start/nerdtree/doc" -c q
```

Otherwise, these are some of the several 3rd-party plugin managers you can choose from. Be sure you read the instructions for your chosen plugin, as there typically are additional steps you nee d to take.

#### [pathogen.vim](https://github.com/tpope/vim-pathogen)

In the terminal,
```bash
git clone https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree
```
In your vimrc,
```vim
call pathogen#infect()
syntax on
filetype plugin indent on
```

Then reload vim, run `:helptags ~/.vim/bundle/nerdtree/doc/` or `:Helptags`.

#### [Vundle.vim](https://github.com/VundleVim/Vundle.vim)
```vim
call vundle#begin()
Plugin 'scrooloose/nerdtree'
call vundle#end()
```

#### [vim-plug](https://github.com/junegunn/vim-plug)
```vim
call plug#begin()
Plug 'scrooloose/nerdtree'
call plug#end()
```

#### [apt-vim](https://github.com/egalpin/apt-vim)
```bash
apt-vim install -y https://github.com/scrooloose/nerdtree.git
```

F.A.Q. (here, and in the [Wiki](https://github.com/scrooloose/nerdtree/wiki))
------

#### Is there any support for `git` flags?

Yes, install [nerdtree-git-plugin](https://github.com/Xuyuanp/nerdtree-git-plugin).

---
#### Can I have the nerdtree on every tab automatically?

Nope. If this is something you want then chances are you aren't using tabs and
buffers as they were intended to be used. Read this
http://stackoverflow.com/questions/102384/using-vims-tabs-like-buffers

If you are interested in this behaviour then consider [vim-nerdtree-tabs](https://github.com/jistr/vim-nerdtree-tabs)

---
#### How can I open a NERDTree automatically when vim starts up?

Stick this in your vimrc: `autocmd vimenter * NERDTree`

---
#### How can I open a NERDTree automatically when vim starts up if no files were specified?

Stick this in your vimrc:
```vim
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
```

Note: Now start vim with plain `vim`, not `vim .`

---
#### What if I'm also opening a saved session, for example `vim -S session_file.vim`? I don't want NERDTree to open in that scenario.
```vim
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") && v:this_session == "" | NERDTree | endif
```

---
#### How can I open NERDTree automatically when vim starts up on opening a directory?
```vim
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
```

This window is tab-specific, meaning it's used by all windows in the tab. This trick also prevents NERDTree from hiding when first selecting a file.

Note: Executing `vim ~/some-directory` will open NERDTree and a new edit window. `exe 'cd '.argv()[0]` sets the `pwd` of the new edit window to `~/some-directory`

---
#### How can I map a specific key or shortcut to open NERDTree?

Stick this in your vimrc to open NERDTree with `Ctrl+n` (you can set whatever key you want):
```vim
map <C-n> :NERDTreeToggle<CR>
```

---
#### How can I close vim if the only window left open is a NERDTree?

Stick this in your vimrc:
```vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
```

---
#### Can I have different highlighting for different file extensions?

See here: https://github.com/scrooloose/nerdtree/issues/433#issuecomment-92590696

---
#### How can I change default arrows?

Use these variables in your vimrc. Note that below are default arrow symbols
```vim
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
```
