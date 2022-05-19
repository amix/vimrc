" vint: -ProhibitSetNoCompatible

" Make most tests just set lists synchronously when run in Docker, etc.
let g:ale_set_lists_synchronously = 1

" This lowercase highlight definition is needed for highlight tests.
hi link aleerrorline spellbad

" Load builtin plugins
" We need this because run_vim.sh sets -i NONE
if has('win32')
    set runtimepath=$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,C:\vader,C:\testplugin
else
    set runtimepath=/home/vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,/testplugin,/vader
endif

" The following is just an example
filetype plugin indent on
syntax on

if !has('win32')
    set shell=/bin/sh
    set shellcmdflag=-c
endif

set nocompatible
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set backspace=2
set nofoldenable
set foldmethod=syntax
set foldlevelstart=10
set foldnestmax=10
set ttimeoutlen=0
" The encoding must be explicitly set for tests for Windows.
execute 'set encoding=utf-8'

let g:mapleader=','

let g:ale_ignore_2_4_warnings = 1
