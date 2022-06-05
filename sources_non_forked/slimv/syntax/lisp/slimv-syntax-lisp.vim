" slimv-syntax-lisp.vim:
"               Lisp syntax plugin for Slimv
" Version:      0.9.11
" Last Change:  22 Apr 2013
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:current_syntax") || exists("g:slimv_disable_lisp")
  finish
endif

runtime syntax/**/lisp.vim

" Change syntax for #\( and #\) to string so that paren matching ignores them
syn match lispString !#\\[\(\)]!

