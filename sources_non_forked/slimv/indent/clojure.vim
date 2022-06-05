" clojure.vim:
"               Clojure indent plugin for Slimv
" Version:      0.9.14
" Last Change:  22 Jan 2021
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_indent") || exists("b:slimv_did_indent") || exists("g:slimv_disable_clojure") ||
\ (exists("g:slimv_indent_disable") && g:slimv_indent_disable)
   finish
endif

" Prevent recursive call but allow loading other clojure plugins
let b:slimv_did_indent = 1

runtime! indent/**/clojure.vim
runtime indent/**/lisp.vim

setlocal nolisp
setlocal autoindent
setlocal expandtab
setlocal indentexpr=SlimvIndent(v:lnum)

