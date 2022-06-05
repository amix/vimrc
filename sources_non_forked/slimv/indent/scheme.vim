" scheme.vim:
"               Scheme indent plugin for Slimv
" Version:      0.9.14
" Last Change:  26 Feb 2021
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_indent") || exists("g:slimv_disable_scheme") ||
\ (exists("g:slimv_indent_disable") && g:slimv_indent_disable)
   finish
endif

let b:did_indent = 1

setlocal nolisp
setlocal autoindent
setlocal expandtab
setlocal indentexpr=SlimvIndent(v:lnum)

