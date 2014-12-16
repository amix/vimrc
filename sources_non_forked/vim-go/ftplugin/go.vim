" Copyright 2013 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" go.vim: Vim filetype plugin for Go.

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

if !exists("g:go_auto_type_info")
    let g:go_auto_type_info = 0
endif

setlocal formatoptions-=t

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//\ %s

setlocal noexpandtab

compiler go

if !exists("g:go_doc_keywordprg_enabled")
    let g:go_doc_keywordprg_enabled = 1
endif
if g:go_doc_keywordprg_enabled
    " keywordprg doesn't allow to use vim commands, override it
    nnoremap <buffer> <silent> K :GoDoc<cr> 
endif


if !exists("g:go_def_mapping_enabled")
    let g:go_def_mapping_enabled = 1
endif
if g:go_def_mapping_enabled
   nnoremap <buffer> <silent> gd :GoDef<cr>
endif


let b:undo_ftplugin = "setl fo< com< cms<"

" Set gocode completion
setlocal omnifunc=go#complete#Complete

" GoInfo automatic update
if g:go_auto_type_info != 0
    setlocal updatetime=300
    au! CursorHold *.go nested call go#complete#Info()
endif


" autoload settings
if !exists('g:go_fmt_autosave')
    let g:go_fmt_autosave = 1
endif

if g:go_fmt_autosave
    autocmd BufWritePre <buffer> call go#fmt#Format(-1)
endif

" vim:ts=4:sw=4:et
