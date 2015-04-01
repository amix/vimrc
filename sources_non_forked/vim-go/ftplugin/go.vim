" Copyright 2013 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" go.vim: Vim filetype plugin for Go.

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

let b:undo_ftplugin = "setl fo< com< cms<"

setlocal formatoptions-=t

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//\ %s

setlocal noexpandtab

compiler go

" Set gocode completion
setlocal omnifunc=go#complete#Complete

if get(g:, "go_doc_keywordprg_enabled", 1)
    " keywordprg doesn't allow to use vim commands, override it
    nnoremap <buffer> <silent> K :GoDoc<cr>
endif

if get(g:, "go_def_mapping_enabled", 1)
   nnoremap <buffer> <silent> gd :GoDef<cr>
endif

if get(g:, "go_textobj_enabled", 1)
    onoremap <buffer> af :<c-u>call go#textobj#Function('a')<cr>
    xnoremap <buffer> af :<c-u>call go#textobj#Function('a')<cr>
    onoremap <buffer> if :<c-u>call go#textobj#Function('i')<cr>
    xnoremap <buffer> if :<c-u>call go#textobj#Function('i')<cr>
endif

if get(g:, "go_auto_type_info", 0)
    setlocal updatetime=800
endif

" vim:ts=4:sw=4:et
