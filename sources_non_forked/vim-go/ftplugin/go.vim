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
  " these are default Vim mappings, we're overriding them to make them
  " useful again for Go source code
  nnoremap <buffer> <silent> gd :GoDef<cr>
  nnoremap <buffer> <silent> <C-]> :GoDef<cr>
  nnoremap <buffer> <silent> <C-w><C-]> :<C-u>call go#def#Jump("split")<CR>
  nnoremap <buffer> <silent> <C-w>] :<C-u>call go#def#Jump("split")<CR>
  nnoremap <buffer> <silent> <C-t> :<C-U>call go#def#StackPop(v:count1)<cr>
endif

if get(g:, "go_textobj_enabled", 1)
  onoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>
  onoremap <buffer> <silent> if :<c-u>call go#textobj#Function('i')<cr>

  xnoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>
  xnoremap <buffer> <silent> if :<c-u>call go#textobj#Function('i')<cr>

  " Remap ]] and [[ to jump betweeen functions as they are useless in Go
  nnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('n', 'next')<cr>
  nnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('n', 'prev')<cr>

  onoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('o', 'next')<cr>
  onoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('o', 'prev')<cr>

  xnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('v', 'next')<cr>
  xnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('v', 'prev')<cr>
endif

if get(g:, "go_auto_type_info", 0)
  setlocal updatetime=800
endif

" vim: sw=2 ts=2 et
