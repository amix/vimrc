" Copyright 2013 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" go.vim: Vim filetype plugin for Go.

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let b:undo_ftplugin = "setl fo< com< cms<
      \ | exe 'au! vim-go-buffer * <buffer>'"

setlocal formatoptions-=t

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//\ %s

setlocal noexpandtab

compiler go

" Set autocompletion
setlocal omnifunc=go#complete#Complete
if !go#util#has_job()
  setlocal omnifunc=go#complete#GocodeComplete
endif

if get(g:, "go_doc_keywordprg_enabled", 1)
  " keywordprg doesn't allow to use vim commands, override it
  nnoremap <buffer> <silent> K :GoDoc<cr>
endif

if get(g:, "go_def_mapping_enabled", 1)
  " these are default Vim mappings, we're overriding them to make them
  " useful again for Go source code
  nnoremap <buffer> <silent> gd :GoDef<cr>
  nnoremap <buffer> <silent> <C-]> :GoDef<cr>
  nnoremap <buffer> <silent> <C-LeftMouse> <LeftMouse>:GoDef<cr>
  nnoremap <buffer> <silent> g<LeftMouse> <LeftMouse>:GoDef<cr>
  nnoremap <buffer> <silent> <C-w><C-]> :<C-u>call go#def#Jump("split", 0)<CR>
  nnoremap <buffer> <silent> <C-w>] :<C-u>call go#def#Jump("split", 0)<CR>
  nnoremap <buffer> <silent> <C-t> :<C-U>call go#def#StackPop(v:count1)<cr>
endif

if get(g:, "go_textobj_enabled", 1)
  onoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>
  xnoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>

  onoremap <buffer> <silent> if :<c-u>call go#textobj#Function('i')<cr>
  xnoremap <buffer> <silent> if :<c-u>call go#textobj#Function('i')<cr>

  onoremap <buffer> <silent> ac :<c-u>call go#textobj#Comment('a')<cr>
  xnoremap <buffer> <silent> ac :<c-u>call go#textobj#Comment('a')<cr>

  onoremap <buffer> <silent> ic :<c-u>call go#textobj#Comment('i')<cr>
  xnoremap <buffer> <silent> ic :<c-u>call go#textobj#Comment('i')<cr>

  " Remap ]] and [[ to jump betweeen functions as they are useless in Go
  nnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('n', 'next')<cr>
  nnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('n', 'prev')<cr>

  onoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('o', 'next')<cr>
  onoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('o', 'prev')<cr>

  xnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('v', 'next')<cr>
  xnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('v', 'prev')<cr>
endif

if go#config#AutoTypeInfo() || go#config#AutoSameids()
  let &l:updatetime= get(g:, "go_updatetime", 800)
endif

" Autocommands
" ============================================================================
"
augroup vim-go-buffer
  autocmd! * <buffer>

  " TODO(bc): notify gopls about changes on CursorHold when the buffer is
  " modified.
  " TODO(bc): notify gopls that the file on disk is correct on BufWritePost

  autocmd CursorHold <buffer> call go#auto#auto_type_info()
  autocmd CursorHold <buffer> call go#auto#auto_sameids()

  " Echo the identifier information when completion is done. Useful to see
  " the signature of a function, etc...
  if exists('##CompleteDone')
    autocmd CompleteDone <buffer> call go#auto#echo_go_info()
  endif

  autocmd BufWritePre <buffer> call go#auto#fmt_autosave()
  autocmd BufWritePost <buffer> call go#auto#metalinter_autosave()

  " clear SameIds when the buffer is unloaded so that loading another buffer
  " in the same window doesn't highlight the most recently matched
  " identifier's positions.
  autocmd BufWinEnter <buffer> call go#guru#ClearSameIds()

  autocmd BufEnter <buffer>
        \  if go#config#AutodetectGopath() && !exists('b:old_gopath')
        \|   let b:old_gopath = exists('$GOPATH') ? $GOPATH : -1
        \|   let $GOPATH = go#path#Detect()
        \| endif
  autocmd BufLeave <buffer>
        \  if exists('b:old_gopath')
        \|   if b:old_gopath isnot -1
        \|     let $GOPATH = b:old_gopath
        \|   endif
        \|   unlet b:old_gopath
        \| endif
augroup end

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
