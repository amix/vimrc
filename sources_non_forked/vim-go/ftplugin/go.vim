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
  nnoremap <buffer> <silent> <C-LeftMouse> <LeftMouse>:GoDef<cr>
  nnoremap <buffer> <silent> g<LeftMouse> <LeftMouse>:GoDef<cr>
  nnoremap <buffer> <silent> <C-w><C-]> :<C-u>call go#def#Jump("split")<CR>
  nnoremap <buffer> <silent> <C-w>] :<C-u>call go#def#Jump("split")<CR>
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

" NOTE(arslan): experimental, disabled by default, doesn't work well. No
" documentation as well. If anyone feels adventurous, enable the following and
" try to search for Go identifiers ;)
"
" if get(g:, "go_sameid_search_enabled", 0)
"   autocmd FileType go nnoremap <buffer> <silent> * :<c-u>call Sameids_search(0)<CR>
"   autocmd FileType go nnoremap <buffer> <silent> # :<c-u>call Sameids_search(1)<CR>
"   autocmd FileType go nnoremap <buffer> <silent> n :<c-u>call Sameids_repeat(0)<CR>
"   autocmd FileType go nnoremap <buffer> <silent> N :<c-u>call Sameids_repeat(1)<CR>
"   autocmd FileType go cabbrev nohlsearch <C-r>=Sameids_nohlsearch()<CR>
" endif

" " mode 0: next 1: prev
" function! Sameids_repeat(mode)
"   let matches = getmatches()
"   if empty(matches)
"     return
"   endif
"   let cur_offset = go#util#OffsetCursor()

"   " reverse list to make it easy to find the prev occurrence
"   if a:mode
"    call reverse(matches)
"   endif

"   for m in matches
"     if !has_key(m, "group")
"       return
"     endif

"     if m.group != "goSameId"
"       return
"     endif

"     let offset = go#util#Offset(m.pos1[0], m.pos1[1])

"     if a:mode && cur_offset > offset
"       call cursor(m.pos1[0], m.pos1[1])
"       return
"     elseif !a:mode && cur_offset < offset
"       call cursor(m.pos1[0], m.pos1[1])
"       return
"     endif
"   endfor

"   " reached start/end, jump to the end/start
"   let initial_match = matches[0]
"   if !has_key(initial_match, "group")
"     return
"   endif

"   if initial_match.group != "goSameId"
"     return
"   endif

"   call cursor(initial_match.pos1[0], initial_match.pos1[1])
" endfunction

" function! Sameids_search(mode)
"   call go#guru#SameIds()
"   call Sameids_repeat(a:mode)
" endfunction

" function! Sameids_nohlsearch()
"   call go#guru#ClearSameIds()
"   return "nohlsearch"
" endfunction

" vim: sw=2 ts=2 et
