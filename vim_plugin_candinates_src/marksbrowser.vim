" marksbrowser.vim
" Author: Viktor Kojouharov
" Version: 0.9
" License: BSD
"
" Description:
" This script provides a graphical browsers of the user marks for the local
" file [a-z]
"
" Help:
" To open the browser, use the :MarksBrowser command
" The select the a mark to jump to, use <CR> or <2-LeftMouse>
" To delete a mark, press d
"
" To have the browser window not close itself after you jump to a mark, set the
" marksCloseWhenSelected in your ~/.vimrc file
" 	Example: let marksCloseWhenSelected = 0
"
" Installation:
" Put this file into your $HOME/.vim/plugin directory.

if exists("loaded_marksbrowser")
"  finish
endif
let loaded_marksbrowser = 1

if !exists("marksCloseWhenSelected")
  let s:marksCloseWhenSelected = 1
else
  let s:marksCloseWhenSelected = marksCloseWhenSelected
endif

let s:win_title = '[Marks]'
let s:all_marks = "abcdefghijklmnopqrstuvwxyz.'`^<>\""
"let s:all_marks = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.'`^<>\""
let s:isShown = 0
let s:originalBuff = 1
let s:bufNo = -1
let s:pos = []

com! -nargs=0 MarksBrowser :call <sid>ToggleMarksBrowser()

fun! s:ToggleMarksBrowser()
  let winNo = bufwinnr(s:bufNo)

  if winNo != winnr()
    let s:originalBuff = bufnr("%")
  endif

  call s:ShowMarksWin(winNo)
endf

fun! s:ShowMarksWin(winNo)
  if winnr() != a:winNo
    let lines = s:FetchMarks()
    let lnum = line('.')
    if a:winNo != -1
      call s:switchTo(a:winNo)
    else
      exec "to sp" . escape(s:win_title, ' ')
      let s:bufNo = bufnr('%')
    endif
    call s:setupBindings()
    let s:isShown = 1
    call s:Fill(lines, lnum)
  else
    close
  endif
endf

fun! s:switchTo(winNo)
  exec a:winNo . "wincmd w"
endf

fun! s:Fill(lines, lnum)
  setlocal modifiable
  1,$d _
  let blnum = 0
  let glnum = 0
  let didSeparate = 0
  put =s:Header()
  for item in a:lines
    if didSeparate == 0 && item[2] !~# '[A-Za-z]'
      let didSeparate = 1
      put ='----------------------------------- Special ------------------------------------'
    endif
    put =item[0]
    if item[1] == a:lnum && item[2] =~# '[A-Za-z]'
      let blnum = line('.')
    endif
    if len(s:pos) == 4 && item[1] == s:pos[1]
      let glnum = line('.')
    endif
  endfor
  1d _

  if !blnum
    let blnum = glnum ? glnum : 3
  endif
  call cursor(blnum - 1, 0)

  call s:setupSyntax()
  setlocal nomodifiable
  setlocal nobuflisted
  setlocal nonumber
  setlocal noswapfile
  setlocal buftype=nofile
  setlocal bufhidden=delete
  setlocal noshowcmd
  setlocal nowrap
endf

fun! s:Header()
  return "Mark\tLine\tText"
endf

fun! s:FetchMarks()
  let maxmarks = strlen(s:all_marks)
  let n = 0
  let res = []
  while n < maxmarks
    let c = strpart(s:all_marks, n, 1)
    let lnum = line("'" . c)
    if lnum != 0
      let line = getline(lnum)
      let string = "'" . c . "\t" . lnum . "\t" . line
      call add(res, [string, lnum, c])
    endif
    let n += 1
  endwhile
  return res
endf

fun! s:setupSyntax()
  syn clear
  setlocal ft=marksbuffer

  syn keyword 	MarkHeader 	Mark Line Text
  syn match 	MarkText 	"\%(\d\+\t\)\@<=.\+$"
  syn match 	MarkLine 	"\%(^'.\t\)\@<=\d\+"
  syn match 	MarkMark 	"^'."
  syn match 	MarkSeparator 	"^-\+ Special -\+$"

  hi def link MarkHeader 	Statement
  hi def link MarkMark 		Type
  hi def link MarkLine 		Number
  hi def link MarkText 		Comment
  hi def link MarkSeparator	Special
endf

fun! s:goToMark()
  let line = getline('.')
  if line !~ "^'.\t"
    return
  endif

  let pos = []
  let s:pos = []
  if s:marksCloseWhenSelected
    close
  endif
  call s:switchTo(bufwinnr(s:originalBuff))
  let mark = matchstr(line, "^'.")
  let pos = getpos(mark)
  if len(pos)
    let s:pos = pos
    call setpos('.', pos)
  endif
endf

fun! s:deleteCurrent()
  let line = getline('.')
  if line !~ "^'.\t"
    return
  endif

  let mark = strpart(line, 1, 1)
  if mark == "'" || mark == '`'
    call cursor(line('.') + 1, 0)
    return
  endif
  call s:switchTo(bufwinnr(s:originalBuff))
  if mark =~ '"'
    let mark = '\' . mark
  endif
  exec "delmarks " . mark
  call s:switchTo(bufwinnr(s:bufNo))

  setlocal modifiable
  d _
  setlocal nomodifiable
endf

fun! s:setupBindings()
  noremap <buffer> <silent> <CR> :call <sid>goToMark()<CR>
  noremap <buffer> <silent> <2-LeftMouse> :call <sid>goToMark()<CR>
  noremap <buffer> <silent> d :call <sid>deleteCurrent()<CR>
  noremap <buffer> <silent> q :call <sid>ToggleMarksBrowser()<CR>
endf
