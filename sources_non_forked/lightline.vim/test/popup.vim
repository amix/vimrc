if !exists('*popup_menu') || !exists('*win_execute')
  finish
endif

let s:suite = themis#suite('popup')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.win_execute_setfiletype()
  let id = popup_menu(['aaa', 'bbb'], {})
  call win_execute(id, 'setfiletype vim')
  call popup_close(id)
endfunction
