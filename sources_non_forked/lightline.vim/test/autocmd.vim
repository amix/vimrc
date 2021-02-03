if !has("patch-8.2.0996")
  finish
endif

let s:suite = themis#suite('autocmd')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.doautoall()
  tabnew
  tabnew
  tabprevious
  doautoall WinEnter
  let statusline = getwinvar(1, '&statusline')
  call s:assert.match(statusline, 'lightline')
  call s:assert.match(statusline, '_active_')
endfunction
