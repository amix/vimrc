let s:suite = themis#suite('quickfix')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.quickfix_statusline()
  call setloclist(winnr(), [])
  lopen
  wincmd p
  call setloclist(winnr(), [])
  for n in range(1, winnr('$'))
    let statusline = getwinvar(n, '&statusline')
    call s:assert.match(statusline, 'lightline')
    if has('patch-8.1.1715')
      call s:assert.match(statusline, n == 1 ? '_active_' : '_inactive_')
    else
      call s:assert.match(statusline, n != 1 ? '_active_' : '_inactive_')
    endif
  endfor
endfunction
