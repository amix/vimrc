let s:suite = themis#suite('mode')
let s:assert = themis#helper('assert')

function! s:suite.mode()
  let g:lightline = {}
  call lightline#init()
  call s:assert.equals(lightline#mode(), 'NORMAL')
endfunction

function! s:suite.mode_map()
  let g:lightline = { 'mode_map': { 'n': 'N' } }
  call lightline#init()
  call s:assert.equals(lightline#mode(), 'N')
endfunction
