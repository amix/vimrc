let s:suite = themis#suite('toggle')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.default()
  call s:assert.equals(exists('#lightline'), 1)
  call s:assert.equals(exists('#lightline-disable'), 0)
  call s:assert.not_equals(&tabline, '')
endfunction

function! s:suite.disable_enable()
  call lightline#disable()
  call s:assert.equals(exists('#lightline'), 0)
  call s:assert.equals(exists('#lightline-disable'), 1)
  call s:assert.equals(&tabline, '')
  call lightline#enable()
  call s:assert.equals(exists('#lightline'), 1)
  call s:assert.equals(exists('#lightline-disable'), 0)
  call s:assert.not_equals(&tabline, '')
endfunction

function! s:suite.toggle()
  call lightline#toggle()
  call s:assert.equals(exists('#lightline'), 0)
  call s:assert.equals(exists('#lightline-disable'), 1)
  call s:assert.equals(&tabline, '')
  call lightline#toggle()
  call s:assert.equals(exists('#lightline'), 1)
  call s:assert.equals(exists('#lightline-disable'), 0)
  call s:assert.not_equals(&tabline, '')
endfunction
