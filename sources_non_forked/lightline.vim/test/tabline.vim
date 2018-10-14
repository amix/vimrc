let s:suite = themis#suite('tabline')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = {}
  call lightline#init()
  tabnew
  tabonly
endfunction

function! s:suite.tabline()
  call s:assert.equals(&tabline, '%!lightline#tabline()')
endfunction

function! s:suite.enabled()
  let g:lightline = { 'enable': { 'tabline': 1 } }
  call lightline#init()
  call s:assert.equals(&tabline, '%!lightline#tabline()')
endfunction

function! s:suite.disabled()
  let g:lightline = { 'enable': { 'tabline': 0 } }
  call lightline#init()
  call s:assert.equals(&tabline, '')
endfunction

function! s:suite.tabnew()
  let tabline = lightline#tabline()
  tabnew
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction

function! s:suite.tabnew_first()
  let tabline = lightline#tabline()
  0tabnew
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction

function! s:suite.tabnext()
  tabnew
  let tabline = lightline#tabline()
  tabnext
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction

function! s:suite.tabonly()
  tabnew
  tabfirst
  let tabline = lightline#tabline()
  tabonly
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction

function! s:suite.tabclose()
  tabnew
  let tabline = lightline#tabline()
  tabclose
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction

function! s:suite.tabclose_last()
  tabnew
  tabfirst
  let tabline = lightline#tabline()
  $tabclose
  call s:assert.not_equals(lightline#tabline(), tabline)
endfunction
