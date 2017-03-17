let s:suite = themis#suite('concatenate')
let s:assert = themis#helper('assert')

function! s:suite.before_each()
  let g:lightline = { 'subseparator': { 'left': '>', 'right': '<' } }
  call lightline#init()
endfunction

function! s:suite.nil()
  call s:assert.equals(lightline#concatenate([], 0), '')
  call s:assert.equals(lightline#concatenate([], 1), '')
endfunction

function! s:suite.one()
  call s:assert.equals(lightline#concatenate(['foo'], 0), 'foo')
  call s:assert.equals(lightline#concatenate(['foo'], 1), 'foo')
endfunction

function! s:suite.two()
  call s:assert.equals(lightline#concatenate(['foo', 'bar'], 0), 'foo > bar')
  call s:assert.equals(lightline#concatenate(['foo', 'bar'], 1), 'foo < bar')
endfunction

function! s:suite.three()
  call s:assert.equals(lightline#concatenate(['foo', 'bar', 'baz'], 0), 'foo > bar > baz')
  call s:assert.equals(lightline#concatenate(['foo', 'bar', 'baz'], 1), 'foo < bar < baz')
endfunction

function! s:suite.one_empty()
  call s:assert.equals(lightline#concatenate([''], 0), '')
  call s:assert.equals(lightline#concatenate([''], 1), '')
endfunction

function! s:suite.two_empty_left()
  call s:assert.equals(lightline#concatenate(['', 'bar'], 0), 'bar')
  call s:assert.equals(lightline#concatenate(['', 'bar'], 1), 'bar')
endfunction

function! s:suite.two_empty_right()
  call s:assert.equals(lightline#concatenate(['foo', ''], 0), 'foo')
  call s:assert.equals(lightline#concatenate(['foo', ''], 1), 'foo')
endfunction

function! s:suite.two_empty_both()
  call s:assert.equals(lightline#concatenate(['', ''], 0), '')
  call s:assert.equals(lightline#concatenate(['', ''], 1), '')
endfunction

function! s:suite.three_empty_left()
  call s:assert.equals(lightline#concatenate(['', 'bar', 'baz'], 0), 'bar > baz')
  call s:assert.equals(lightline#concatenate(['', 'bar', 'baz'], 1), 'bar < baz')
endfunction

function! s:suite.three_empty_middle()
  call s:assert.equals(lightline#concatenate(['foo', '', 'baz'], 0), 'foo > baz')
  call s:assert.equals(lightline#concatenate(['foo', '', 'baz'], 1), 'foo < baz')
endfunction

function! s:suite.three_empty_right()
  call s:assert.equals(lightline#concatenate(['foo', 'bar', ''], 0), 'foo > bar')
  call s:assert.equals(lightline#concatenate(['foo', 'bar', ''], 1), 'foo < bar')
endfunction

function! s:suite.three_empty_middle_right()
  call s:assert.equals(lightline#concatenate(['foo', '', ''], 0), 'foo')
  call s:assert.equals(lightline#concatenate(['foo', '', ''], 1), 'foo')
endfunction

function! s:suite.three_empty_left_right()
  call s:assert.equals(lightline#concatenate(['', 'bar', ''], 0), 'bar')
  call s:assert.equals(lightline#concatenate(['', 'bar', ''], 1), 'bar')
endfunction

function! s:suite.three_empty_left_middle()
  call s:assert.equals(lightline#concatenate(['', '', 'baz'], 0), 'baz')
  call s:assert.equals(lightline#concatenate(['', '', 'baz'], 1), 'baz')
endfunction

function! s:suite.three_empty_all()
  call s:assert.equals(lightline#concatenate(['', '', ''], 0), '')
  call s:assert.equals(lightline#concatenate(['', '', ''], 1), '')
endfunction

function! s:suite.keep_original()
  let xs = ['', 'bar', '']
  call s:assert.equals(lightline#concatenate(xs, 0), 'bar')
  call s:assert.equals(xs, ['', 'bar', ''])
  call s:assert.equals(lightline#concatenate(xs, 1), 'bar')
  call s:assert.equals(xs, ['', 'bar', ''])
endfunction
