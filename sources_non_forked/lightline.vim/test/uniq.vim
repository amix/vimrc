let s:suite = themis#suite('uniq')
let s:assert = themis#helper('assert')

function! s:uniq(...)
  try
    return call(SID('uniq'), a:000)
  catch
    return call(function('uniq'), a:000)
  endtry
endfunction

function! s:suite.nil()
  call s:assert.equals(s:uniq([]), [])
endfunction

function! s:suite.one()
  call s:assert.equals(s:uniq(['foo']), ['foo'])
endfunction

function! s:suite.two()
  call s:assert.equals(s:uniq(['foo', 'bar']), ['foo', 'bar'])
endfunction

function! s:suite.three()
  call s:assert.equals(s:uniq(['foo', 'bar', 'baz']), ['foo', 'bar', 'baz'])
endfunction

function! s:suite.two_duplicated()
  call s:assert.equals(s:uniq(['foo', 'foo']), ['foo'])
endfunction

function! s:suite.three_duplicated()
  call s:assert.equals(s:uniq(['foo', 'bar', 'foo']), ['foo', 'bar', 'foo'])
endfunction

function! s:suite.many1()
  call s:assert.equals(s:uniq(['foo', 'foo', 'bar', 'baz', 'baz', 'qux', 'foo']), ['foo', 'bar', 'baz', 'qux', 'foo'])
endfunction

function! s:suite.many2()
  call s:assert.equals(s:uniq(['foo', 'foo', 'foo', 'foo', 'bar', 'bar', 'bar']), ['foo', 'bar'])
endfunction

function! s:suite.many3()
  call s:assert.equals(s:uniq(['foo', 'foo', 'bar', 'bar', 'bar', 'foo', 'foo', 'foo']), ['foo', 'bar', 'foo'])
endfunction
