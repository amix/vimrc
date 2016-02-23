let s:t = themis#suite('valid filetype for using :PrevimOpen')
let s:assert = themis#helper('assert')

function! s:t.before()
  let self._ft = &filetype
endfunction

function! s:t.after()
  let &filetype = self._ft
  call s:_clean_command()
endfunction

function! s:_clean_command()
  if exists(':PrevimOpen') == 2
    delcommand PrevimOpen
  endif
endfunction

" helper
function! s:_assert_filetype(ft, expected)
  let &filetype = a:ft
  let actual = exists(':PrevimOpen')
  if actual !=# a:expected
    call s:assert.fail(printf("'%s': expected %d but actual %d", a:ft, a:expected, actual))
  endif
endfunction
"""

function! s:t.invalid_filetype()
  let not_exist_command = 0
  for type in ['', 'rb', 'php']
    call s:_assert_filetype(type, not_exist_command)
  endfor
endfunction

function! s:t.valid_filetype()
  let exist_command = 2
  for type in [
        \ 'markdown', 'mkd', 'rst', 'textile',
        \ 'aaa.markdown', 'mkd.foo', 'bb.rst.cc', 'a.b.c.textile',
        \]
    call s:_assert_filetype(type, exist_command)
    call s:_clean_command()
  endfor
endfunction

