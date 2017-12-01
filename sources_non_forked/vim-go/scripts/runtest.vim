" Make sure some options are set to sane defaults and output all messages in
" English.

" vint: -ProhibitSetNoCompatible
set nocompatible nomore shellslash encoding=utf-8 shortmess+=WIF
lang mess C

" Initialize variables.
let s:total_started = reltime()
let s:fail = 0
let s:done = 0
let s:logs = []
let s:gopath = $GOPATH

" Source the passed test file.
source %

" cd into the folder of the test file.
let s:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
let s:testfile = expand('%:t')
execute s:cd . expand('%:p:h')

" Export root path to vim-go dir.
let g:vim_go_root = fnamemodify(getcwd(), ':p')

" Get a list of all Test_ functions for the given file.
redir @q
  silent function /^Test_
redir END
let s:tests = split(substitute(@q, 'function \(\k*()\)', '\1', 'g'))

" Iterate over all tests and execute them.
for s:test in sort(s:tests)
  " Since we extract the tests from a regexp the "abort" keyword is also in the
  " list, which is not a test name :-)
  if s:test == 'abort'
    continue
  endif

  let s:started = reltime()
  call add(s:logs, printf("=== RUN  %s", s:test[:-3]))
  exe 'call ' . s:test

  " Restore GOPATH after each test.
  let $GOPATH = s:gopath

  let s:elapsed_time = substitute(reltimestr(reltime(s:started)), '^\s*\(.\{-}\)\s*$', '\1', '')
  let s:done += 1

  if len(v:errors) > 0
    let s:fail += 1
    call add(s:logs, printf("--- FAIL %s (%ss)", s:test[:-3], s:elapsed_time))
    call extend(s:logs, map(v:errors, '"        ".  v:val'))

    " Reset so we can capture failures of the next test.
    let v:errors = []
  else
    call add(s:logs, printf("--- PASS %s (%ss)", s:test[:-3], s:elapsed_time))
  endif
endfor

" Create an empty fail to indicate that at least one test failed.
if s:fail > 0
  split /tmp/vim-go-test/FAILED
  silent write
endif

let s:total_elapsed_time = substitute(reltimestr(reltime(s:total_started)), '^\s*\(.\{-}\)\s*$', '\1', '')

" Add all messages (usually errors).
redir => s:mess
  silent messages
redir END
let s:logs = s:logs + filter(split(s:mess, "\n"), 'v:val !~ "^Messages maintainer"')

" Also store all internal messages from s:logs as well.
silent! split /tmp/vim-go-test/test.tmp
call append(line('$'), s:logs)
call append(line('$'), printf("%s%s       %s / %s tests",
      \ (s:fail > 0 ? 'FAIL     ' : 'ok       '),
      \ s:testfile, s:total_elapsed_time, s:done))
silent! write

" Our work here is done.
qall!

" vim:ts=2:sts=2:sw=2:et
