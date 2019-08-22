" Make sure some options are set to sane defaults and output all messages in
" English.

" vint: -ProhibitSetNoCompatible

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

set nocompatible nomore shellslash encoding=utf-8 shortmess+=WIF
lang mess C

" Initialize variables.
let s:total_started = reltime()
let s:fail = 0
let s:done = 0
let s:logs = []
let s:gopath = $GOPATH
if !exists('g:test_verbose')
  let g:test_verbose = 0
endif
let g:go_echo_command_info = 0

function! s:logmessages() abort
  " Add all messages (usually errors).
  redir => s:mess
    silent messages
  redir END
  let s:logs = s:logs + filter(split(s:mess, "\n"), 'v:val !~ "^Messages maintainer"')
  silent messages clear
endfunction

" Source the passed test file.
source %

" cd into the folder of the test file.
let s:cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
let s:testfile = expand('%:t')
let s:dir = expand('%:p:h')
execute s:cd . s:dir

" Export root path to vim-go dir.
let g:vim_go_root = fnamemodify(getcwd(), ':p')

" Get a list of all Test_ functions for the given file.
redir @q
  silent function /^Test_
redir END
let s:tests = split(substitute(@q, 'function \(\k\+()\)', '\1', 'g'))

" log any messages already accumulated.
call s:logmessages()
" Iterate over all tests and execute them.
for s:test in sort(s:tests)
  " Since we extract the tests from a regexp the "abort" keyword is also in
  " the list, which is not a test name :-)
  if s:test == 'abort'
    continue
  endif

  let s:started = reltime()
  if g:test_verbose is 1
    call add(s:logs, printf("=== RUN  %s", s:test[:-3]))
  endif
  try
    exe 'call ' . s:test
    " sleep to give events a chance to be processed. This is especially
    " important for the LSP code to have a chance to run before Vim exits,  in
    " order to avoid errors trying to write to the gopls channels since Vim
    " would otherwise stop gopls before the event handlers were run and result
    " in 'stream closed' errors when the events were run _after_ gopls exited.
    sleep 50m
  catch
    let v:errors += [v:exception]
  endtry

  " Restore GOPATH after each test.
  let $GOPATH = s:gopath
  " Restore the working directory after each test.
  execute s:cd . s:dir

  let s:elapsed_time = substitute(reltimestr(reltime(s:started)), '^\s*\(.\{-}\)\s*$', '\1', '')
  let s:done += 1

  if len(v:errors) > 0
    let s:fail += 1
    call add(s:logs, printf("--- FAIL %s (%ss)", s:test[:-3], s:elapsed_time))
    call s:logmessages()
    call extend(s:logs, map(v:errors, '"        ".  v:val'))

    " Reset so we can capture failures of the next test.
    let v:errors = []
  else
    if g:test_verbose is 1
      call s:logmessages()
      call add(s:logs, printf("--- PASS %s (%ss)", s:test[:-3], s:elapsed_time))
    endif
  endif
endfor

" Create an empty fail to indicate that at least one test failed.
if s:fail > 0
  split /tmp/vim-go-test/FAILED
  silent write
endif

let s:total_elapsed_time = substitute(reltimestr(reltime(s:total_started)), '^\s*\(.\{-}\)\s*$', '\1', '')

" Also store all internal messages from s:logs as well.
silent! split /tmp/vim-go-test/test.tmp
call append(line('$'), s:logs)
call append(line('$'), printf("%s %s %s %ss / %s tests",
      \ (s:fail > 0 ? 'FAIL' : 'ok  '),
      \ s:testfile,
      \ repeat(' ', 25 - len(s:testfile)),
      \ s:total_elapsed_time, s:done))
if g:test_verbose is 0
  silent :g/^$/d
endif
silent! write

" Our work here is done.
qall!

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim:ts=2:sts=2:sw=2:et
