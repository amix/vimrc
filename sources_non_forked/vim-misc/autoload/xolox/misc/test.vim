" Test runner & infrastructure for Vim plug-ins.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 2, 2013
" URL: http://peterodding.com/code/vim/misc/
"
" The Vim auto-load script `autoload/xolox/misc/test.vim` contains
" infrastructure that can be used to run an automated Vim plug-in test suite.
" It provides a framework for running test functions, keeping track of the
" test status, making assertions and reporting test results to the user.

" The process handling tests cannot use the built-in "echo" command from the
" Windows shell because it has way too much idiosyncrasies for me to put up
" with. Seriously. Instead I'm using an "echo.exe" from the UnxUtils project.
if xolox#misc#os#is_win()
  let g:xolox#misc#test#echo = xolox#misc#escape#shell(xolox#misc#path#merge(expand('<sfile>:p:h'), 'echo.exe'))
else
  let g:xolox#misc#test#echo = 'echo'
endif

function! xolox#misc#test#reset() " {{{1
  " Reset counters for executed tests and passed/failed assertions.
  let s:num_executed = 0
  let s:num_passed = 0
  let s:num_failed = 0
  let s:tests_started_at = xolox#misc#timer#start()
endfunction

function! xolox#misc#test#summarize() " {{{1
  " Print a summary of test results, to be interpreted interactively.
  call s:delimit_output()
  call xolox#misc#timer#force("Took %s to run %s: %s passed, %s failed.",
        \ s:tests_started_at,
        \ xolox#misc#format#pluralize(s:num_executed, 'test', 'tests'),
        \ xolox#misc#format#pluralize(s:num_passed, 'assertion', 'assertions'),
        \ xolox#misc#format#pluralize(s:num_failed, 'assertion', 'assertions'))
endfunction

function! xolox#misc#test#wrap(function) " {{{1
  " Call a function in a try/catch block and prevent exceptions from bubbling.
  " The name of the function should be passed as the first and only argument;
  " it should be a string containing the name of a Vim auto-load function.
  let num_failed = s:num_failed
  try
    if s:num_passed + s:num_failed > 0
      call s:delimit_output()
    endif
    let test_name = split(a:function, '#')[-1]
    let test_name = substitute(test_name, '_', ' ', 'g')
    let test_name = substitute(test_name, '^.', '\U\0', '')
    call xolox#misc#msg#info("Running test #%i: %s", s:num_executed + 1, test_name)
    call call(a:function, [])
  catch
    call xolox#misc#msg#warn("Test %s raised exception:", a:function)
    call xolox#misc#msg#warn("%s", v:exception)
    call xolox#misc#msg#warn("(at %s)", v:throwpoint)
    if num_failed == s:num_failed
      " Make sure exceptions are counted as failures, but don't inflate the
      " number of failed assertions when it's not needed (it can produce
      " confusing test output).
      call xolox#misc#test#failed()
    endif
  endtry
  let s:num_executed += 1
endfunction

function! xolox#misc#test#passed() " {{{1
  " Record a test which succeeded.
  let s:num_passed += 1
  call s:print_feedback()
endfunction

function! xolox#misc#test#failed() " {{{1
  " Record a test which failed.
  let s:num_failed += 1
  call s:print_feedback()
endfunction

function! s:delimit_output() " {{{1
  " Print a delimiter between output of tests.
  call xolox#misc#msg#info("%s", repeat("-", 40))
endfunction

function! s:print_feedback() " {{{1
  " Let the user know the status of the test suite.
  call xolox#misc#msg#info("Test status: %s passed, %s failed ..",
        \ xolox#misc#format#pluralize(s:num_passed, 'assertion', 'assertions'),
        \ xolox#misc#format#pluralize(s:num_failed, 'assertion', 'assertions'))
endfunction

function! xolox#misc#test#assert_true(expr) " {{{1
  " Check whether an expression is true.
  if a:expr
    call xolox#misc#test#passed()
  else
    call xolox#misc#test#failed()
    let msg = "Expected value to be true, got %s instead"
    throw printf(msg, string(a:expr))
  endif
endfunction

function! xolox#misc#test#assert_equals(expected, received) " {{{1
  " Check whether two values are the same.
  call xolox#misc#test#assert_same_type(a:expected, a:received)
  if a:expected == a:received
    call xolox#misc#test#passed()
  else
    call xolox#misc#test#failed()
    let msg = "Expected value %s, received value %s!"
    throw printf(msg, string(a:expected), string(a:received))
  endif
endfunction

function! xolox#misc#test#assert_same_type(expected, received) " {{{1
  " Check whether two values are of the same type.
  if type(a:expected) == type(a:received)
    call xolox#misc#test#passed()
  else
    call xolox#misc#test#failed()
    let msg = "Expected value of same type as %s, got value %s!"
    throw printf(msg, string(a:expected), string(a:received))
  endif
endfunction

call xolox#misc#test#reset()
