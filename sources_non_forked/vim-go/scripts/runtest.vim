let total_started = reltime()

" add vim-go the only plugin inside the runtimepath
let git_root_path = system("git rev-parse --show-toplevel | tr -d '\\n'")
exe 'set rtp=' . git_root_path

" source the passed test file
source %

" cd into the folder of the test file
let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
let dir = getcwd()
execute cd . expand('%:p:h')

" initialize variables
let g:testname = expand('%')
let s:fail = 0
let s:done = 0
let s:logs = []

" get a list of all Test_ functions for the given file
set nomore
redir @q
silent function /^Test_
redir END
let s:tests = split(substitute(@q, 'function \(\k*()\)', '\1', 'g'))

" Iterate over all tests and execute them
for s:test in sort(s:tests)
  let started = reltime()

  call add(s:logs, printf("=== RUN   %s", s:test[:-3]))
  exe 'call ' . s:test

  let elapsed_time = reltimestr(reltime(started))
  let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')

  let s:done += 1

  if len(v:errors) > 0
    let s:fail += 1
    call add(s:logs, printf("--- FAIL: %s (%ss)", s:test[:-3], elapsed_time))
    call extend(s:logs, map(v:errors, '"        ".  v:val'))

    " reset so we can capture failures of next test
    let v:errors = []
  else
    call add(s:logs, printf("--- PASS: %s (%ss)", s:test[:-3], elapsed_time))
  endif
endfor

" pop out into the scripts folder
execute cd . fnameescape(dir)

" create an empty fail to indicate that the test failed
if s:fail > 0
  split FAILED
  write
endif

let total_elapsed_time = reltimestr(reltime(total_started))
let total_elapsed_time = substitute(total_elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')

let message = 'Executed ' . s:done . (s:done > 1 ? ' tests' : ' test') . '. Total test time: '. total_elapsed_time .'s'
call add(s:logs, "")
call add(s:logs, message)

" store all error messages from within vim into test.log
redir > test.log
silent messages
redir END

" also store all internal messages from s:logs: as well
split test.log
call append(line('$'), '')
call append(line('$'), 'From ' . g:testname . ':')
call append(line('$'), s:logs)
write

" bye, bye!
qall!
