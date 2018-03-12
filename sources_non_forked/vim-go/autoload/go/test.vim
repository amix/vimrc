" Test runs `go test` in the current directory. If compile is true, it'll
" compile the tests instead of running them (useful to catch errors in the
" test files). Any other argument is appended to the final `go test` command.
function! go#test#Test(bang, compile, ...) abort
  let args = ["test"]

  " don't run the test, only compile it. Useful to capture and fix errors.
  if a:compile
    let testfile = tempname() . ".vim-go.test"
    call extend(args, ["-c", "-o", testfile])
  endif

  if exists('g:go_build_tags')
    let tags = get(g:, 'go_build_tags')
    call extend(args, ["-tags", tags])
  endif

  if a:0
    let goargs = a:000

    " do not expand for coverage mode as we're passing the arg ourself
    if a:1 != '-coverprofile'
      " expand all wildcards(i.e: '%' to the current file name)
      let goargs = map(copy(a:000), "expand(v:val)")
    endif

    if !(has('nvim') || go#util#has_job())
      let goargs = go#util#Shelllist(goargs, 1)
    endif

    call extend(args, goargs, 1)
  else
    " only add this if no custom flags are passed
    let timeout  = get(g:, 'go_test_timeout', '10s')
    call add(args, printf("-timeout=%s", timeout))
  endif

  if get(g:, 'go_echo_command_info', 1)
    if a:compile
      call go#util#EchoProgress("compiling tests ...")
    else
      call go#util#EchoProgress("testing...")
    endif
  endif

  if go#util#has_job()
    " use vim's job functionality to call it asynchronously
    let job_args = {
          \ 'cmd': ['go'] + args,
          \ 'bang': a:bang,
          \ 'winnr': winnr(),
          \ 'dir': getcwd(),
          \ 'compile_test': a:compile,
          \ 'jobdir': fnameescape(expand("%:p:h")),
          \ }

    call s:test_job(job_args)
    return
  elseif has('nvim')
    " use nvims's job functionality
    if get(g:, 'go_term_enabled', 0)
      let id = go#term#new(a:bang, ["go"] + args)
    else
      let id = go#jobcontrol#Spawn(a:bang, "test", "GoTest", args)
    endif

    return id
  endif

  call go#cmd#autowrite()
  redraw

  let command = "go " . join(args, ' ')
  let out = go#tool#ExecuteInDir(command)
  " TODO(bc): When the output is JSON, the JSON should be run through a
  " filter to produce lines that are more easily described by errorformat.

  let l:listtype = go#list#Type("GoTest")

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  execute cd fnameescape(expand("%:p:h"))

  if go#util#ShellError() != 0
    call go#list#ParseFormat(l:listtype, s:errorformat(), split(out, '\n'), command)
    let errors = go#list#Get(l:listtype)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    elseif empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(out)
    endif
    call go#util#EchoError("[test] FAIL")
  else
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)

    if a:compile
      call go#util#EchoSuccess("[test] SUCCESS")
    else
      call go#util#EchoSuccess("[test] PASS")
    endif
  endif
  execute cd . fnameescape(dir)
endfunction

" Testfunc runs a single test that surrounds the current cursor position.
" Arguments are passed to the `go test` command.
function! go#test#Func(bang, ...) abort
  " search flags legend (used only)
  " 'b' search backward instead of forward
  " 'c' accept a match at the cursor position
  " 'n' do Not move the cursor
  " 'W' don't wrap around the end of the file
  "
  " for the full list
  " :help search
  let test = search('func \(Test\|Example\)', "bcnW")

  if test == 0
    echo "vim-go: [test] no test found immediate to cursor"
    return
  end

  let line = getline(test)
  let name = split(split(line, " ")[1], "(")[0]
  let args = [a:bang, 0, "-run", name . "$"]

  if a:0
    call extend(args, a:000)
  else
    " only add this if no custom flags are passed
    let timeout  = get(g:, 'go_test_timeout', '10s')
    call add(args, printf("-timeout=%s", timeout))
  endif

  call call('go#test#Test', args)
endfunction

function! s:test_job(args) abort
  let status_dir = expand('%:p:h')
  let started_at = reltime()

  let status = {
        \ 'desc': 'current status',
        \ 'type': "test",
        \ 'state': "started",
        \ }

  if a:args.compile_test
    let status.state = "compiling"
  endif

  call go#statusline#Update(status_dir, status)

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  let l:exited = 0
  let l:closed = 0
  let l:exitval = 0
  let messages = []

  function! s:callback(chan, msg) closure
    call add(messages, a:msg)
  endfunction

  function! s:exit_cb(job, exitval) closure
    let exited = 1
    let exitval = a:exitval

    let status = {
          \ 'desc': 'last status',
          \ 'type': "test",
          \ 'state': "pass",
          \ }

    if a:args.compile_test
      let status.state = "success"
    endif

    if a:exitval
      let status.state = "failed"
    endif

    if get(g:, 'go_echo_command_info', 1)
      if a:exitval == 0
        if a:args.compile_test
          call go#util#EchoSuccess("[test] SUCCESS")
        else
          call go#util#EchoSuccess("[test] PASS")
        endif
      else
        call go#util#EchoError("[test] FAIL")
      endif
    endif

    let elapsed_time = reltimestr(reltime(started_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
    let status.state .= printf(" (%ss)", elapsed_time)

    call go#statusline#Update(status_dir, status)

    if closed
      call s:show_errors(a:args, l:exitval, messages)
    endif
  endfunction

  function! s:close_cb(ch) closure
    let closed = 1

    if exited
      call s:show_errors(a:args, l:exitval, messages)
    endif
  endfunction

  let start_options = {
        \ 'callback': funcref("s:callback"),
        \ 'exit_cb': funcref("s:exit_cb"),
        \ 'close_cb': funcref("s:close_cb"),
        \ }

  " pre start
  let dir = getcwd()
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let jobdir = fnameescape(expand("%:p:h"))
  execute cd . jobdir

  call job_start(a:args.cmd, start_options)

  " post start
  execute cd . fnameescape(dir)
endfunction

" show_errors parses the given list of lines of a 'go test' output and returns
" a quickfix compatible list of errors. It's intended to be used only for go
" test output.
function! s:show_errors(args, exit_val, messages) abort
    let l:listtype = go#list#Type("GoTest")
    if a:exit_val == 0
      call go#list#Clean(l:listtype)
      call go#list#Window(l:listtype)
      return
    endif

  " TODO(bc): When messages is JSON, the JSON should be run through a
  " filter to produce lines that are more easily described by errorformat.

  let l:listtype = go#list#Type("GoTest")

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  try
    execute cd a:args.jobdir
    call go#list#ParseFormat(l:listtype, s:errorformat(), a:messages, join(a:args.cmd))
    let errors = go#list#Get(l:listtype)
  finally
    execute cd . fnameescape(a:args.dir)
  endtry

  if !len(errors)
    " failed to parse errors, output the original content
    call go#util#EchoError(a:messages)
    call go#util#EchoError(a:args.dir)
    return
  endif

  if a:args.winnr == winnr()
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:args.bang
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction


let s:efm= ""
let s:go_test_show_name=0

function! s:errorformat() abort
  " NOTE(arslan): once we get JSON output everything will be easier :).
  " TODO(bc): When the output is JSON, the JSON should be run through a
  " filter to produce lines that are more easily described by errorformat.
  "   https://github.com/golang/go/issues/2981.
  let goroot = go#util#goroot()

  let show_name=get(g:, 'go_test_show_name', 0)
  if s:efm != "" && s:go_test_show_name == show_name
    return s:efm
  endif
  let s:go_test_show_name = show_name

  " each level of test indents the test output 4 spaces. Capturing groups
  " (e.g. \(\)) cannot be used in an errorformat, but non-capturing groups can
  " (e.g. \%(\)).
  let indent = '%\\%(    %\\)%#'

  " match compiler errors
  let format = "%f:%l:%c: %m"

  " ignore `go test -v` output for starting tests
  let format .= ",%-G=== RUN   %.%#"
  " ignore `go test -v` output for passing tests
  let format .= ",%-G" . indent . "--- PASS: %.%#"

  " Match failure lines.
  "
  " Test failures start with '--- FAIL: ', followed by the test name followed
  " by a space the duration of the test in parentheses
  "
  " e.g.:
  "   '--- FAIL: TestSomething (0.00s)'
  if show_name
    let format .= ",%G" . indent . "--- FAIL: %m (%.%#)"
  else
    let format .= ",%-G" . indent . "--- FAIL: %.%#"
  endif

  " Matches test output lines.
  "
  " All test output lines start with the test indentation and a tab, followed
  " by the filename, a colon, the line number, another colon, a space, and the
  " message. e.g.:
  "   '\ttime_test.go:30: Likely problem: the time zone files have not been installed.'
  let format .= ",%A" . indent . "%\\t%\\+%f:%l: %m"
  " also match lines that don't have a message (i.e. the message begins with a
  " newline or is the empty string):
  " e.g.:
  "     t.Errorf("\ngot %v; want %v", actual, expected)
  "     t.Error("")
  let format .= ",%A" . indent . "%\\t%\\+%f:%l: "

  " Match the 2nd and later lines of multi-line output. These lines are
  " indented the number of spaces for the level of nesting of the test,
  " followed by two tabs, followed by the message.
  "
  " Treat these lines as if they are stand-alone lines of output by using %G.
  " It would also be valid to treat these lines as if they were the
  " continuation of a multi-line error by using %C instead of %G, but that
  " would also require that all test errors using a %A or %E modifier to
  " indicate that they're multiple lines of output, but in that case the lines
  " get concatenated in the quickfix list, which is not what users typically
  " want when writing a newline into their test output.
  let format .= ",%G" . indent . "%\\t%\\{2}%m"

  " set the format for panics.

  " handle panics from test timeouts
  let format .= ",%+Gpanic: test timed out after %.%\\+"

  " handle non-timeout panics
  " In addition to 'panic', check for 'fatal error' to support older versions
  " of Go that used 'fatal error'.
  "
  " Panics come in two flavors. When the goroutine running the tests panics,
  " `go test` recovers and tries to exit more cleanly. In that case, the panic
  " message is suffixed with ' [recovered]'. If the panic occurs in a
  " different goroutine, it will not be suffixed with ' [recovered]'.
  let format .= ",%+Afatal error: %.%# [recovered]"
  let format .= ",%+Apanic: %.%# [recovered]"
  let format .= ",%+Afatal error: %.%#"
  let format .= ",%+Apanic: %.%#"

  " Match address lines in stacktraces produced by panic.
  "
  " Address lines in the stack trace have leading tabs, followed by the path
  " to the file. The file path is followed by a colon and then the line number
  " within the file where the panic occurred. After that there's a space and
  " hexadecimal number.
  "
  " e.g.:
  "   '\t/usr/local/go/src/time.go:1313 +0x5d'

  " panicaddress, and readyaddress are identical except for
  " panicaddress sets the filename and line number.
  let panicaddress = "%\\t%f:%l +0x%[0-9A-Fa-f]%\\+"
  let readyaddress = "%\\t%\\f%\\+:%\\d%\\+ +0x%[0-9A-Fa-f]%\\+"
  " stdlib address is identical to readyaddress, except it matches files
  " inside GOROOT.
  let stdlibaddress = "%\\t" . goroot . "%\\f%\\+:%\\d%\\+ +0x%[0-9A-Fa-f]%\\+"

  " Match and ignore the running goroutine line.
  let format .= ",%-Cgoroutine %\\d%\\+ [running]:"
  " Match address lines that refer to stdlib, but consider them informational
  " only. This is to catch the lines after the first address line in the
  " running goroutine of a panic stack trace. Ideally, this wouldn't be
  " necessary, but when a panic happens in the goroutine running a test, it's
  " recovered and another panic is created, so the stack trace actually has
  " the line that caused the original panic a couple of addresses down the
  " stack.
  let format .= ",%-C" . stdlibaddress
  " Match address lines in the first matching goroutine. This means the panic
  " message will only be shown as the error message in the first address of
  " the running goroutine's stack.
  let format .= ",%Z" . panicaddress

  " Match and ignore panic address without being part of a multi-line message.
  " This is to catch those lines that come after the top most non-standard
  " library line in stack traces.
  let format .= ",%-G" . readyaddress

  " Match and ignore exit status lines (produced when go test panics) whether
  " part of a multi-line message or not, because these lines sometimes come
  " before and sometimes after panic stacktraces.
  let format .= ",%-Cexit status %[0-9]%\\+"
  "let format .= ",exit status %[0-9]%\\+"

  " Match and ignore exit failure lines whether part of a multi-line message
  " or not, because these lines sometimes come before and sometimes after
  " panic stacktraces.
  let format .= ",%-CFAIL%\\t%.%#"
  "let format .= ",FAIL%\\t%.%#"

  " Match and ignore everything else in multi-line messages.
  let format .= ",%-C%.%#"
  " Match and ignore everything else not in a multi-line message:
  let format .= ",%-G%.%#"

  let s:efm = format

  return s:efm
endfunction

" vim: sw=2 ts=2 et
