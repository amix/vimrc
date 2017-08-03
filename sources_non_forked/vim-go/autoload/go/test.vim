" Test runs `go test` in the current directory. If compile is true, it'll
" compile the tests instead of running them (useful to catch errors in the
" test files). Any other argument is appendend to the final `go test` command
function! go#test#Test(bang, compile, ...) abort
  let args = ["test"]

  " don't run the test, only compile it. Useful to capture and fix errors.
  if a:compile
    " we're going to tell to run a test function that doesn't exist. This
    " triggers a build of the test file itself but no tests will run.
    call extend(args, ["-run", "499EE4A2-5C85-4D35-98FC-7377CD87F263"])
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
      echon "vim-go: " | echohl Identifier | echon "compiling tests ..." | echohl None
    else
      echon "vim-go: " | echohl Identifier | echon "testing ..." | echohl None
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
      let id = go#jobcontrol#Spawn(a:bang, "test", args)
    endif

    return id
  endif

  call go#cmd#autowrite()
  redraw

  let command = "go " . join(args, ' ')
  let out = go#tool#ExecuteInDir(command)

  let l:listtype = "quickfix"

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  execute cd fnameescape(expand("%:p:h"))

  if go#util#ShellError() != 0
    let errors = s:parse_errors(split(out, '\n'))
    let errors = go#tool#FilterValids(errors)

    call go#list#Populate(l:listtype, errors, command)
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:bang
      call go#list#JumpToFirst(l:listtype)
    elseif empty(errors)
      " failed to parse errors, output the original content
      call go#util#EchoError(out)
    endif
    echon "vim-go: " | echohl ErrorMsg | echon "[test] FAIL" | echohl None
  else
    call go#list#Clean(l:listtype)
    call go#list#Window(l:listtype)

    if a:compile
      echon "vim-go: " | echohl Function | echon "[test] SUCCESS" | echohl None
    else
      echon "vim-go: " | echohl Function | echon "[test] PASS" | echohl None
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
  endif

  call call('go#test#Test', args)
endfunction

function s:test_job(args) abort
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

  let messages = []
  function! s:callback(chan, msg) closure
    call add(messages, a:msg)
  endfunction

  function! s:exit_cb(job, exitval) closure
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
          call go#util#EchoSuccess("SUCCESS")
        else
          call go#util#EchoSuccess("PASS")
        endif
      else
        call go#util#EchoError("FAILED")
      endif
    endif

    let elapsed_time = reltimestr(reltime(started_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
    let status.state .= printf(" (%ss)", elapsed_time)

    call go#statusline#Update(status_dir, status)

    let l:listtype = go#list#Type("quickfix")
    if a:exitval == 0
      call go#list#Clean(l:listtype)
      call go#list#Window(l:listtype)
      return
    endif

    call s:show_errors(a:args, a:exitval, messages)
  endfunction

  let start_options = {
        \ 'callback': funcref("s:callback"),
        \ 'exit_cb': funcref("s:exit_cb"),
        \ }

  " modify GOPATH if needed
  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  " pre start
  let dir = getcwd()
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let jobdir = fnameescape(expand("%:p:h"))
  execute cd . jobdir

  call job_start(a:args.cmd, start_options)

  " post start
  execute cd . fnameescape(dir)
  let $GOPATH = old_gopath
endfunction

" show_errors parses the given list of lines of a 'go test' output and returns
" a quickfix compatible list of errors. It's intended to be used only for go
" test output. 
function! s:show_errors(args, exit_val, messages) abort
  let l:listtype = go#list#Type("quickfix")

  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  try
    execute cd a:args.jobdir
    let errors = s:parse_errors(a:messages)
    let errors = go#tool#FilterValids(errors)
  finally
    execute cd . fnameescape(a:args.dir)
  endtry

  if !len(errors)
    " failed to parse errors, output the original content
    call go#util#EchoError(join(a:messages, " "))
    call go#util#EchoError(a:args.dir)
    return
  endif

  if a:args.winnr == winnr()
    call go#list#Populate(l:listtype, errors, join(a:args.cmd))
    call go#list#Window(l:listtype, len(errors))
    if !empty(errors) && !a:args.bang
      call go#list#JumpToFirst(l:listtype)
    endif
  endif
endfunction

function! s:parse_errors(lines) abort
  let errors = []

  " NOTE(arslan): once we get JSON output everything will be easier :)
  " https://github.com/golang/go/issues/2981
  for line in a:lines
    let fatalerrors = matchlist(line, '^\(fatal error:.*\)$')
    let tokens = matchlist(line, '^\s*\(.\{-}\.go\):\(\d\+\):\s*\(.*\)')

    if !empty(fatalerrors)
      call add(errors, {"text": fatalerrors[1]})
    elseif !empty(tokens)
      " strip endlines of form ^M
      let out = substitute(tokens[3], '\r$', '', '')

      call add(errors, {
            \ "filename" : fnamemodify(tokens[1], ':p'),
            \ "lnum"     : tokens[2],
            \ "text"     : out,
            \ })
    elseif !empty(errors)
      " Preserve indented lines.
      " This comes up especially with multi-line test output.
      if match(line, '^\s') >= 0
        call add(errors, {"text": line})
      endif
    endif
  endfor

  return errors
endfunction

" vim: sw=2 ts=2 et
"
