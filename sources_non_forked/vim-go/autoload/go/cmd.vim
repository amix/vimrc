function! go#cmd#autowrite() abort
  if &autowrite == 1
    silent! wall
  endif
endfunction

" Build builds the source code without producting any output binary. We live in
" an editor so the best is to build it to catch errors and fix them. By
" default it tries to call simply 'go build', but it first tries to get all
" dependent files for the current folder and passes it to go build.
function! go#cmd#Build(bang, ...) abort
  " expand all wildcards(i.e: '%' to the current file name)
  let goargs = map(copy(a:000), "expand(v:val)")

  " escape all shell arguments before we pass it to make
  if !has('nvim')
    let goargs = go#util#Shelllist(goargs, 1)
  endif
  " create our command arguments. go build discards any results when it
  " compiles multiple packages. So we pass the `errors` package just as an
  " placeholder with the current folder (indicated with '.')
  let args = ["build"]  + goargs + [".", "errors"]

  if go#util#has_job()
    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoProgress("building dispatched ...")
    endif

    call s:cmd_job({
          \ 'cmd': ['go'] + args,
          \ 'bang': a:bang,
          \})
    return
  elseif has('nvim')
    " if we have nvim, call it asynchronously and return early ;)
    call go#util#EchoProgress("building dispatched ...")
    call go#jobcontrol#Spawn(a:bang, "build", args)
    return
  endif

  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()
  let default_makeprg = &makeprg
  let &makeprg = "go " . join(args, ' ')

  let l:listtype = go#list#Type("quickfix")
  " execute make inside the source folder so we can parse the errors
  " correctly
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    if l:listtype == "locationlist"
      silent! exe 'lmake!'
    else
      silent! exe 'make!'
    endif
    redraw!
  finally
    execute cd . fnameescape(dir)
  endtry

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
  if !empty(errors) && !a:bang
    call go#list#JumpToFirst(l:listtype)
  else
    call go#util#EchoSuccess("[build] SUCCESS")
  endif

  let &makeprg = default_makeprg
  let $GOPATH = old_gopath
endfunction


" Run runs the current file (and their dependencies if any) in a new terminal.
function! go#cmd#RunTerm(bang, mode, files) abort
  if empty(a:files)
    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())
  else
    let cmd = "go run ".  go#util#Shelljoin(map(copy(a:files), "expand(v:val)"), 1)
  endif
  call go#term#newmode(a:bang, cmd, a:mode)
endfunction

" Run runs the current file (and their dependencies if any) and outputs it.
" This is intented to test small programs and play with them. It's not
" suitable for long running apps, because vim is blocking by default and
" calling long running apps will block the whole UI.
function! go#cmd#Run(bang, ...) abort
  if has('nvim')
    call go#cmd#RunTerm(a:bang, '', a:000)
    return
  endif

  if go#util#has_job()
    " NOTE(arslan): 'term': 'open' case is not implement for +jobs. This means
    " executions waiting for stdin will not work. That's why we don't do
    " anything. Once this is implemented we're going to make :GoRun async
  endif

  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  if go#util#IsWin()
    exec '!go run ' . go#util#Shelljoin(go#tool#Files())
    if v:shell_error
      redraws! | echon "vim-go: [run] " | echohl ErrorMsg | echon "FAILED"| echohl None
    else
      redraws! | echon "vim-go: [run] " | echohl Function | echon "SUCCESS"| echohl None
    endif

    let $GOPATH = old_gopath
    return
  endif

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let default_makeprg = &makeprg
  if a:0 == 0
    let &makeprg = 'go run ' . go#util#Shelljoin(go#tool#Files(), 1)
  else
    let &makeprg = "go run " . go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  endif

  let l:listtype = go#list#Type("quickfix")

  if l:listtype == "locationlist"
    exe 'lmake!'
  else
    exe 'make!'
  endif

  let items = go#list#Get(l:listtype)
  let errors = go#tool#FilterValids(items)

  call go#list#Populate(l:listtype, errors, &makeprg)
  call go#list#Window(l:listtype, len(errors))
  if !empty(errors) && !a:bang
    call go#list#JumpToFirst(l:listtype)
  endif

  let $GOPATH = old_gopath
  let &makeprg = default_makeprg
endfunction

" Install installs the package by simple calling 'go install'. If any argument
" is given(which are passed directly to 'go install') it tries to install
" those packages. Errors are populated in the location window.
function! go#cmd#Install(bang, ...) abort
  " use vim's job functionality to call it asynchronously
  if go#util#has_job()
    " expand all wildcards(i.e: '%' to the current file name)
    let goargs = map(copy(a:000), "expand(v:val)")

    " escape all shell arguments before we pass it to make
    let goargs = go#util#Shelllist(goargs, 1)

    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoProgress("installing dispatched ...")
    endif

    call s:cmd_job({
          \ 'cmd': ['go', 'install'] + goargs,
          \ 'bang': a:bang,
          \})
    return
  endif

  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()
  let default_makeprg = &makeprg

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let goargs = go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  let &makeprg = "go install " . goargs

  let l:listtype = go#list#Type("quickfix")
  " execute make inside the source folder so we can parse the errors
  " correctly
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
  let dir = getcwd()
  try
    execute cd . fnameescape(expand("%:p:h"))
    if l:listtype == "locationlist"
      silent! exe 'lmake!'
    else
      silent! exe 'make!'
    endif
    redraw!
  finally
    execute cd . fnameescape(dir)
  endtry

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
  if !empty(errors) && !a:bang
    call go#list#JumpToFirst(l:listtype)
  else
    call go#util#EchoSuccess("installed to ". $GOPATH)
  endif

  let $GOPATH = old_gopath
  let &makeprg = default_makeprg
endfunction

" Test runs `go test` in the current directory. If compile is true, it'll
" compile the tests instead of running them (useful to catch errors in the
" test files). Any other argument is appendend to the final `go test` command
function! go#cmd#Test(bang, compile, ...) abort
  let args = ["test"]

  " don't run the test, only compile it. Useful to capture and fix errors.
  if a:compile
    let compile_file = "vim-go-test-compile"
    call extend(args, ["-c", "-o", compile_file])
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
          \ }

    if a:compile
      let job_args['custom_cb'] = function('s:test_compile', [compile_file])
    endif

    call s:cmd_job(job_args)
    return
  elseif has('nvim')
    " use nvims's job functionality
    if get(g:, 'go_term_enabled', 0)
      let id = go#term#new(a:bang, ["go"] + args)
    else
      let id = go#jobcontrol#Spawn(a:bang, "test", args)
    endif

    if a:compile
      call go#jobcontrol#AddHandler(function('s:test_compile_handler'))
      let s:test_compile_handlers[id] = compile_file
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

  if a:compile
    call delete(compile_file)
  endif

  if go#util#ShellError() != 0
    let errors = go#tool#ParseErrors(split(out, '\n'))
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
function! go#cmd#TestFunc(bang, ...) abort
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

  call call('go#cmd#Test', args)
endfunction

" Generate runs 'go generate' in similar fashion to go#cmd#Build()
function! go#cmd#Generate(bang, ...) abort
  let default_makeprg = &makeprg

  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let goargs = go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  if go#util#ShellError() != 0
    let &makeprg = "go generate " . goargs
  else
    let gofiles = go#util#Shelljoin(go#tool#Files(), 1)
    let &makeprg = "go generate " . goargs . ' ' . gofiles
  endif

  let l:listtype = go#list#Type("quickfix")

  echon "vim-go: " | echohl Identifier | echon "generating ..."| echohl None
  if l:listtype == "locationlist"
    silent! exe 'lmake!'
  else
    silent! exe 'make!'
  endif
  redraw!

  let errors = go#list#Get(l:listtype)
  call go#list#Window(l:listtype, len(errors))
  if !empty(errors) 
    if !a:bang
      call go#list#JumpToFirst(l:listtype)
    endif
  else
    redraws! | echon "vim-go: " | echohl Function | echon "[generate] SUCCESS"| echohl None
  endif

  let &makeprg = default_makeprg
  let $GOPATH = old_gopath
endfunction


" ---------------------
" | Vim job callbacks |
" ---------------------

function s:cmd_job(args) abort
  let status_dir = expand('%:p:h')
  let started_at = reltime()

  call go#statusline#Update(status_dir, {
        \ 'desc': "current status",
        \ 'type': a:args.cmd[1],
        \ 'state': "started",
        \})

  " autowrite is not enabled for jobs
  call go#cmd#autowrite()

  function! s:error_info_cb(job, exit_status, data) closure abort
    let status = {
          \ 'desc': 'last status',
          \ 'type': a:args.cmd[1],
          \ 'state': "success",
          \ }

    if a:exit_status
      let status.state = "failed"
    endif

    let elapsed_time = reltimestr(reltime(started_at))
    " strip whitespace
    let elapsed_time = substitute(elapsed_time, '^\s*\(.\{-}\)\s*$', '\1', '')
    let status.state .= printf(" (%ss)", elapsed_time)

    call go#statusline#Update(status_dir, status)
  endfunction

  let a:args.error_info_cb = function('s:error_info_cb')
  let callbacks = go#job#Spawn(a:args)

  let start_options = {
        \ 'callback': callbacks.callback,
        \ 'close_cb': callbacks.close_cb,
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


" test_compile is called when a GoTestCompile call is finished
function! s:test_compile(test_file, job, exit_status, data) abort
  call delete(a:test_file)
endfunction

" -----------------------
" | Neovim job handlers |
" -----------------------
let s:test_compile_handlers = {}

function! s:test_compile_handler(job, exit_status, data) abort
  if !has_key(s:test_compile_handlers, a:job.id)
    return
  endif
  let l:compile_file = s:test_compile_handlers[a:job.id]
  call delete(l:compile_file)
  unlet s:test_compile_handlers[a:job.id]
endfunction

" vim: sw=2 ts=2 et
