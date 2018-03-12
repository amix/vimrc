function! go#cmd#autowrite() abort
  if &autowrite == 1 || &autowriteall == 1
    silent! wall
  endif
endfunction

" Build builds the source code without producing any output binary. We live in
" an editor so the best is to build it to catch errors and fix them. By
" default it tries to call simply 'go build', but it first tries to get all
" dependent files for the current folder and passes it to go build.
function! go#cmd#Build(bang, ...) abort
  " Create our command arguments. go build discards any results when it
  " compiles multiple packages. So we pass the `errors` package just as an
  " placeholder with the current folder (indicated with '.'). We also pass -i
  " that tries to install the dependencies, this has the side effect that it
  " caches the build results, so every other build is faster.
  let args =
        \ ["build"] +
        \ map(copy(a:000), "expand(v:val)") +
        \ ["-i", ".", "errors"]

  " Vim async.
  if go#util#has_job()
    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoProgress("building dispatched ...")
    endif

    call s:cmd_job({
          \ 'cmd': ['go'] + args,
          \ 'bang': a:bang,
          \ 'for': 'GoBuild',
          \})

  " Nvim async.
  elseif has('nvim')
    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoProgress("building dispatched ...")
    endif

    call go#jobcontrol#Spawn(a:bang, "build", "GoBuild", args)

  " Vim 7.4 without async
  else
    let default_makeprg = &makeprg
    let &makeprg = "go " . join(go#util#Shelllist(args), ' ')

    let l:listtype = go#list#Type("GoBuild")
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
  endif
endfunction


" BuildTags sets or shows the current build tags used for tools
function! go#cmd#BuildTags(bang, ...) abort
  if a:0
    if a:0 == 1 && a:1 == '""'
      unlet g:go_build_tags
      call go#util#EchoSuccess("build tags are cleared")
    else
      let g:go_build_tags = a:1
      call go#util#EchoSuccess("build tags are changed to: ". a:1)
    endif

    return
  endif

  if !exists('g:go_build_tags')
    call go#util#EchoSuccess("build tags are not set")
  else
    call go#util#EchoSuccess("current build tags: ". g:go_build_tags)
  endif
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
" This is intended to test small programs and play with them. It's not
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

  if go#util#IsWin()
    exec '!go run ' . go#util#Shelljoin(go#tool#Files())
    if v:shell_error
      redraws! | echon "vim-go: [run] " | echohl ErrorMsg | echon "FAILED"| echohl None
    else
      redraws! | echon "vim-go: [run] " | echohl Function | echon "SUCCESS"| echohl None
    endif

    return
  endif

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let default_makeprg = &makeprg
  if a:0 == 0
    let &makeprg = 'go run ' . go#util#Shelljoin(go#tool#Files(), 1)
  else
    let &makeprg = "go run " . go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  endif

  let l:listtype = go#list#Type("GoRun")

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

    if get(g:, 'go_echo_command_info', 1)
      call go#util#EchoProgress("installing dispatched ...")
    endif

    call s:cmd_job({
          \ 'cmd': ['go', 'install'] + goargs,
          \ 'bang': a:bang,
          \ 'for': 'GoInstall',
          \})
    return
  endif

  let default_makeprg = &makeprg

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let goargs = go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  let &makeprg = "go install " . goargs

  let l:listtype = go#list#Type("GoInstall")
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
    call go#util#EchoSuccess("installed to ". go#path#Default())
  endif

  let &makeprg = default_makeprg
endfunction

" Generate runs 'go generate' in similar fashion to go#cmd#Build()
function! go#cmd#Generate(bang, ...) abort
  let default_makeprg = &makeprg

  " :make expands '%' and '#' wildcards, so they must also be escaped
  let goargs = go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
  if go#util#ShellError() != 0
    let &makeprg = "go generate " . goargs
  else
    let gofiles = go#util#Shelljoin(go#tool#Files(), 1)
    let &makeprg = "go generate " . goargs . ' ' . gofiles
  endif

  let l:listtype = go#list#Type("GoGenerate")

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

  function! s:complete(job, exit_status, data) closure abort
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

  let a:args.complete = funcref('s:complete')
  let callbacks = go#job#Spawn(a:args)

  let start_options = {
        \ 'callback': callbacks.callback,
        \ 'exit_cb': callbacks.exit_cb,
        \ 'close_cb': callbacks.close_cb,
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

" vim: sw=2 ts=2 et
