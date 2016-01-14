if !exists("g:go_dispatch_enabled")
    let g:go_dispatch_enabled = 0
endif

function! go#cmd#autowrite()
    if &autowrite == 1
        silent wall
    endif
endfunction


" Build buils the source code without producting any output binary. We live in
" an editor so the best is to build it to catch errors and fix them. By
" default it tries to call simply 'go build', but it first tries to get all
" dependent files for the current folder and passes it to go build.
function! go#cmd#Build(bang, ...)
    " expand all wildcards(i.e: '%' to the current file name)
    let goargs = map(copy(a:000), "expand(v:val)")

    " escape all shell arguments before we pass it to make
    let goargs = go#util#Shelllist(goargs, 1)

    " create our command arguments. go build discards any results when it
    " compiles multiple packages. So we pass the `errors` package just as an
    " placeholder with the current folder (indicated with '.')
    let args = ["build"]  + goargs + [".", "errors"]

    " if we have nvim, call it asynchronously and return early ;)
    if has('nvim')
        call go#jobcontrol#Spawn(a:bang, "build", args)
        return
    endif

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()
    let default_makeprg = &makeprg
    let &makeprg = "go " . join(args, ' ')

    " execute make inside the source folder so we can parse the errors
    " correctly
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
    let dir = getcwd()
    try
      execute cd . fnameescape(expand("%:p:h"))
      if g:go_dispatch_enabled && exists(':Make') == 2
          call go#util#EchoProgress("building dispatched ...")
          silent! exe 'Make'
      else
          silent! exe 'lmake!'
      endif
      redraw!
    finally
      execute cd . fnameescape(dir)
    endtry

    let errors = go#list#Get()
    call go#list#Window(len(errors))

    if !empty(errors)
        if !a:bang
            call go#list#JumpToFirst()
        endif
    else
        call go#util#EchoSuccess("[build] SUCCESS")
    endif

    let &makeprg = default_makeprg
    let $GOPATH = old_gopath
endfunction


" Run runs the current file (and their dependencies if any) in a new terminal.
function! go#cmd#RunTerm(bang, mode)
    let cmd = "go run ".  go#util#Shelljoin(go#tool#Files())
    call go#term#newmode(a:bang, cmd, a:mode)
endfunction

" Run runs the current file (and their dependencies if any) and outputs it.
" This is intented to test small programs and play with them. It's not
" suitable for long running apps, because vim is blocking by default and
" calling long running apps will block the whole UI.
function! go#cmd#Run(bang, ...)
    if has('nvim')
        call go#cmd#RunTerm(a:bang, '')
        return
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

    if g:go_dispatch_enabled && exists(':Make') == 2
        silent! exe 'Make'
    else
        exe 'lmake!'
    endif

    let items = go#list#Get()
    let errors = go#tool#FilterValids(items)

    call go#list#Populate(errors)
    call go#list#Window(len(errors))
    if !empty(errors) && !a:bang
        call go#list#JumpToFirst()
    endif

    let $GOPATH = old_gopath
    let &makeprg = default_makeprg
endfunction

" Install installs the package by simple calling 'go install'. If any argument
" is given(which are passed directly to 'go instal') it tries to install those
" packages. Errors are populated in the location window.
function! go#cmd#Install(bang, ...)
    let command = 'go install ' . go#util#Shelljoin(a:000)
    call go#cmd#autowrite()
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        let errors = go#tool#ParseErrors(split(out, '\n'))
        call go#list#Populate(errors)
        call go#list#Window(len(errors))
        if !empty(errors) && !a:bang
            call go#list#JumpToFirst()
        endif
        return
    else
        call go#list#Clean()
        call go#list#Window()
    endif

    echon "vim-go: " | echohl Function | echon "installed to ". $GOPATH | echohl None
endfunction

" Test runs `go test` in the current directory. If compile is true, it'll
" compile the tests instead of running them (useful to catch errors in the
" test files). Any other argument is appendend to the final `go test` command
function! go#cmd#Test(bang, compile, ...)
    let args = ["test"]

    " don't run the test, only compile it. Useful to capture and fix errors or
    " to create a test binary.
    if a:compile
        call add(args, "-c")
    endif

    if a:0
        " expand all wildcards(i.e: '%' to the current file name)
        let goargs = map(copy(a:000), "expand(v:val)")

        " escape all shell arguments before we pass it to test
        call extend(args, go#util#Shelllist(goargs, 1))
    else
        " only add this if no custom flags are passed
        let timeout  = get(g:, 'go_test_timeout', '10s')
        call add(args, printf("-timeout=%s", timeout))
    endif

    if has('nvim')
        if get(g:, 'go_term_enabled', 0)
            call go#term#new(a:bang, ["go"] + args)
        else
            call go#jobcontrol#Spawn(a:bang, "test", args)
        endif
        return
    endif

    if a:compile
        echon "vim-go: " | echohl Identifier | echon "compiling tests ..." | echohl None
    else
        echon "vim-go: " | echohl Identifier | echon "testing ..." | echohl None
    endif

    call go#cmd#autowrite()
    redraw

    let command = "go " . join(args, ' ')

    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        let errors = go#tool#ParseErrors(split(out, '\n'))
        let errors = go#tool#FilterValids(errors)

        call go#list#Populate(errors)
        call go#list#Window(len(errors))
        if !empty(errors) && !a:bang
            call go#list#JumpToFirst()
        elseif empty(errors)
            " failed to parse errors, output the original content
            call go#util#EchoError(out)
        endif
        echon "vim-go: " | echohl ErrorMsg | echon "[test] FAIL" | echohl None
    else
        call go#list#Clean()
        call go#list#Window()

        if a:compile
            echon "vim-go: " | echohl Function | echon "[test] SUCCESS" | echohl None
        else
            echon "vim-go: " | echohl Function | echon "[test] PASS" | echohl None
        endif
    endif
endfunction

" Testfunc runs a single test that surrounds the current cursor position.
" Arguments are passed to the `go test` command.
function! go#cmd#TestFunc(bang, ...)
    " search flags legend (used only)
    " 'b' search backward instead of forward
    " 'c' accept a match at the cursor position
    " 'n' do Not move the cursor
    " 'W' don't wrap around the end of the file
    "
    " for the full list
    " :help search
    let test = search("func Test", "bcnW")

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

" Coverage creates a new cover profile with 'go test -coverprofile' and opens
" a new HTML coverage page from that profile.
function! go#cmd#Coverage(bang, ...)
    let l:tmpname=tempname()

    let command = "go test -coverprofile=" . l:tmpname . ' ' . go#util#Shelljoin(a:000)

    call go#cmd#autowrite()
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        let errors = go#tool#ParseErrors(split(out, '\n'))
        call go#list#Populate(errors)
        call go#list#Window(len(errors))
        if !empty(errors) && !a:bang
            call go#list#JumpToFirst()
        endif
    else
        " clear previous location list 
        call go#list#Clean()
        call go#list#Window()

        let openHTML = 'go tool cover -html='.l:tmpname
        call go#tool#ExecuteInDir(openHTML)
    endif

    call delete(l:tmpname)
endfunction

" Generate runs 'go generate' in similar fashion to go#cmd#Build()
function! go#cmd#Generate(bang, ...)
    let default_makeprg = &makeprg

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    " :make expands '%' and '#' wildcards, so they must also be escaped
    let goargs = go#util#Shelljoin(map(copy(a:000), "expand(v:val)"), 1)
    if v:shell_error
        let &makeprg = "go generate " . goargs
    else
        let gofiles = go#util#Shelljoin(go#tool#Files(), 1)
        let &makeprg = "go generate " . goargs . ' ' . gofiles
    endif

    echon "vim-go: " | echohl Identifier | echon "generating ..."| echohl None
    if g:go_dispatch_enabled && exists(':Make') == 2
        silent! exe 'Make'
    else
        silent! exe 'lmake!'
    endif
    redraw!

    let errors = go#list#Get()
    call go#list#Window(len(errors))
    if !empty(errors) 
        if !a:bang
            call go#list#JumpToFirst()
        endif
    else
        redraws! | echon "vim-go: " | echohl Function | echon "[generate] SUCCESS"| echohl None
    endif

    let &makeprg = default_makeprg
    let $GOPATH = old_gopath
endfunction

" vim:ts=4:sw=4:et
