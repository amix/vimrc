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
    let default_makeprg = &makeprg
    let gofiles = join(go#tool#Files(), '" "')

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    if v:shell_error
        let &makeprg = "go build . errors"
    else
        let &makeprg = "go build -o /dev/null " . join(a:000, ' ') . ' "' . gofiles . '"'
    endif

    echon "vim-go: " | echohl Identifier | echon "building ..."| echohl None
    if g:go_dispatch_enabled && exists(':Make') == 2
        silent! exe 'Make'
    else
        silent! exe 'make!'
    endif
    redraw!

    cwindow
    let errors = getqflist()
    if !empty(errors) 
        if !a:bang
            cc 1 "jump to first error if there is any
        endif
    else
        redraws! | echon "vim-go: " | echohl Function | echon "[build] SUCCESS"| echohl None
    endif

    let &makeprg = default_makeprg
    let $GOPATH = old_gopath
endfunction

" Run runs the current file (and their dependencies if any) and outputs it.
" This is intented to test small programs and play with them. It's not
" suitable for long running apps, because vim is blocking by default and
" calling long running apps will block the whole UI.
function! go#cmd#Run(bang, ...)
    let goFiles = '"' . join(go#tool#Files(), '" "') . '"'

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    if go#util#IsWin()
        exec '!go run ' . goFiles
        if v:shell_error
            redraws! | echon "vim-go: [run] " | echohl ErrorMsg | echon "FAILED"| echohl None
        else
            redraws! | echon "vim-go: [run] " | echohl Function | echon "SUCCESS"| echohl None
        endif

        let $GOPATH = old_gopath
        return
    endif

    let default_makeprg = &makeprg
    if !len(a:000)
        let &makeprg = 'go run ' . goFiles
    else
        let &makeprg = "go run " . expand(a:1)
    endif

    if g:go_dispatch_enabled && exists(':Make') == 2
        silent! exe 'Make!'
    else
        exe 'make!'
    endif

    cwindow
    let errors = getqflist()
    if !empty(errors) && !a:bang
        cc 1 "jump to first error if there is any
    endif

    let $GOPATH = old_gopath
    let &makeprg = default_makeprg
endfunction

" Install installs the package by simple calling 'go install'. If any argument
" is given(which are passed directly to 'go insta'') it tries to install those
" packages. Errors are populated in the quickfix window.
function! go#cmd#Install(bang, ...)
    let pkgs = join(a:000, '" "')
    let command = 'go install "' . pkgs . '"'
    call go#cmd#autowrite()
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        call go#tool#ShowErrors(out)
        cwindow
        let errors = getqflist()
        if !empty(errors) && !a:bang
            cc 1 "jump to first error if there is any
        endif
        return
    endif

    echon "vim-go: " | echohl Function | echon "installed to ". $GOPATH | echohl None
endfunction

" Test runs `go test` in the current directory. If compile is true, it'll
" compile the tests instead of running them (useful to catch errors in the
" test files). Any other argument is appendend to the final `go test` command
function! go#cmd#Test(bang, compile, ...)
    let command = "go test "

    " don't run the test, only compile it. Useful to capture and fix errors or
    " to create a test binary.
    if a:compile
        let command .= "-c"
    endif

    if len(a:000)
        let command .= expand(a:1)
    endif

    if len(a:000) == 2
        let command .= a:2
    endif

    call go#cmd#autowrite()
    if a:compile
        echon "vim-go: " | echohl Identifier | echon "compiling tests ..." | echohl None
    else
        echon "vim-go: " | echohl Identifier | echon "testing ..." | echohl None
    endif

    redraw
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        call go#tool#ShowErrors(out)
        cwindow
        let errors = getqflist()
        if !empty(errors) && !a:bang
            cc 1 "jump to first error if there is any
        endif
        echon "vim-go: " | echohl ErrorMsg | echon "[test] FAIL" | echohl None
    else
        call setqflist([])
        cwindow

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
    let flag = "-run \"" . name . "$\""

    let a1 = ""
    if len(a:000)
        let a1 = a:1

        " add extra space
        let flag = " " . flag
    endif

    call go#cmd#Test(a:bang, 0, a1, flag)
endfunction

" Coverage creates a new cover profile with 'go test -coverprofile' and opens
" a new HTML coverage page from that profile.
function! go#cmd#Coverage(bang, ...)
    let l:tmpname=tempname()

    let command = "go test -coverprofile=".l:tmpname

    call go#cmd#autowrite()
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        call go#tool#ShowErrors(out)
    else
        " clear previous quick fix window
        call setqflist([])

        let openHTML = 'go tool cover -html='.l:tmpname
        call go#tool#ExecuteInDir(openHTML)
    endif
    cwindow
    let errors = getqflist()
    if !empty(errors) && !a:bang
        cc 1 "jump to first error if there is any
    endif
    call delete(l:tmpname)
endfunction

" Vet calls "go vet' on the current directory. Any warnings are populated in
" the quickfix window
function! go#cmd#Vet(bang)
    call go#cmd#autowrite()
    echon "vim-go: " | echohl Identifier | echon "calling vet..." | echohl None
    let out = go#tool#ExecuteInDir('go vet')
    if v:shell_error
        call go#tool#ShowErrors(out)
    else
        call setqflist([])
    endif

    let errors = getqflist()
    if !empty(errors) 
        if !a:bang
            cc 1 "jump to first error if there is any
        endif
    else
        redraw | echon "vim-go: " | echohl Function | echon "[vet] PASS" | echohl None
    endif
endfunction
"
" Generate runs 'go generate' in similar fashion to go#cmd#Build()
function! go#cmd#Generate(bang, ...)
    let default_makeprg = &makeprg
    let gofiles = join(go#tool#Files(), '" "')

    let old_gopath = $GOPATH
    let $GOPATH = go#path#Detect()

    if v:shell_error
        let &makeprg = "go generate " . join(a:000, ' ')
    else
        let &makeprg = "go generate " . join(a:000, ' ') . ' "' . gofiles . '"'
    endif

    echon "vim-go: " | echohl Identifier | echon "generating ..."| echohl None
    if g:go_dispatch_enabled && exists(':Make') == 2
        silent! exe 'Make'
    else
        silent! exe 'make!'
    endif
    redraw!

    cwindow
    let errors = getqflist()
    if !empty(errors) 
        if !a:bang
            cc 1 "jump to first error if there is any
        endif
    else
        redraws! | echon "vim-go: " | echohl Function | echon "[generate] SUCCESS"| echohl None
    endif

    let &makeprg = default_makeprg
    let $GOPATH = old_gopath
endfunction

" vim:ts=4:sw=4:et
"
