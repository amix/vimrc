if !exists("g:go_jump_to_error")
    let g:go_jump_to_error = 1
endif

function! go#cmd#Run(bang, ...)
    let goFiles = '"' . join(go#tool#Files(), '" "') . '"'

    if IsWin()
        exec '!go run ' . goFiles
        if v:shell_error
            redraws! | echon "vim-go: [run] " | echohl ErrorMsg | echon "FAILED"| echohl None
        else
            redraws! | echon "vim-go: [run] " | echohl Function | echon "SUCCESS"| echohl None
        endif

        return
    endif

    let default_makeprg = &makeprg
    if !len(a:000)
        let &makeprg = 'go run ' . goFiles
    else
        let &makeprg = "go run " . expand(a:1)
    endif

    exe 'make!'
    if !a:bang
        cwindow
        let errors = getqflist()
        if !empty(errors)
            if g:go_jump_to_error
                cc 1 "jump to first error if there is any
            endif
        endif
    endif

    let &makeprg = default_makeprg
endfunction

function! go#cmd#Install(...)
    let pkgs = join(a:000, '" "')
    let command = 'go install "' . pkgs . '"'
    let out = go#tool#ExecuteInDir(command)
    if v:shell_error
        call go#tool#ShowErrors(out)
        cwindow
        return
    endif

    if exists("$GOBIN")
        echon "vim-go: " | echohl Function | echon "installed to ". $GOBIN | echohl None
    else
        echon "vim-go: " | echohl Function | echon "installed to ". $GOPATH . "/bin" | echohl None
    endif
endfunction

function! go#cmd#Build(bang, ...)
    let default_makeprg = &makeprg
    let gofiles = join(go#tool#Files(), '" "')
    if v:shell_error
        let &makeprg = "go build . errors"
    else
        let &makeprg = "go build -o /dev/null " . join(a:000, ' ') . ' "' . gofiles . '"'
    endif

    echon "vim-go: " | echohl Identifier | echon "building ..."| echohl None
    silent! exe 'make!'
    redraw!
    if !a:bang
        cwindow
        let errors = getqflist()
        if !empty(errors)
            if g:go_jump_to_error
                cc 1 "jump to first error if there is any
            endif
        else
            redraws! | echon "vim-go: " | echohl Function | echon "[build] SUCCESS"| echohl None
        endif
    endif

    let &makeprg = default_makeprg
endfunction

function! go#cmd#Test(compile, ...)
    let command = "go test "

    " don't run the test, only compile it. Useful to capture and fix errors or
    " to create a test binary.
    if a:compile
        let command .= "-c"
    endif

    if len(a:000)
        let command .= expand(a:1)
    endif

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
        if !empty(errors)
            if g:go_jump_to_error
                cc 1 "jump to first error if there is any
            endif
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

function! go#cmd#Coverage(...)
    let l:tmpname=tempname()

    let command = "go test -coverprofile=".l:tmpname

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
    if !empty(errors)
        if g:go_jump_to_error
            cc 1 "jump to first error if there is any
        endif
    endif

    call delete(l:tmpname)
endfunction

function! go#cmd#Vet()
    echon "vim-go: " | echohl Identifier | echon "calling vet..." | echohl None
    let out = go#tool#ExecuteInDir('go vet')
    if v:shell_error
        call go#tool#ShowErrors(out)
    else
        call setqflist([])
    endif
    cwindow

    let errors = getqflist()
    if !empty(errors)
        if g:go_jump_to_error
            cc 1 "jump to first error if there is any
        endif
    else
        redraw | echon "vim-go: " | echohl Function | echon "[vet] PASS" | echohl None
    endif
endfunction

" vim:ts=4:sw=4:et
"
