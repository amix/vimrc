if !exists("g:go_errcheck_bin")
    let g:go_errcheck_bin = "errcheck"
endif

function! go#errcheck#Run(...) abort
    if a:0 == 0
        let package = go#package#ImportPath(expand('%:p:h'))
        if package == -1
            echohl Error | echomsg "vim-go: package is not inside GOPATH src" | echohl None
            return
        endif
    else
        let package = a:1
    end

    let bin_path = go#tool#BinPath(g:go_errcheck_bin)
    if empty(bin_path)
        return
    endif

    echon "vim-go: " | echohl Identifier | echon "errcheck analysing ..." | echohl None
    let out = system(bin_path . ' ' . package)
    if v:shell_error
        let errors = []
        let mx = '^\(.\{-}\):\(\d\+\):\(\d\+\)\s*\(.*\)'
        for line in split(out, '\n')
            let tokens = matchlist(line, mx)

            if !empty(tokens)
                call add(errors, {"filename": expand(DefaultGoPath() . "/src/" . tokens[1]),
                            \"lnum": tokens[2],
                            \"col": tokens[3],
                            \"text": tokens[4]})
            endif
        endfor

        if empty(errors)
            echohl Error | echomsg "GoErrCheck returned error" | echohl None
            echo out
        endif

        if !empty(errors)
            redraw | echo
            call setqflist(errors, 'r')
        endif
    else
        call setqflist([])
    endif

    cwindow
endfunction

" vim:ts=4:sw=4:et
