if !exists("g:go_errcheck_bin")
    let g:go_errcheck_bin = "errcheck"
endif

function! go#errcheck#Run(...) abort
    if a:0 == 0
        let package = go#package#ImportPath(expand('%:p:h'))
    else
        let package = a:1
    end

    let bin_path = go#tool#BinPath(g:go_errcheck_bin)
    if empty(bin_path)
        return
    endif

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
            % | " Couldn't detect error format, output errors
        endif
        if !empty(errors)
            call setqflist(errors, 'r')
        endif
        echohl Error | echomsg "GoErrCheck returned error" | echohl None
    else
        call setqflist([])
    endif
    cwindow
endfunction
