" Author: rhysd <https://github.com/rhysd>
" Description: Handle errors for naga-cli.

function! ale#handlers#naga#Handle(buffer, lines) abort
    let l:errors = []
    let l:current_error = v:null

    for l:line in a:lines
        if l:line =~# '^error: '
            let l:text = l:line[7:]
            let l:current_error = { 'text': l:text, 'type': 'E' }
            continue
        endif

        if l:current_error isnot v:null
            let l:matches = matchlist(l:line, '\v:(\d+):(\d+)$')

            if !empty(l:matches)
                let l:current_error.lnum = str2nr(l:matches[1])
                let l:current_error.col = str2nr(l:matches[2])
                call add(l:errors, l:current_error)
                let l:current_error = v:null
                continue
            endif
        endif
    endfor

    return l:errors
endfunction

