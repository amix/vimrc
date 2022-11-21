" Author: Ty-Lucas Kelley <tylucaskelley@gmail.com>
" Description: Adds support for markdownlint

function! ale#handlers#markdownlint#Handle(buffer, lines) abort
    let l:pattern=': \?\(\d\+\)\(:\(\d\+\)\?\)\? \(MD\d\{3}/[A-Za-z0-9-/]\+\) \(.*\)$'
    let l:output=[]

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:result = ({
        \ 'lnum': l:match[1] + 0,
        \ 'code': l:match[4],
        \ 'text': l:match[5],
        \ 'type': 'W',
        \})

        if len(l:match[3]) > 0
            let l:result.col = (l:match[3] + 0)
        endif

        call add(l:output, l:result)
    endfor

    return l:output
endfunction
