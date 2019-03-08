" Author: Ty-Lucas Kelley <tylucaskelley@gmail.com>
" Description: Adds support for markdownlint

function! ale#handlers#markdownlint#Handle(buffer, lines) abort
    let l:pattern=': \(\d*\): \(MD\d\{3}\)\(\/\)\([A-Za-z0-9-]\+\)\(.*\)$'
    let l:output=[]

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \ 'lnum': l:match[1] + 0,
        \ 'text': '(' . l:match[2] . l:match[3] . l:match[4] . ')' . l:match[5],
        \ 'type': 'W',
        \})
    endfor

    return l:output
endfunction
