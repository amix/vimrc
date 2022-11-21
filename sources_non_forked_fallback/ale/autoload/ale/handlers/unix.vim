" Author: w0rp <devw0rp@gmail.com>
" Description: Error handling for errors in a Unix format.

function! s:HandleUnixFormat(buffer, lines, type) abort
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):?(\d+)?:? ?(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': a:type,
        \})
    endfor

    return l:output
endfunction

function! ale#handlers#unix#HandleAsError(buffer, lines) abort
    return s:HandleUnixFormat(a:buffer, a:lines, 'E')
endfunction

function! ale#handlers#unix#HandleAsWarning(buffer, lines) abort
    return s:HandleUnixFormat(a:buffer, a:lines, 'W')
endfunction
