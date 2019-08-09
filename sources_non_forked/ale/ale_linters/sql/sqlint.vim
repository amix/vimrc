" Author: Adriaan Zonnenberg <amz@adriaan.xyz>
" Description: sqlint for SQL files

function! ale_linters#sql#sqlint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " stdin:3:1:ERROR syntax error at or near "WIBBLE"
    let l:pattern = '\v^[^:]+:(\d+):(\d+):(\u+) (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'type': l:match[3][0],
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('sql', {
\   'name': 'sqlint',
\   'executable': 'sqlint',
\   'command': 'sqlint',
\   'callback': 'ale_linters#sql#sqlint#Handle',
\})
