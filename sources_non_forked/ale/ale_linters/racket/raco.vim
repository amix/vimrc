" Author: aqui18 <https://github.com/aqui18>
" Description: This file adds support for checking Racket code with raco.
"              This is the same form of syntax-checking used by DrRacket as well. The
"              downside is that it will only catch the first error, but none of the
"              subsequent ones. This is due to how evaluation in Racket works.

function! ale_linters#racket#raco#Handle(buffer, lines) abort
    " Matches patterns
    " <file>:<line>:<column> <message>
    " eg:
    " info.rkt:4:0: infotab-module: not a well-formed definition
    let l:pattern = '^\(\s\)\@!\(.\+\):\(\d\+\):\(\d\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[3] + 0,
        \   'col': l:match[4] + 0,
        \   'type': 'E',
        \   'text': l:match[5],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('racket', {
\   'name': 'raco',
\   'executable': 'raco',
\   'output_stream': 'stderr',
\   'command': 'raco expand %s',
\   'callback': 'ale_linters#racket#raco#Handle',
\})
