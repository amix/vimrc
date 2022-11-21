" Author: awlayton <alex@layton.in>
" Description: mlint for MATLAB files

call ale#Set('matlab_mlint_executable', 'mlint')

function! ale_linters#matlab#mlint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " L 27 (C 1): FNDEF: Terminate statement with semicolon to suppress output.
    " L 30 (C 13-15): FNDEF: A quoted string is unterminated.
    let l:pattern = '^L \(\d\+\) (C \([0-9-]\+\)): \([A-Z]\+\): \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:lnum = l:match[1] + 0
        let l:col = l:match[2] + 0
        let l:code = l:match[3]
        let l:text = l:match[4]

        " Suppress erroneous warning about filename
        " TODO: Enable this error when copying filename is supported
        if l:code is# 'FNDEF'
            continue
        endif

        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:lnum,
        \   'col': l:col,
        \   'text': l:text,
        \   'type': 'W',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('matlab', {
\   'name': 'mlint',
\   'executable': {b -> ale#Var(b, 'matlab_mlint_executable')},
\   'command': '%e -id %t',
\   'output_stream': 'stderr',
\   'callback': 'ale_linters#matlab#mlint#Handle',
\})
