" Author: Utkarsh Verma <utkarshverma@protonmail.com>
" Description: AVRA linter for avra syntax.

call ale#Set('avra_avra_executable', 'avra')
call ale#Set('avra_avra_options', '')

function! ale_linters#avra#avra#GetCommand(buffer) abort
    return '%e'
    \   . ' %t'
    \   . ale#Pad(ale#Var(a:buffer, 'avra_avra_options'))
    \   . ' -o ' . g:ale#util#nul_file
endfunction

function! ale_linters#avra#avra#Handle(buffer, lines) abort
    " Note that we treat 'fatal' as errors.
    let l:pattern = '^\S\+(\(\d\+\))\s\+:\s\+\(\S\+\)\s\+:\s\+\(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \ 'lnum': l:match[1] + 0,
        \ 'type': l:match[2] =~? 'Error' ? 'E' : 'W',
        \ 'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('avra', {
\   'name': 'avra',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'avra_avra_executable')},
\   'command': function('ale_linters#avra#avra#GetCommand'),
\   'callback': 'ale_linters#avra#avra#Handle',
\})
