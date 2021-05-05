" Author: stewy33 <slocumstewy@gmail.com>
" Description: Lints mercury files using mmc

call ale#Set('mercury_mmc_executable', 'mmc')
call ale#Set('mercury_mmc_options', '--make --output-compile-error-lines 100')

function! ale_linters#mercury#mmc#GetCommand(buffer) abort
    return '%e --errorcheck-only '
    \ . ale#Var(a:buffer, 'mercury_mmc_options')
    \ . ' %s:t:r'
endfunction

function! ale_linters#mercury#mmc#Handle(buffer, lines) abort
    " output format
    " <filename>:<line>:   <issue type>: <message>
    let l:pattern = '\v^\w+\.m:(\d+):\s+([W|w]arning|.*[E|e]rror.*): (.*)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': substitute(l:match[1], '\v^0*', '', '') + 0,
        \   'type': l:match[2][0] =~? 'W' ? 'W' : 'E',
        \   'text': l:match[2] . ': ' . l:match[3]
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('mercury', {
\   'name': 'mmc',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'mercury_mmc_executable')},
\   'cwd': '%s:h',
\   'command': function('ale_linters#mercury#mmc#GetCommand'),
\   'callback': 'ale_linters#mercury#mmc#Handle',
\   'lint_file': 1,
\})
