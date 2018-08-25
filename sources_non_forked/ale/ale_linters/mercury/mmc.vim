" Author: stewy33 <slocumstewy@gmail.com>
" Description: Lints mercury files using mmc

call ale#Set('mercury_mmc_executable', 'mmc')
call ale#Set('mercury_mmc_options', '--make --output-compile-error-lines 100')

function! ale_linters#mercury#mmc#GetCommand(buffer) abort
    let l:module_name = expand('#' . a:buffer . ':t:r')

    return ale#path#BufferCdString(a:buffer)
    \ . '%e --errorcheck-only '
    \ . ale#Var(a:buffer, 'mercury_mmc_options')
    \ . ' ' . l:module_name
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
\   'executable_callback': ale#VarFunc('mercury_mmc_executable'),
\   'command_callback': 'ale_linters#mercury#mmc#GetCommand',
\   'callback': 'ale_linters#mercury#mmc#Handle',
\   'lint_file': 1,
\})
