" Author: Martino Pilia <martino.pilia@gmail.com>
" Description: Lint ispc files with the Intel(R) SPMD Program Compiler

call ale#Set('ispc_ispc_executable', 'ispc')
call ale#Set('ispc_ispc_options', '')

function! ale_linters#ispc#ispc#GetCommand(buffer) abort
    " --nowrap: do not wrap message lines
    return '%e --nowrap'
    \   . ale#Pad(ale#c#IncludeOptions(ale#c#FindLocalHeaderPaths(a:buffer)))
    \   . ale#Pad(ale#Var(a:buffer, 'ispc_ispc_options'))
    \   . ' %s'
endfunction

" Note that we ignore the two warnings in the beginning of the compiler output
" ('no output file specified' and 'no --target specified'), since they have
" nothing to do with linting.
function! ale_linters#ispc#ispc#Handle(buffer, lines) abort
    " Message format: <filename>:<lnum>:<col> <type>: <text>
    " As far as I know, <type> can be any of:
    "   'error', 'Error', 'fatal error', 'Warning', 'Performance Warning'
    let l:re = '\v.+:([0-9]+):([0-9]+):\s+([^:]+):\s+(.+)'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:re)
        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': str2nr(l:match[1]),
        \   'col': str2nr(l:match[2]),
        \   'type': l:match[3] =~? 'error' ? 'E' : 'W',
        \   'text': l:match[4],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('ispc', {
\   'name': 'ispc',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'ispc_ispc_executable')},
\   'command': function('ale_linters#ispc#ispc#GetCommand'),
\   'callback': 'ale_linters#ispc#ispc#Handle',
\   'lint_file': 1,
\})
