" Author: w0rp <devw0rp@gmail.com>,
" Nicolas Pauss <https://github.com/nicopauss>
" Description: cython syntax checking for cython files.

call ale#Set('pyrex_cython_executable', 'cython')
call ale#Set('pyrex_cython_options', '--warning-extra')

function! ale_linters#pyrex#cython#GetCommand(buffer) abort
    let l:local_dir = ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))

    return '%e --working ' . l:local_dir . ' --include-dir ' . l:local_dir
    \   . ale#Pad(ale#Var(a:buffer, 'pyrex_cython_options'))
    \   . ' --output-file ' . g:ale#util#nul_file . ' %t'
endfunction

function! ale_linters#pyrex#cython#Handle(buffer, lines) abort
    let l:pattern = '\v^(\w+: )?[^:]+:(\d+):?(\d+)?:? ?(.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[2] + 0,
        \   'col': l:match[3] + 0,
        \   'text': l:match[4],
        \   'type': l:match[1][0] is# 'w' ? 'W' : 'E',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('pyrex', {
\   'name': 'cython',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'pyrex_cython_executable')},
\   'command': function('ale_linters#pyrex#cython#GetCommand'),
\   'callback': 'ale_linters#pyrex#cython#Handle',
\})
