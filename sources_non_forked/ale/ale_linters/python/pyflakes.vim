" Author: w0rp <devw0rp@gmail.com>
" Description: pyflakes for python files

call ale#Set('python_pyflakes_executable', 'pyflakes')
call ale#Set('python_pyflakes_use_global', 0)

function! ale_linters#python#pyflakes#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_pyflakes', ['pyflakes'])
endfunction

function! ale_linters#python#pyflakes#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyflakes#GetExecutable(a:buffer)

    return ale#Escape(l:executable) . ' %t'
endfunction

function! ale_linters#python#pyflakes#Handle(buffer, lines) abort
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+):(\d+)?:? (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('python', {
\   'name': 'pyflakes',
\   'executable_callback': 'ale_linters#python#pyflakes#GetExecutable',
\   'command_callback': 'ale_linters#python#pyflakes#GetCommand',
\   'callback': 'ale_linters#python#pyflakes#Handle',
\   'output_stream': 'both',
\})
