" Author: w0rp <devw0rp@gmail.com>
" Description: pyflakes for python files

call ale#Set('python_pyflakes_executable', 'pyflakes')
call ale#Set('python_pyflakes_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_pyflakes_auto_pipenv', 0)
call ale#Set('python_pyflakes_auto_poetry', 0)

function! ale_linters#python#pyflakes#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_pyflakes_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    if (ale#Var(a:buffer, 'python_auto_poetry') || ale#Var(a:buffer, 'python_pyflakes_auto_poetry'))
    \ && ale#python#PoetryPresent(a:buffer)
        return 'poetry'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_pyflakes', ['pyflakes'])
endfunction

function! ale_linters#python#pyflakes#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyflakes#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv\|poetry$'
    \   ? ' run pyflakes'
    \   : ''

    return ale#Escape(l:executable)
    \   . l:exec_args
    \   . ' %t'
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
\   'executable': function('ale_linters#python#pyflakes#GetExecutable'),
\   'command': function('ale_linters#python#pyflakes#GetCommand'),
\   'callback': 'ale_linters#python#pyflakes#Handle',
\   'output_stream': 'both',
\})
