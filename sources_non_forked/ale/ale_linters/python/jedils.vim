" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: https://github.com/pappasam/jedi-language-server

call ale#Set('python_jedils_executable', 'jedi-language-server')
call ale#Set('python_jedils_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_jedils_auto_pipenv', 0)

function! ale_linters#python#jedils#GetExecutable(buffer) abort
    if (ale#Var(a:buffer, 'python_auto_pipenv') || ale#Var(a:buffer, 'python_jedils_auto_pipenv'))
    \ && ale#python#PipenvPresent(a:buffer)
        return 'pipenv'
    endif

    return ale#python#FindExecutable(a:buffer, 'python_jedils', ['jedi-language-server'])
endfunction

function! ale_linters#python#jedils#GetCommand(buffer) abort
    let l:executable = ale_linters#python#jedils#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run jedi-language-server'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
endfunction

call ale#linter#Define('python', {
\   'name': 'jedils',
\   'lsp': 'stdio',
\   'executable': function('ale_linters#python#jedils#GetExecutable'),
\   'command': function('ale_linters#python#jedils#GetCommand'),
\   'project_root': function('ale#python#FindProjectRoot'),
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\})
