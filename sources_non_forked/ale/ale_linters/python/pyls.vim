" Author: aurieh <me@aurieh.me>
" Description: A language server for Python

call ale#Set('python_pyls_executable', 'pyls')
call ale#Set('python_pyls_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#python#pyls#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_pyls', ['pyls'])
endfunction

function! ale_linters#python#pyls#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyls#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run pyls'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
endfunction

call ale#linter#Define('python', {
\   'name': 'pyls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#python#pyls#GetExecutable',
\   'command_callback': 'ale_linters#python#pyls#GetCommand',
\   'language': 'python',
\   'project_root_callback': 'ale#python#FindProjectRoot',
\   'completion_filter': 'ale#completion#python#CompletionItemFilter',
\})
