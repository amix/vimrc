" Author: aurieh <me@aurieh.me>
" Description: A language server for Python

call ale#Set('python_pyls_executable', 'pyls')
call ale#Set('python_pyls_use_global', 0)

function! ale_linters#python#pyls#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_pyls', ['pyls'])
endfunction

function! ale_linters#python#pyls#GetCommand(buffer) abort
    let l:executable = ale_linters#python#pyls#GetExecutable(a:buffer)

    return ale#Escape(l:executable)
endfunction

function! ale_linters#python#pyls#GetLanguage(buffer) abort
    return 'python'
endfunction

call ale#linter#Define('python', {
\   'name': 'pyls',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#python#pyls#GetExecutable',
\   'command_callback': 'ale_linters#python#pyls#GetCommand',
\   'language_callback': 'ale_linters#python#pyls#GetLanguage',
\   'project_root_callback': 'ale#python#FindProjectRoot',
\})
