" Author: unpairedbracket ben.spiers22@gmail.com
" Description: A language server for fortran

call ale#Set('fortran_language_server_executable', 'fortls')
call ale#Set('fortran_language_server_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#fortran#language_server#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'fortran_language_server_executable')
endfunction

function! ale_linters#fortran#language_server#GetCommand(buffer) abort
    return ale#Escape(ale_linters#fortran#language_server#GetExecutable(a:buffer))
endfunction

function! ale_linters#fortran#language_server#GetProjectRoot(buffer) abort
    let l:fortls_file = ale#path#FindNearestFile(a:buffer, '.fortls')

    return !empty(l:fortls_file) ? fnamemodify(l:fortls_file, ':h') : ''
endfunction

call ale#linter#Define('fortran', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#fortran#language_server#GetExecutable',
\   'command_callback': 'ale_linters#fortran#language_server#GetCommand',
\   'project_root_callback': 'ale_linters#fortran#language_server#GetProjectRoot',
\})
