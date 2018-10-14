" Author: unpairedbracket ben.spiers22@gmail.com
" Description: A language server for fortran

call ale#Set('fortran_language_server_executable', 'fortls')
call ale#Set('fortran_language_server_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#fortran#language_server#GetProjectRoot(buffer) abort
    let l:fortls_file = ale#path#FindNearestFile(a:buffer, '.fortls')

    return !empty(l:fortls_file) ? fnamemodify(l:fortls_file, ':h') : ''
endfunction

call ale#linter#Define('fortran', {
\   'name': 'language_server',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('fortran_language_server_executable'),
\   'command': '%e',
\   'project_root_callback': 'ale_linters#fortran#language_server#GetProjectRoot',
\})
