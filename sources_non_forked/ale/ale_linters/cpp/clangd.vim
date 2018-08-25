" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('cpp_clangd_executable', 'clangd')
call ale#Set('cpp_clangd_options', '')

function! ale_linters#cpp#clangd#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')
    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : ''
endfunction

function! ale_linters#cpp#clangd#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'cpp_clangd_options'))
endfunction

call ale#linter#Define('cpp', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable_callback': ale#VarFunc('cpp_clangd_executable'),
\   'command_callback': 'ale_linters#cpp#clangd#GetCommand',
\   'project_root_callback': 'ale_linters#cpp#clangd#GetProjectRoot',
\})
