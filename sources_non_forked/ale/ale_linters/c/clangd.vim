" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('c_clangd_executable', 'clangd')
call ale#Set('c_clangd_options', '')

function! ale_linters#c#clangd#GetProjectRoot(buffer) abort
    let l:project_root = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')
    return !empty(l:project_root) ? fnamemodify(l:project_root, ':h') : ''
endfunction

function! ale_linters#c#clangd#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'c_clangd_executable')
endfunction

function! ale_linters#c#clangd#GetCommand(buffer) abort
    let l:executable = ale_linters#c#clangd#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'c_clangd_options')

    return ale#Escape(l:executable) . (!empty(l:options) ? ' ' . l:options : '')
endfunction

call ale#linter#Define('c', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable_callback': 'ale_linters#c#clangd#GetExecutable',
\   'command_callback': 'ale_linters#c#clangd#GetCommand',
\   'project_root_callback': 'ale_linters#c#clangd#GetProjectRoot',
\})
