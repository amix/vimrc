" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('cpp_clangd_executable', 'clangd')
call ale#Set('cpp_clangd_options', '')

function! ale_linters#cpp#clangd#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'cpp_clangd_options'))
endfunction

call ale#linter#Define('cpp', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'cpp_clangd_executable')},
\   'command': function('ale_linters#cpp#clangd#GetCommand'),
\   'project_root': function('ale#c#FindProjectRoot'),
\})
