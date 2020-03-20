" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('cpp_clangd_executable', 'clangd')
call ale#Set('cpp_clangd_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#cpp#clangd#GetCommand(buffer) abort
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)

    return '%e'
    \    . ale#Pad(ale#Var(a:buffer, 'cpp_clangd_options'))
    \    . (!empty(l:build_dir) ? ' -compile-commands-dir=' . ale#Escape(l:build_dir) : '')
endfunction

call ale#linter#Define('cpp', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'cpp_clangd_executable')},
\   'command': function('ale_linters#cpp#clangd#GetCommand'),
\   'project_root': function('ale#c#FindProjectRoot'),
\})
