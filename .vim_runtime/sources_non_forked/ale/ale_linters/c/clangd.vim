" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('c_clangd_executable', 'clangd')
call ale#Set('c_clangd_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#c#clangd#GetCommand(buffer) abort
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)

    return '%e'
    \    . ale#Pad(ale#Var(a:buffer, 'c_clangd_options'))
    \    . (!empty(l:build_dir) ? ' -compile-commands-dir=' . ale#Escape(l:build_dir) : '')
endfunction

call ale#linter#Define('c', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'c_clangd_executable')},
\   'command': function('ale_linters#c#clangd#GetCommand'),
\   'project_root': function('ale#c#FindProjectRoot'),
\})
