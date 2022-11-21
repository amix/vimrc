" Author: Tommy Chiang <ty1208chiang@gmail.com>
" Description: Clangd language server for CUDA (modified from Andrey
" Melentyev's implementation for C++)

call ale#Set('cuda_clangd_executable', 'clangd')
call ale#Set('cuda_clangd_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#cuda#clangd#GetCommand(buffer) abort
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)

    return '%e'
    \    . ale#Pad(ale#Var(a:buffer, 'cuda_clangd_options'))
    \    . (!empty(l:build_dir) ? ' -compile-commands-dir=' . ale#Escape(l:build_dir) : '')
endfunction

call ale#linter#Define('cuda', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'cuda_clangd_executable')},
\   'command': function('ale_linters#cuda#clangd#GetCommand'),
\   'project_root': function('ale#c#FindProjectRoot'),
\})
