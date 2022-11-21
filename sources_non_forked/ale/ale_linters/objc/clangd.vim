" Author: Andrey Melentyev <andrey.melentyev@protonmail.com>
" Description: Clangd language server

call ale#Set('objc_clangd_executable', 'clangd')
call ale#Set('objc_clangd_options', '')

function! ale_linters#objc#clangd#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'objc_clangd_options'))
endfunction

call ale#linter#Define('objc', {
\   'name': 'clangd',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'objc_clangd_executable')},
\   'command': function('ale_linters#objc#clangd#GetCommand'),
\   'project_root': function('ale#c#FindProjectRoot'),
\})
