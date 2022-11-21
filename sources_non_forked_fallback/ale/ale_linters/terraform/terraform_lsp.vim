" Author: OJFord <dev@ojford.com>
" Description: terraform-lsp integration for ALE (cf. https://github.com/juliosueiras/terraform-lsp)

call ale#Set('terraform_langserver_executable', 'terraform-lsp')
call ale#Set('terraform_langserver_options', '')

function! ale_linters#terraform#terraform_lsp#GetCommand(buffer) abort
    return '%e'
    \   . ale#Pad(ale#Var(a:buffer, 'terraform_langserver_options'))
endfunction

function! ale_linters#terraform#terraform_lsp#GetProjectRoot(buffer) abort
    let l:tf_dir = ale#path#FindNearestDirectory(a:buffer, '.terraform')

    return !empty(l:tf_dir) ? fnamemodify(l:tf_dir, ':h:h') : ''
endfunction

call ale#linter#Define('terraform', {
\   'name': 'terraform_lsp',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'terraform_langserver_executable')},
\   'command': function('ale_linters#terraform#terraform_lsp#GetCommand'),
\   'project_root': function('ale_linters#terraform#terraform_lsp#GetProjectRoot'),
\   'language': 'terraform',
\})
