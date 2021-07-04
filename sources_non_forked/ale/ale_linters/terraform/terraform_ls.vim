" Author: Horacio Sanson <hsanson@gmail.com>
" Description: terraform-ls integration for ALE (cf. https://github.com/hashicorp/terraform-ls)

call ale#Set('terraform_terraform_executable', 'terraform')
call ale#Set('terraform_ls_executable', 'terraform-ls')
call ale#Set('terraform_ls_options', '')

function! ale_linters#terraform#terraform_ls#GetTerraformExecutable(buffer) abort
    let l:terraform_executable = ale#Var(a:buffer, 'terraform_terraform_executable')

    if(ale#path#IsAbsolute(l:terraform_executable))
        return '-tf-exec ' . l:terraform_executable
    endif

    return ''
endfunction

function! ale_linters#terraform#terraform_ls#GetCommand(buffer) abort
    return '%e'
    \ . ale#Pad('serve')
    \ . ale#Pad(ale_linters#terraform#terraform_ls#GetTerraformExecutable(a:buffer))
    \ . ale#Pad(ale#Var(a:buffer, 'terraform_ls_options'))
endfunction

function! ale_linters#terraform#terraform_ls#GetProjectRoot(buffer) abort
    let l:tf_dir = ale#path#FindNearestDirectory(a:buffer, '.terraform')

    return !empty(l:tf_dir) ? fnamemodify(l:tf_dir, ':h:h') : ''
endfunction

call ale#linter#Define('terraform', {
\   'name': 'terraform_ls',
\   'lsp': 'stdio',
\   'executable': {b -> ale#Var(b, 'terraform_ls_executable')},
\   'command': function('ale_linters#terraform#terraform_ls#GetCommand'),
\   'project_root': function('ale_linters#terraform#terraform_ls#GetProjectRoot'),
\   'language': 'terraform',
\})
