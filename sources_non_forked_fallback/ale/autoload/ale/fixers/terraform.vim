" Author: dsifford <dereksifford@gmail.com>
" Description: Fixer for terraform and .hcl files

call ale#Set('terraform_fmt_executable', 'terraform')
call ale#Set('terraform_fmt_options', '')

function! ale#fixers#terraform#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'terraform_fmt_executable')
    let l:options = ale#Var(a:buffer, 'terraform_fmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' fmt'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' -'
    \}
endfunction
