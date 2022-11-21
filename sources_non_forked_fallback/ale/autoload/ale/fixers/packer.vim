" Author: Zhuoyun Wei <wzyboy@wzyboy.org>
" Description: Fixer for Packer HCL files

call ale#Set('packer_fmt_executable', 'packer')
call ale#Set('packer_fmt_options', '')

function! ale#fixers#packer#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'packer_fmt_executable')
    let l:options = ale#Var(a:buffer, 'packer_fmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' fmt'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' -'
    \}
endfunction
