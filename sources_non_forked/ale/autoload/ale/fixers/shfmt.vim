scriptencoding utf-8
" Author: Simon Bugert <simon.bugert@gmail.com>
" Description: Fix sh files with shfmt.

call ale#Set('sh_shfmt_executable', 'shfmt')
call ale#Set('sh_shfmt_options', '')

function! ale#fixers#shfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'sh_shfmt_executable')
    let l:options = ale#Var(a:buffer, 'sh_shfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -filename=%s'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction
