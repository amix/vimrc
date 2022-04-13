scriptencoding utf-8
" Author: Simon Bugert <simon.bugert@gmail.com>
" Description: Fix sh files with shfmt.

call ale#Set('sh_shfmt_executable', 'shfmt')
call ale#Set('sh_shfmt_options', '')

function! s:DefaultOption(buffer) abort
    if getbufvar(a:buffer, '&expandtab') == 0
        " Tab is used by default
        return ''
    endif

    let l:tabsize = getbufvar(a:buffer, '&shiftwidth')

    if l:tabsize == 0
        let l:tabsize = getbufvar(a:buffer, '&tabstop')
    endif

    return ' -i ' . l:tabsize
endfunction

function! ale#fixers#shfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'sh_shfmt_executable')
    let l:options = ale#Var(a:buffer, 'sh_shfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? s:DefaultOption(a:buffer) : ' ' . l:options)
    \}
endfunction
