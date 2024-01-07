" Author: Yining <zhang.yining@gmail.com>
" Description: nickel format as ALE fixer for Nickel files

call ale#Set('nickel_nickel_format_executable', 'nickel')
call ale#Set('nickel_nickel_format_options', '')

function! ale#fixers#nickel_format#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'nickel_nickel_format_executable')
    let l:options = ale#Var(a:buffer, 'nickel_nickel_format_options')

    return {
    \   'command': ale#Escape(l:executable) . ' format'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction

