" Author: tim <tim@inept.tech>
" Description: Fix files with gnatpp.

call ale#Set('ada_gnatpp_executable', 'gnatpp')
call ale#Set('ada_gnatpp_options', '')

function! ale#fixers#gnatpp#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ada_gnatpp_executable')
    let l:options = ale#Var(a:buffer, 'ada_gnatpp_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
