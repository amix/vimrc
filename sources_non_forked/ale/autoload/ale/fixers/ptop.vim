" Author: BarrOff https://github.com/BarrOff
" Description: Integration of ptop with ALE.

call ale#Set('pascal_ptop_executable', 'ptop')
call ale#Set('pascal_ptop_options', '')

function! ale#fixers#ptop#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'pascal_ptop_executable')
    let l:options = ale#Var(a:buffer, 'pascal_ptop_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %s %t',
    \   'read_temporary_file': 1,
    \}
endfunction
