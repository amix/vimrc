" Author: theoldmoon0602
" Description: Integration of dfmt with ALE.

call ale#Set('d_dfmt_executable', 'dfmt')
call ale#Set('d_dfmt_options', '')

function! ale#fixers#dfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'd_dfmt_executable')
    let l:options = ale#Var(a:buffer, 'd_dfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -i'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
