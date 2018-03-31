" Author: Ahmed El Gabri <@ahmedelgabri>
" Description: Integration of refmt with ALE.

call ale#Set('reasonml_refmt_executable', 'refmt')
call ale#Set('reasonml_refmt_options', '')

function! ale#fixers#refmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'reasonml_refmt_executable')
    let l:options = ale#Var(a:buffer, 'reasonml_refmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --in-place'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
