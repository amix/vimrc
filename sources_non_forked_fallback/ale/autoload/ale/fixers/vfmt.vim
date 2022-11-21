" Author: fiatjaf <fiatjaf@alhur.es>
" Description: Integration of `v fmt` with ALE.

call ale#Set('v_vfmt_options', '')

function! ale#fixers#vfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'v_v_executable')
    let l:options = ale#Var(a:buffer, 'v_vfmt_options')

    return {
    \   'command': ale#Escape(l:executable) . ' fmt' . ale#Pad(l:options)
    \}
endfunction
