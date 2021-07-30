" Author: ghsang <gwonhyuksang@gmail.com>
" Description: Integration of dart format with ALE.

call ale#Set('dart_format_executable', 'dart')
call ale#Set('dart_format_options', '')

function! ale#fixers#dart_format#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'dart_format_executable')
    let l:options = ale#Var(a:buffer, 'dart_format_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' format'
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
