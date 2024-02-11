" Author: Jonathan Palardt https://github.com/jpalardy
" Description: Integration of 'gleam format' with ALE.

call ale#Set('gleam_format_executable', 'gleam')

function! ale#fixers#gleam_format#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'gleam_format_executable')

    return ale#Escape(l:executable)
endfunction

function! ale#fixers#gleam_format#Fix(buffer) abort
    let l:executable = ale#fixers#gleam_format#GetExecutable(a:buffer)

    return {
    \   'command': l:executable . ' format %t',
    \   'read_temporary_file': 1,
    \}
endfunction
