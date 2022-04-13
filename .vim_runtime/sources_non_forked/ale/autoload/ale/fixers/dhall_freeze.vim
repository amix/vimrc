" Author: toastal <toastal@protonmail.com>
" Description: Dhall’s package freezing

call ale#Set('dhall_freeze_options', '')

function! ale#fixers#dhall_freeze#Freeze(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)
    let l:command = l:executable
    \   . ' freeze'
    \   . ale#Pad(ale#Var(a:buffer, 'dhall_freeze_options'))
    \   . ' --inplace %t'


    return {
    \   'command': l:command,
    \   'read_temporary_file': 1,
    \}
endfunction
