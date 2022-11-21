" Author: toastal <toastal@posteo.net>
" Description: Dhall’s package freezing

call ale#Set('dhall_freeze_options', '')

function! ale#fixers#dhall_freeze#Freeze(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)

    return {
    \   'command': l:executable
    \       . ' freeze'
    \       . ale#Pad(ale#Var(a:buffer, 'dhall_freeze_options'))
    \}
endfunction
