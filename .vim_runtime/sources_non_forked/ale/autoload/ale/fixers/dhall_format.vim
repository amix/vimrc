" Author: toastal <toastal@protonmail.com>
" Description: Dhall’s built-in formatter
"
function! ale#fixers#dhall_format#Fix(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)
    let l:command = l:executable
    \   . ' format'
    \   . ' --inplace %t'

    return {
    \   'command': l:command,
    \   'read_temporary_file': 1,
    \}
endfunction
