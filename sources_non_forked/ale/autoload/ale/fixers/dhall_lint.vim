" Author: toastal <toastal@protonmail.com>
" Description: Dhall’s built-in linter/formatter

function! ale#fixers#dhall_lint#Fix(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)
    let l:command = l:executable
    \   . ' lint'
    \   . ' --inplace %t'

    return {
    \   'command': l:command,
    \   'read_temporary_file': 1,
    \}
endfunction
