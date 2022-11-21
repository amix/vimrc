" Author: toastal <toastal@posteo.net>
" Description: Dhall’s built-in linter/formatter

function! ale#fixers#dhall_lint#Fix(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)

    return {
    \   'command': l:executable
    \       . ' lint'
    \}
endfunction
