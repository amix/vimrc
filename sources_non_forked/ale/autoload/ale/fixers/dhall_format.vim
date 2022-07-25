" Author: toastal <toastal@posteo.net>
" Description: Dhall’s built-in formatter
"
function! ale#fixers#dhall_format#Fix(buffer) abort
    let l:executable = ale#dhall#GetExecutableWithOptions(a:buffer)

    return {
    \   'command': l:executable
    \       . ' format'
    \}
endfunction
