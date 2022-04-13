" Author: Michael Phillips <michaeljoelphillips@gmail.com>
" Description: Fix Kotlin files with ktlint.

function! ale#fixers#ktlint#Fix(buffer) abort
    return {
    \   'command': ale#handlers#ktlint#GetCommand(a:buffer) . ' --format'
    \}
endfunction
