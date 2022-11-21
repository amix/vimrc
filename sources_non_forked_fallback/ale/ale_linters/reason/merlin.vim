" Author: Andrey Popp -- @andreypopp
" Description: Report errors in ReasonML code with Merlin

if !exists('g:merlin')
    finish
endif

function! ale_linters#reason#merlin#Handle(buffer, lines) abort
    return merlin#ErrorLocList()
endfunction

call ale#linter#Define('reason', {
\   'name': 'merlin',
\   'executable': 'ocamlmerlin',
\   'command': 'true',
\   'callback': 'ale_linters#reason#merlin#Handle',
\})
