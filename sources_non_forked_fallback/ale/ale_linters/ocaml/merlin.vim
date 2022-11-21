" Author: Andrey Popp -- @andreypopp
" Description: Report errors in OCaml code with Merlin

if !exists('g:merlin')
    finish
endif

function! ale_linters#ocaml#merlin#Handle(buffer, lines) abort
    return merlin#ErrorLocList()
endfunction

call ale#linter#Define('ocaml', {
\   'name': 'merlin',
\   'executable': 'ocamlmerlin',
\   'command': 'true',
\   'callback': 'ale_linters#ocaml#merlin#Handle',
\})
