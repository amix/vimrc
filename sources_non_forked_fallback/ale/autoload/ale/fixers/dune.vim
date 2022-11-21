" Author: Albert Peschar <albert@peschar.net>
" Description: Fix files with dune format.

call ale#Set('ocaml_dune_executable', 'dune')
call ale#Set('ocaml_dune_options', '')

function! ale#fixers#dune#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ocaml_dune_executable')
    let l:options = ale#Var(a:buffer, 'ocaml_dune_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' format'
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction
