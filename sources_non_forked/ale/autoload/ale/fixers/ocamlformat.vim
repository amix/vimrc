" Author: Stephen Lumenta <@sbl>
" Description: Integration of ocamlformat with ALE.

call ale#Set('ocaml_ocamlformat_executable', 'ocamlformat')
call ale#Set('ocaml_ocamlformat_options', '')

function! ale#fixers#ocamlformat#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ocaml_ocamlformat_executable')
    let l:options = ale#Var(a:buffer, 'ocaml_ocamlformat_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' --name=%s'
    \       . ' -'
    \}
endfunction
