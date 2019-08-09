" Author: Kanenobu Mitsuru
" Description: Integration of ocp-indent with ALE.

call ale#Set('ocaml_ocp_indent_executable', 'ocp-indent')
call ale#Set('ocaml_ocp_indent_options', '')
call ale#Set('ocaml_ocp_indent_config', '')

function! ale#fixers#ocp_indent#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ocaml_ocp_indent_executable')
    let l:config = ale#Var(a:buffer, 'ocaml_ocp_indent_config')
    let l:options = ale#Var(a:buffer, 'ocaml_ocp_indent_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:config) ? '' : ' --config=' . ale#Escape(l:config))
    \       . (empty(l:options) ? '': ' ' . l:options)
    \}
endfunction
