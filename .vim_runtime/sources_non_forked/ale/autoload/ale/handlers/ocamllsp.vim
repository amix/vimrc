" Author: Risto Stevcev <me@risto.codes>
" Description: Handlers for the official OCaml language server

let s:language_id_of_filetype = {
\  'menhir': 'ocaml.menhir',
\  'ocaml': 'ocaml',
\  'ocamlinterface': 'ocaml.interface',
\  'ocamllex': 'ocaml.lex'
\}

function! ale#handlers#ocamllsp#GetExecutable(buffer) abort
    return 'ocamllsp'
endfunction

function! ale#handlers#ocamllsp#GetCommand(buffer) abort
    let l:executable = ale#handlers#ocamllsp#GetExecutable(a:buffer)
    let l:ocaml_ocamllsp_use_opam = ale#Var(a:buffer, 'ocaml_ocamllsp_use_opam')

    return l:ocaml_ocamllsp_use_opam ? 'opam config exec -- ' . l:executable : l:executable
endfunction

function! ale#handlers#ocamllsp#GetLanguage(buffer) abort
    return s:language_id_of_filetype[getbufvar(a:buffer, '&filetype')]
endfunction

function! ale#handlers#ocamllsp#GetProjectRoot(buffer) abort
    let l:dune_project_file = ale#path#FindNearestFile(a:buffer, 'dune-project')

    return !empty(l:dune_project_file) ? fnamemodify(l:dune_project_file, ':h') : ''
endfunction
