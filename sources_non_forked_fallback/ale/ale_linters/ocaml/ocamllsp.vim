" Author: Risto Stevcev <me@risto.codes>
" Description: The official language server for OCaml

call ale#Set('ocaml_ocamllsp_use_opam', 1)

call ale#linter#Define('ocaml', {
\   'name': 'ocamllsp',
\   'lsp': 'stdio',
\   'executable': function('ale#handlers#ocamllsp#GetExecutable'),
\   'command': function('ale#handlers#ocamllsp#GetCommand'),
\   'language': function('ale#handlers#ocamllsp#GetLanguage'),
\   'project_root': function('ale#handlers#ocamllsp#GetProjectRoot'),
\})
