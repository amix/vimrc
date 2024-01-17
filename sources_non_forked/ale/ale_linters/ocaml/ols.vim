" Author: Michael Jungo <michaeljungo92@gmail.com>
" Description: A language server for OCaml

call ale#Set('ocaml_ols_executable', 'ocaml-language-server')
call ale#Set('ocaml_ols_use_global', get(g:, 'ale_use_global_executables', 0))

call ale#linter#Define('ocaml', {
\   'name': 'ols',
\   'aliases': ['ocaml-language-server'],
\   'lsp': 'stdio',
\   'executable': function('ale#handlers#ols#GetExecutable'),
\   'command': function('ale#handlers#ols#GetCommand'),
\   'language': function('ale#handlers#ols#GetLanguage'),
\   'project_root': function('ale#handlers#ols#GetProjectRoot'),
\})
