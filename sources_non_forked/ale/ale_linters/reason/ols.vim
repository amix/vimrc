" Author: Michael Jungo <michaeljungo92@gmail.com>
" Description: A language server for Reason

call ale#Set('reason_ols_executable', 'ocaml-language-server')
call ale#Set('reason_ols_use_global', get(g:, 'ale_use_global_executables', 0))

call ale#linter#Define('reason', {
\   'name': 'ols',
\   'lsp': 'stdio',
\   'executable': function('ale#handlers#ols#GetExecutable'),
\   'command': function('ale#handlers#ols#GetCommand'),
\   'language_callback': 'ale#handlers#ols#GetLanguage',
\   'project_root': function('ale#handlers#ols#GetProjectRoot'),
\})
