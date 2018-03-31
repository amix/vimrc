" Author: Michael Jungo <michaeljungo92@gmail.com>
" Description: A language server for Reason

call ale#Set('reason_ols_executable', 'ocaml-language-server')
call ale#Set('reason_ols_use_global', 0)

call ale#linter#Define('reason', {
\   'name': 'ols',
\   'lsp': 'stdio',
\   'executable_callback': 'ale#handlers#ols#GetExecutable',
\   'command_callback': 'ale#handlers#ols#GetCommand',
\   'language_callback': 'ale#handlers#ols#GetLanguage',
\   'project_root_callback': 'ale#handlers#ols#GetProjectRoot',
\})
