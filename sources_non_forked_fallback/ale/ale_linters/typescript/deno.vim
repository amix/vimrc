" Author: Mohammed Chelouti - https://github.com/motato1
"         Arnold Chand <creativenull@outlook.com>
" Description: Deno lsp linter for TypeScript files.

call ale#linter#Define('typescript', {
\   'name': 'deno',
\   'lsp': 'stdio',
\   'executable': function('ale#handlers#deno#GetExecutable'),
\   'command': '%e lsp',
\   'project_root': function('ale#handlers#deno#GetProjectRoot'),
\   'initialization_options': function('ale#handlers#deno#GetInitializationOptions'),
\})
