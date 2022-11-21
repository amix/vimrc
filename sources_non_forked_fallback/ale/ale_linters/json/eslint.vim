" Author: João Pesce <joao@pesce.cc>
" Description: eslint for JSON files.
"
" Requires eslint-plugin-jsonc or a similar plugin to work
"
" Uses the same funtcions as ale_linters/javascript/eslint.vim by w0rp
" <devw0rp@gmail.com>

call ale#linter#Define('json', {
\   'name': 'eslint',
\   'output_stream': 'both',
\   'executable': function('ale#handlers#eslint#GetExecutable'),
\   'cwd': function('ale#handlers#eslint#GetCwd'),
\   'command': function('ale#handlers#eslint#GetCommand'),
\   'callback': 'ale#handlers#eslint#HandleJSON',
\})
