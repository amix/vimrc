" Author: Victor Ananyev <vindex10@gmail.com>
" Description: eslint for js snippets in HTML files


call ale#linter#Define('html', {
\   'name': 'eslint',
\   'output_stream': 'both',
\   'executable': function('ale#handlers#eslint#GetExecutable'),
\   'cwd': function('ale#handlers#eslint#GetCwd'),
\   'command': function('ale#handlers#eslint#GetCommand'),
\   'callback': 'ale#handlers#eslint#HandleJSON',
\ })
