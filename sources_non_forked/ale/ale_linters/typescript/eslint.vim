" Author: w0rp <devw0rp@gmail.com>
" Description: eslint for JavaScript files

call ale#linter#Define('typescript', {
\   'name': 'eslint',
\   'executable': function('ale#handlers#eslint#GetExecutable'),
\   'command': function('ale#handlers#eslint#GetCommand'),
\   'callback': 'ale#handlers#eslint#Handle',
\})
