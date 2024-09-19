" Author: Hyuksang Kwon <gwonhyuksang@gmail.com>
" Description: eslint for astro files

call ale#linter#Define('astro', {
\   'name': 'eslint',
\   'output_stream': 'both',
\   'executable': function('ale#handlers#eslint#GetExecutable'),
\   'cwd': function('ale#handlers#eslint#GetCwd'),
\   'command': function('ale#handlers#eslint#GetCommand'),
\   'callback': 'ale#handlers#eslint#HandleJSON',
\})
