" Author: Daniel Lupu <lupu.daniel.f@gmail.com>
" Description: xo for JavaScript files

call ale#linter#Define('javascript', {
\   'name': 'xo',
\   'executable': function('ale#handlers#xo#GetExecutable'),
\   'command': function('ale#handlers#xo#GetLintCommand'),
\   'callback': 'ale#handlers#xo#HandleJSON',
\})
