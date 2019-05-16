" Author: harttle <yangjvn@126.com>
" Description: fecs for JavaScript files

call ale#linter#Define('javascript', {
\   'name': 'fecs',
\   'executable': function('ale#handlers#fecs#GetExecutable'),
\   'command': function('ale#handlers#fecs#GetCommand'),
\   'read_buffer': 0,
\   'callback': 'ale#handlers#fecs#Handle',
\})
