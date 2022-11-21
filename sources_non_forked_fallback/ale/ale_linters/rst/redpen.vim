" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

call ale#linter#Define('rst', {
\   'name': 'redpen',
\   'executable': 'redpen',
\   'command': 'redpen -f rest -r json %t',
\   'callback': 'ale#handlers#redpen#HandleRedpenOutput',
\})
