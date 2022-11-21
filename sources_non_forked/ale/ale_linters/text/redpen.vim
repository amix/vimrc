" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

call ale#linter#Define('text', {
\   'name': 'redpen',
\   'executable': 'redpen',
\   'command': 'redpen -f plain -r json %t',
\   'callback': 'ale#handlers#redpen#HandleRedpenOutput',
\})
