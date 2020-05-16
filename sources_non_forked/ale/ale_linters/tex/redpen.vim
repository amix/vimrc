" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

call ale#linter#Define('tex', {
\   'name': 'redpen',
\   'executable': 'redpen',
\   'command': 'redpen -f latex -r json %t',
\   'callback': 'ale#handlers#redpen#HandleRedpenOutput',
\})
