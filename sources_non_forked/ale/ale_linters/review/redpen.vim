" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

call ale#linter#Define('review', {
\   'name': 'redpen',
\   'executable': 'redpen',
\   'command': 'redpen -f review -r json %t',
\   'callback': 'ale#handlers#redpen#HandleRedpenOutput',
\})
