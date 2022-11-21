" Author: rhysd https://rhysd.github.io
" Description: Redpen, a proofreading tool (http://redpen.cc)

call ale#linter#Define('asciidoc', {
\   'name': 'redpen',
\   'executable': 'redpen',
\   'command': 'redpen -f asciidoc -r json %t',
\   'callback': 'ale#handlers#redpen#HandleRedpenOutput',
\})
