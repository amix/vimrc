" Author: neersighted <bjorn@neersighted.com>
" Description: golint for Go files

call ale#linter#Define('go', {
\   'name': 'golint',
\   'output_stream': 'both',
\   'executable': 'golint',
\   'command': 'golint %t',
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
