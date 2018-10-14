" Author: neersighted <bjorn@neersighted.com>
" Description: gofmt for Go files

call ale#linter#Define('go', {
\   'name': 'gofmt',
\   'output_stream': 'stderr',
\   'executable': 'gofmt',
\   'command': 'gofmt -e %t',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
