" Author: neersighted <bjorn@neersighted.com>
" Description: gofmt for Go files

function! ale_linters#go#gofmt#GetCommand(buffer) abort
    return ale#go#EnvString(a:buffer)
    \   . '%e -e %t'
endfunction

call ale#linter#Define('go', {
\   'name': 'gofmt',
\   'output_stream': 'stderr',
\   'executable': 'gofmt',
\   'command': function('ale_linters#go#gofmt#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
