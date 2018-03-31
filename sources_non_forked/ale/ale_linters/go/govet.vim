" Author: neersighted <bjorn@neersighted.com>
" Description: go vet for Go files
"
" Author: John Eikenberry <jae@zhar.net>
" Description: updated to work with go1.10

function! ale_linters#go#govet#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer) . ' go vet .'
endfunction

call ale#linter#Define('go', {
\   'name': 'go vet',
\   'output_stream': 'stderr',
\   'executable': 'go',
\   'command_callback': 'ale_linters#go#govet#GetCommand',
\   'callback': 'ale#handlers#go#Handler',
\   'lint_file': 1,
\})
