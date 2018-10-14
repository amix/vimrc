" Author: Ben Reedy <https://github.com/breed808>
" Description: gosimple for Go files

function! ale_linters#go#gosimple#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer) . ' gosimple .'
endfunction

call ale#linter#Define('go', {
\   'name': 'gosimple',
\   'executable': 'gosimple',
\   'command_callback': 'ale_linters#go#gosimple#GetCommand',
\   'callback': 'ale#handlers#go#Handler',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
