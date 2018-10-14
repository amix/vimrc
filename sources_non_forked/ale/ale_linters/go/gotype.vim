" Author: Jelte Fennema <github-public@jeltef.nl>
" Description: gotype for Go files

function! ale_linters#go#gotype#GetCommand(buffer) abort
    if expand('#' . a:buffer . ':p') =~# '_test\.go$'
        return ''
    endif

    return ale#path#BufferCdString(a:buffer) . ' gotype .'
endfunction

call ale#linter#Define('go', {
\   'name': 'gotype',
\   'output_stream': 'stderr',
\   'executable': 'gotype',
\   'command_callback': 'ale_linters#go#gotype#GetCommand',
\   'callback': 'ale#handlers#go#Handler',
\   'lint_file': 1,
\})
