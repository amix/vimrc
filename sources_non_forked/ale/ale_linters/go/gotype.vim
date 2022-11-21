" Author: Jelte Fennema <github-public@jeltef.nl>
" Description: gotype for Go files

function! ale_linters#go#gotype#GetExecutable(buffer) abort
    if expand('#' . a:buffer . ':p') =~# '_test\.go$'
        return ''
    endif

    return 'gotype'
endfunction

function! ale_linters#go#gotype#GetCommand(buffer) abort
    return ale#go#EnvString(a:buffer) . 'gotype -e .'
endfunction

call ale#linter#Define('go', {
\   'name': 'gotype',
\   'output_stream': 'stderr',
\   'executable': function('ale_linters#go#gotype#GetExecutable'),
\   'cwd': '%s:h',
\   'command': function('ale_linters#go#gotype#GetCommand'),
\   'callback': 'ale#handlers#go#Handler',
\   'lint_file': 1,
\})
