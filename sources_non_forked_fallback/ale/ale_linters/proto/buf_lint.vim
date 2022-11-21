" Author: Alex McKinney <alexmckinney01@gmail.com>
" Description: Run buf lint.

call ale#Set('proto_buf_lint_executable', 'buf')
call ale#Set('proto_buf_lint_config', '')
call ale#Set('proto_buf_lint_options', '')

function! ale_linters#proto#buf_lint#GetCommand(buffer) abort
    let l:config = ale#Var(a:buffer, 'proto_buf_lint_config')
    let l:options = ale#Var(a:buffer, 'proto_buf_lint_options')

    return '%e lint'
    \ . (!empty(l:config) ? ' --config=' . ale#Escape(l:config) : '')
    \ . (!empty(l:options) ? ' ' . l:options : '')
    \ . ' %s#include_package_files=true'
endfunction

call ale#linter#Define('proto', {
\   'name': 'buf_lint',
\   'aliases': ['buf-lint'],
\   'lint_file': 1,
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'proto_buf_lint_executable')},
\   'command': function('ale_linters#proto#buf_lint#GetCommand'),
\   'callback': 'ale#handlers#go#Handler',
\})
