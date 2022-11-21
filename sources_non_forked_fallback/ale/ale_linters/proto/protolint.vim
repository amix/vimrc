" Author: Yohei Yoshimuta <yoheimuta@gmail.com>
" Description: run the protolint for Protocol Buffer files

call ale#Set('proto_protolint_executable', 'protolint')
call ale#Set('proto_protolint_config', '')

function! ale_linters#proto#protolint#GetCommand(buffer) abort
    let l:config = ale#Var(a:buffer, 'proto_protolint_config')

    return '%e lint'
    \ . (!empty(l:config) ? ' -config_path=' . ale#Escape(l:config) : '')
    \ . ' -reporter=unix'
    \ . ' %s'
endfunction

call ale#linter#Define('proto', {
\   'name': 'protolint',
\   'lint_file': 1,
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'proto_protolint_executable')},
\   'command': function('ale_linters#proto#protolint#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsError',
\})

