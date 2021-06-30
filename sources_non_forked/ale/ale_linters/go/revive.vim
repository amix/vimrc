" Author: Penghui Liao <liaoishere@gmail.com>
" Description: Adds support for revive

call ale#Set('go_revive_executable', 'revive')
call ale#Set('go_revive_options', '')

function! ale_linters#go#revive#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'go_revive_options')

    return ale#go#EnvString(a:buffer) . '%e'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

call ale#linter#Define('go', {
\   'name': 'revive',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'go_revive_executable')},
\   'command': function('ale_linters#go#revive#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
