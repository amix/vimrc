" Author: neersighted <bjorn@neersighted.com>
" Description: golint for Go files

call ale#Set('go_golint_executable', 'golint')
call ale#Set('go_golint_options', '')

function! ale_linters#go#golint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'go_golint_options')

    return ale#go#EnvString(a:buffer) . '%e'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

call ale#linter#Define('go', {
\   'name': 'golint',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'go_golint_executable')},
\   'command': function('ale_linters#go#golint#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsWarning',
\})
