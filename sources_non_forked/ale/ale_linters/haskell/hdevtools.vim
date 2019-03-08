" Author: rob-b, Takano Akio <tak@anoak.io>
" Description: hdevtools for Haskell files

call ale#Set('haskell_hdevtools_executable', 'hdevtools')
call ale#Set('haskell_hdevtools_options', get(g:, 'hdevtools_options', '-g -Wall'))

function! ale_linters#haskell#hdevtools#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'haskell_hdevtools_executable')

    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'hdevtools')
    \ . ' check' . ale#Pad(ale#Var(a:buffer, 'haskell_hdevtools_options'))
    \ . ' -p %s %t'
endfunction

call ale#linter#Define('haskell', {
\   'name': 'hdevtools',
\   'executable': {b -> ale#Var(b, 'haskell_hdevtools_executable')},
\   'command': function('ale_linters#haskell#hdevtools#GetCommand'),
\   'callback': 'ale#handlers#haskell#HandleGHCFormat',
\})
